%% Copyright (c) 2012 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : luerl_comp_vars.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does variable and stack analysis in the compiler

-module(luerl_comp_vars).

-include("luerl.hrl").

-include("luerl_comp.hrl").

-export([chunk/2]).

-import(ordsets, [add_element/2,is_element/2,union/1,union/2,
		  subtract/2,intersection/2]).

chunk(#chunk{code=C0}=St0, Opts) ->
    {C1,_,St1} = exp(C0, [], St0),
    debug_print(Opts, "cv: ~p\n", [C1]),
    {ok,St1#chunk{code=C1}}.

debug_print(Opts, Format, Args) ->
    case lists:member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

%% stat(Stats, LocalVars, FreeVars, UsedVars, State) ->
%%     {Stats,NewLocalVars,NewFreeVars,NewUsedVars,State}.
%%  Main problem here is to calculate local/free/used variables in the
%%  right order. Must do everything going forwards.

stats([S0|Ss0], Local0, Free0, Used0, St0) ->
    case stat(S0, Local0, St0) of		%Export S1,Snew,Sfree,Sused,St1
	{S1,Snew,Sfree,St1} ->
	    Sused = intersection(Sfree, Local0);	%Add Sused
	{S1,Snew,Sfree,Sused,St1} -> ok
    end,
    Local1 = union(Snew, Local0),
    Used1 = union(Sused, Used0),
    Free1 = union(subtract(Sfree, Sused), Free0),
    %% io:format("ss1: ~p\n", [{Local0,Free0,Used0}]),
    %% io:format("ss1: ~p\n", [{Snew,Sfree,Sused}]),
    %% io:format("ss1: ~p\n", [{Local1,Free1,Used1}]),
    {Ss1,Local2,Free2,Used2,St2} = stats(Ss0, Local1, Free1, Used1, St1),
    {[S1|Ss1],Local2,Free2,Used2,St2};
stats([], Local, Free, Used, St) -> {[],Local,Free,Used,St}.

%% stat(Stat, LocalVars, State) -> {Stat,NewVars,FreeVars,State}.

stat(#assign{}=A, Loc, St) ->
    assign_stat(A, Loc, St);
stat(#return{}=R, Loc, St) ->
    return_stat(R, Loc, St);
stat(#break{}=B, _, St) -> {B,[],[],St};
stat(#block{}=B, _, St) ->			%Sub-block
    block_stat(B, St);
stat(#while{}=W, Loc, St) ->
    while_stat(W, Loc, St);
stat(#repeat{}=R, Loc, St) ->
    repeat_stat(R, Loc, St);
stat(#'if'{}=If, Loc, St) ->
    if_stat(If, Loc, St);
stat(#nfor{}=For, Loc, St) ->
    numfor_stat(For, Loc, St);
stat(#gfor{}=For, Loc, St) ->
    genfor_stat(For, Loc, St);
stat(#local_assign{}=L, Loc, St) ->
    local_assign_stat(L, Loc, St);
stat(#local_fdef{}=L, Loc, St) ->
    local_fdef_stat(L, Loc, St);
stat(Exp0, Loc, St0) ->
    {Exp1,Efree,St1} = exp(Exp0, Loc, St0),
    {Exp1,[],Efree,St1}.

%% assign_stat(Assign, LocalVars, State) -> {Assign,NewVars,FreeVars,State}.

assign_stat(#assign{vs=Vs0,es=Es0}=A, Loc, St0) ->
    {Vs1,Vfree,St1} = assign_loop(Vs0, Loc, St0),
    {Es1,Efree,St2} = explist(Es0, Loc, St1),
    Free = union(Vfree, Efree),
    {A#assign{vs=Vs1,es=Es1},[],Free,St2}.

assign_loop([V0|Vs0], Loc, St0) ->
    {V1,Vfree,St1} = var(V0, Loc, St0),
    {Vs1,Vsfree,St2} = assign_loop(Vs0, Loc, St1),
    {[V1|Vs1],union(Vfree, Vsfree),St2};
assign_loop([], _, St) -> {[],[],St}.

var(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Efree,St1} = prefixexp_first(Exp0, Loc, St0),
    {Rest1,Rfree,St2} = var_rest(Rest0, Loc, St1),
    {D#dot{e=Exp1,r=Rest1},union(Efree, Rfree),St2};
var(#var{n=N}=V, Loc, St) ->
    Free = maybe_free(N, Loc, []),
    {V,Free,St}.

var_rest(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Efree,St1} = prefixexp_element(Exp0, Loc, St0),
    {Rest1,Rfree,St2} = var_rest(Rest0, Loc, St1),
    {D#dot{e=Exp1,r=Rest1},union(Efree, Rfree),St2};
var_rest(Exp, Loc, St) -> var_last(Exp, Loc, St).

var_last(#key{k=Exp0}=K, Loc, St0) ->
    {Exp1,Efree,St1} = exp(Exp0, Loc, St0),
    {K#key{k=Exp1},Efree,St1}.

%% return_stat(Return, LocalVars, State) -> {Return,NewVars,FreeVars,State}.

return_stat(#return{es=Es0}=R, Loc, St0) ->
    {Es1,Esfree,St1} = explist(Es0, Loc, St0),
    {R#return{es=Es1},[],Esfree,St1}.

%% block_stat(Block, State) -> {Block,NewVars,FreeVars,State}.

block_stat(B0, St0) ->
    {B1,Bfree,St1} = do_block(B0, St0),
    {B1,[],Bfree,St1}.

%% do_block(Block, State) -> {Block,FreeVars,State}.
%% do_block(Block, LocalVars, State) -> {Block,FreeVars,State}.
%%  Do_block never returns external new variables as it never exports
%%  variables.

do_block(B, St) -> do_block(B, [], St).

do_block(#block{ss=Ss0}=B, Loc0, St0) ->
    {Ss1,Loc1,Bfree,Bused,St1} = stats(Ss0, Loc0, [], [], St0),
    {B#block{ss=Ss1,local=Loc1,free=Bfree,used=Bused},Bfree,St1}.

%% while_stat(While, LocalVars, State) -> {While,NewVars,FreeVars,State}.
%%  While_stat never returns external new variables. Fits into stat().
%%  The test expression is done in the context of the surrounding
%%  block.

while_stat(#while{e=E0,b=B0}=W, Loc, St0) ->
    {E1,Efree,St1} = exp(E0, Loc, St0),
    {B1,Bfree,St2} = do_block(B0, St1),
    Free = union(Efree, Bfree),
    {W#while{e=E1,b=B1},[],Free,St2}.

%% repeat_stat(Repeat, LocalVars, State) -> {Repeat,NewVars,FreeVars,State}.
%%  Repeat_stat never returns external new variables. Fits into
%%  stat(). The test expression is done in the context of the repeat
%%  block.

repeat_stat(#repeat{b=B0,e=E0}=R, _, St0) ->
    {B1,E1,Bfree,St1} = repeat_block(B0, E0, St0),
    {R#repeat{b=B1,e=E1},[],Bfree,St1}.

repeat_block(#block{ss=B0}=B, E0, St0) ->
    {B1,Bloc,Bfree,Bused,St1} = stats(B0, [], [], [], St0),
    %% Now do the test in the context of the block.
    {E1,Efree,St2} = exp(E0, Bloc, St1),
    Eused = intersection(Efree, Bloc),
    Free = union(subtract(Efree, Eused), Bfree),
    Used = union(Eused, Bused),
    {B#block{ss=B1,local=Bloc,free=Free,used=Used},E1,Free,St2}.

%% if_stat(If, LocalVars, State) -> {If,NewVars,FreeVars,State}.
%%  The block info includes anything from the test expressions even
%%  though we keep them separate.

if_stat(#'if'{tests=Ts0,else=E0}=If, Loc, St0) ->
    {Ts1,Tfree,St1} = if_tests(Ts0, Loc, St0),
    {E1,Efree,St2} = do_block(E0, St1),
    Free = union(Efree, Tfree),
    {If#'if'{tests=Ts1,else=E1},[],Free,St2}.

if_tests([{E0,B0}|Ts0], Loc, St0) ->
    {E1,Efree,St1} = exp(E0, Loc, St0),
    {B1,Bfree,St2} = do_block(B0, St1),
    {Ts1,Tsfree,St3} = if_tests(Ts0, Loc, St2),
    Free = union([Efree,Bfree,Tsfree]),
    {[{E1,B1}|Ts1],Free,St3};
if_tests([], _, St) -> {[],[],St}.

%% numfor_stat(For, LocalVars, State) -> {For,NewVars,FreeVars,State}.

numfor_stat(#nfor{v=#var{n=N},init=I0,limit=L0,step=S0,b=B0}=For, Loc, St0) ->
    {[I1,L1,S1],Esfree,St1} = explist([I0,L0,S0], Loc, St0),
    {B1,Bfree,St2} = do_block(B0, [N], St1),
    Free = union(Esfree, Bfree),
    {For#nfor{init=I1,limit=L1,step=S1,b=B1},[],Free,St2}.

%% genfor_stat(For, LocalVars, State) -> {For,NewVars,FreeVars,State}.

genfor_stat(#gfor{vs=Vs,gens=Gs0,b=B0}=For, Loc, St0) ->
    {Gs1,Gfree,St1} = explist(Gs0, Loc, St0),
    Ns = lists:foldl(fun (#var{n=N}, Ns) -> add_element(N, Ns) end, [], Vs),
    {B1,Bfree,St2} = do_block(B0, Ns, St1),
    Free = union(Gfree, Bfree),
    {For#gfor{gens=Gs1,b=B1},[],Free,St2}.

%% local_assign_stat(Local, LocalVars, State) -> {Local,NewVars,FreeVars,State}.

local_assign_stat(#local_assign{vs=Vs,es=Es0}=L, Loc, St0) ->
    {Es1,Esfree,St1} = explist(Es0, Loc, St0),
    Ns = lists:foldl(fun (#var{n=N}, Ns) -> add_element(N, Ns) end, [], Vs),
    New = subtract(Ns, Loc),
    {L#local_assign{es=Es1},New,Esfree,St1}.

%% local_fdef_stat(Local, LocalVars, State) -> {Local,NewVars,FreeVars,State}.

local_fdef_stat(#local_fdef{v=#var{n=N},f=F0}=L, Loc, St0) ->
    {F1,Ffree,St1} = functiondef(F0, null, St0),
    New = [N],
    Free = Ffree,
    Used = intersection(Free, union(Loc, New)),
    %% io:fwrite("lfs: ~p\n", [{Ffree,New,Free,Used}]),
    {L#local_fdef{f=F1},New,Free,Used,St1}.

%% explist(Exprs, LocalVars, State) -> {Exprs,FreeVars,State}.
%% exp(Expr, LocalVars, State) -> {Expr,FreeVars,State}.
%%  An expression can never create new local variables.

explist([E0|Es0], Loc, St0) ->
    {E1,Efree,St1} = exp(E0, Loc, St0),
    {Es1,Esfree,St2} = explist(Es0, Loc, St1),
    {[E1|Es1],union(Efree, Esfree),St2};
explist([], _, St) -> {[],[],St}.		%No expressions at all

exp(#lit{}=L, _, St) -> {L,[],St};		%Nothing to do
exp(#fdef{}=F, Loc, St) -> functiondef(F, Loc, St);
exp(#op{as=Es0}=Op, Loc, St0) ->
    {Es1,Efree,St1} = explist(Es0, Loc, St0),
    {Op#op{as=Es1},Efree,St1};
exp(#tc{fs=Fs0}=T, Loc, St0) ->
    {Fs1,Tfree,St1} = tableconstructor(Fs0, Loc, St0),
    {T#tc{fs=Fs1},Tfree,St1};
exp(E, Loc, St) ->
    prefixexp(E, Loc, St).

prefixexp(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Efree,St1} = prefixexp_first(Exp0, Loc, St0),
    {Rest1,Rfree,St2} = prefixexp_rest(Rest0, Loc, St1),
    Free = union(Efree, Rfree),
    {D#dot{e=Exp1,r=Rest1},Free,St2};
prefixexp(Exp, Loc, St) -> prefixexp_first(Exp, Loc, St).

prefixexp_first(#single{e=E0}=S, Loc, St0) ->
    {E1,Efree,St1} = exp(E0, Loc, St0),
    {S#single{e=E1},Efree,St1};
prefixexp_first(#var{n=N}=V, Loc, St) ->
    Free = maybe_free(N, Loc, []),
    {V,Free,St}.

prefixexp_rest(#dot{e=Exp0,r=Rest0}=D, Loc, St0) ->
    {Exp1,Efree,St1} = prefixexp_element(Exp0, Loc, St0),
    {Rest1,Rfree,St2} = prefixexp_rest(Rest0, Loc, St1),
    Free = union(Efree, Rfree),
    {D#dot{e=Exp1,r=Rest1},Free,St2};
prefixexp_rest(Exp, Loc, St) -> prefixexp_element(Exp, Loc, St).

prefixexp_element(#key{k=E0}=K, Loc, St0) ->
    {E1,Efree,St1} = exp(E0, Loc, St0),
    {K#key{k=E1},Efree,St1};
prefixexp_element(#fcall{as=As0}=F, Loc, St0) ->
    {As1,Asfree,St1} = explist(As0, Loc, St0),
    {F#fcall{as=As1},Asfree,St1};
prefixexp_element(#mcall{m=#lit{v=N},as=As0}=M, Loc, St0) ->
    {As1,Asfree,St1} = explist(As0, Loc, St0),
    Free = maybe_free(N, Loc, Asfree),
    {M#mcall{as=As1},Free,St1}.

%% functiondef(Func, LocalVars, State) -> {Func,FreeVars,State}.

functiondef(#fdef{ps=Ps,ss=Ss0}=F, _, St0) ->
    Loc0 = lists:foldl(fun (#var{n=N}, Vs) -> add_element(N, Vs);
			   (_, Vs) -> Vs
		       end, [], Ps),
    {Ss1,Loc1,Bfree,Bused,St1} = stats(Ss0, Loc0, [], [], St0),
    {F#fdef{ss=Ss1,local=Loc1,free=Bfree,used=Bused},Bfree,St1}.

%% tableconstructor(Fields, LocalVars, State) -> {Fields,FreeVars,State}.

tableconstructor(Fs0, Loc, St0) ->
    Fun = fun (#efield{v=V0}=F, {Free,S0}) ->
		  {V1,Vfree,S1} = exp(V0, Loc, S0),
		  {F#efield{v=V1},{union(Vfree, Free),S1}};
	      (#kfield{k=K0,v=V0}=F, {Free,S0}) ->
		  {K1,Kfree,S1} = exp(K0, Loc, S0),
		  {V1,Vfree,S2} = exp(V0, Loc, S1),
		  {F#kfield{k=K1,v=V1},{union([Kfree,Vfree,Free]),S2}}
	  end,
    {Fs1,{Free,St1}} = lists:mapfoldl(Fun, {[],St0}, Fs0),
    {Fs1,Free,St1}.

%% maybe_free(Var, LocalVars, FreeVars) -> NewFreeVars.
%%  Add Var to free vars if not a local var.

maybe_free(V, Local, Free) ->
    case is_element(V, Local) of
	true -> Free;
	false -> add_element(V, Free)
    end.
