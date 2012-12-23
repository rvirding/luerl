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

%% File    : luerl_comp_locf.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does local function analysis.

-module(luerl_comp_locf).

-include("luerl.hrl").

-include("luerl_comp.hrl").

-export([chunk/2]).

-import(ordsets, [add_element/2,is_element/2,union/1,union/2,
		  subtract/2,intersection/2]).

chunk(#chunk{code=C0}=St0, Opts) ->
    {C1,_,St1} = exp(C0, St0),
    debug_print(Opts, "cf: ~p\n", [C1]),
    {ok,St1#chunk{code=C1}}.

debug_print(Opts, Format, Args) ->
    case lists:member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

%% stat(Stats, State) ->
%%     {Stats,LocalFunc,State}.
%%  Main problem here is to calculate local/free/used variables in the
%%  right order. Must do everything going forwards.

stats([S0|Ss0], St0) ->
    {S1,Slocf,St1} = stat(S0, St0),
    {Ss1,Sslocf,St2} = stats(Ss0, St1),
    Locf = Slocf or Sslocf,
    {[S1|Ss1],Locf,St2};
stats([], St) -> {[],false,St}.

%% stat(Stat, State) -> {Stat,LocalFunc,State}.

stat(#assign{}=A, St) -> assign_stat(A, St);
stat(#return{}=R, St) -> return_stat(R, St);
stat(#break{}=B, St) -> {B,false,St};
stat(#block{}=B, St) -> block_stat(B, St);
stat(#while{}=W, St) -> while_stat(W, St);
stat(#repeat{}=R, St) -> repeat_stat(R, St);
stat(#'if'{}=If, St) -> if_stat(If, St);
stat(#nfor{}=For, St) -> numfor_stat(For, St);
stat(#gfor{}=For, St) -> genfor_stat(For, St);
stat(#local_assign{}=L, St) ->
    local_assign_stat(L, St);
stat(#local_fdef{}=L, St) ->
    local_fdef_stat(L, St);
stat(Exp0, St0) ->
    {Exp1,Locf,St1} = exp(Exp0, St0),
    {Exp1,Locf,St1}.

%% assign_stat(Assign, State) -> {Assign,LocalFunc,State}.

assign_stat(#assign{vs=Vs0,es=Es0}=A, St0) ->
    {Vs1,Vlocf,St1} = assign_loop(Vs0, St0),
    {Es1,Elocf,St2} = explist(Es0, St1),
    Locf = Vlocf or Elocf,
    {A#assign{vs=Vs1,es=Es1},Locf,St2}.

assign_loop([V0|Vs0], St0) ->
    {V1,Vlocf,St1} = var(V0, St0),
    {Vs1,Vslocf,St2} = assign_loop(Vs0, St1),
    Locf = Vlocf or Vslocf,
    {[V1|Vs1],Locf,St2};
assign_loop([], St) -> {[],false,St}.

var(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_first(Exp0, St0),
    {Rest1,Rlocf,St2} = var_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},Elocf or Rlocf,St2};
var(V, St) ->
    {V,false,St}.

var_rest(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_element(Exp0, St0),
    {Rest1,Rlocf,St2} = var_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},Elocf or Rlocf,St2};
var_rest(Exp, St) -> var_last(Exp, St).

var_last(#key{k=Exp0}=K, St0) ->
    {Exp1,Elocf,St1} = exp(Exp0, St0),
    {K#key{k=Exp1},Elocf,St1}.

%% return_stat(Return, State) -> {Return,LocalFunc,State}.

return_stat(#return{es=Es0}=R, St0) ->
    {Es1,Locf,St1} = explist(Es0, St0),
    {R#return{es=Es1},Locf,St1}.

%% block_stat(Block, State) -> {Block,LocalFunc,State}.

block_stat(B0, St0) ->
    {B1,Locf,St1} = do_block(B0, St0),
    {B1,Locf,St1}.

%% do_block(Block, State) -> {Block,LocalFunc,State}.
%%  Do_block never returns external new variables as it never exports
%%  variables.

do_block(#block{ss=Ss0}=B, St0) ->
    {Ss1,Blocf,St1} = stats(Ss0, St0),
    {B#block{ss=Ss1,locf=Blocf},Blocf,St1}.

%% while_stat(While, State) -> {While,LocalFunc,State}.
%%  While_stat never returns external new variables. Fits into stat().
%%  The test expression is done in the context of the surrounding
%%  block.

while_stat(#while{e=E0,b=B0}=W, St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    {W#while{e=E1,b=B1},Elocf or Blocf,St2}.

%% repeat_stat(Repeat, State) -> {Repeat,LocalFunc,State}.
%%  Repeat_stat never returns external new variables. Fits into
%%  stat(). The test expression is done in the context of the repeat
%%  block.

repeat_stat(#repeat{b=B0,e=E0}=R, St0) ->
    {B1,Blocf,St1} = repeat_block(B0, St0),
    {E1,Elocf,St2} = exp(E0, St1),
    Locf = Blocf or Elocf,
    {R#repeat{b=B1,e=E1},Locf,St2}.

repeat_block(#block{ss=B0}=B, St0) ->
    {B1,Blocf,St1} = stats(B0, St0),
    %% Now do the test in the context of the block.
    {B#block{ss=B1,locf=Blocf},Blocf,St1}.

%% if_stat(If, State) -> {If,LocalFunc,State}.
%%  The block info includes anything from the test expressions even
%%  though we keep them separate.

if_stat(#'if'{tests=Ts0,else=E0}=If, St0) ->
    {Ts1,Tlocf,St1} = if_tests(Ts0, St0),
    {E1,Elocf,St2} = do_block(E0, St1),
    Locf = Tlocf or Elocf,
    {If#'if'{tests=Ts1,else=E1},Locf,St2}.

if_tests([{E0,B0}|Ts0], St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    {Ts1,Tslocf,St3} = if_tests(Ts0, St2),
    Locf = Elocf or Blocf or Tslocf,
    {[{E1,B1}|Ts1],Locf,St3};
if_tests([], St) -> {[],false,St}.

%% numfor_stat(For, State) -> {For,LocalFunc,State}.

numfor_stat(#nfor{init=I0,limit=L0,step=S0,b=B0}=For, St0) ->
    {[I1,L1,S1],Eslocf,St1} = explist([I0,L0,S0], St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    Locf = Eslocf or Blocf,
    {For#nfor{init=I1,limit=L1,step=S1,b=B1},Locf,St2}.

%% genfor_stat(For, State) -> {For,LocalFunc,State}.

genfor_stat(#gfor{gens=Gs0,b=B0}=For, St0) ->
    {Gs1,Glocf,St1} = explist(Gs0, St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    Locf = Glocf or Blocf,
    {For#gfor{gens=Gs1,b=B1},Locf,St2}.

%% local_assign_stat(Local, State) -> {Local,LocalFunc,State}.

local_assign_stat(#local_assign{es=Es0}=L, St0) ->
    {Es1,Eslocf,St1} = explist(Es0, St0),
    {L#local_assign{es=Es1},Eslocf,St1}.

%% local_fdef_stat(Local, State) -> {Local,LocalFunc,State}.

local_fdef_stat(#local_fdef{f=F0}=L, St0) ->
    {F1,_,St1} = functiondef(F0, St0),		%Don't care what's in func
    {L#local_fdef{f=F1},true,St1}.

%% explist(Exprs, State) -> {Exprs,LocalFunc,State}.
%% exp(Expr, State) -> {Expr,LocalFunc,State}.
%%  An expression can never create new local variables.

explist([E0|Es0], St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {Es1,Eslocf,St2} = explist(Es0, St1),
    {[E1|Es1],Elocf or Eslocf,St2};
explist([], St) -> {[],false,St}.		%No expressions at all

exp(#lit{}=L, St) -> {L,false,St};		%Nothing to do
exp(#fdef{}=F0, St0) ->
    {F1,_,St1} = functiondef(F0, St0),
    {F1,true,St1};
exp(#op{as=Es0}=Op, St0) ->
    {Es1,Eslocf,St1} = explist(Es0, St0),
    {Op#op{as=Es1},Eslocf,St1};
exp(#tc{fs=Fs0}=T, St0) ->
    {Fs1,Tlocf,St1} = tableconstructor(Fs0, St0),
    {T#tc{fs=Fs1},Tlocf,St1};
exp(E, St) ->
    prefixexp(E, St).

prefixexp(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_first(Exp0, St0),
    {Rest1,Rlocf,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},Elocf or Rlocf,St2};
prefixexp(Exp, St) -> prefixexp_first(Exp, St).

prefixexp_first(#single{e=E0}=S, St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {S#single{e=E1},Elocf,St1};
prefixexp_first(V, St) ->
    {V,false,St}.

prefixexp_rest(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_element(Exp0, St0),
    {Rest1,Rlocf,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},Elocf or Rlocf,St2};
prefixexp_rest(Exp, St) -> prefixexp_element(Exp, St).

prefixexp_element(#key{k=E0}=K, St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {K#key{k=E1},Elocf,St1};
prefixexp_element(#fcall{as=As0}=F, St0) ->
    {As1,Aslocf,St1} = explist(As0, St0),
    {F#fcall{as=As1},Aslocf,St1};
prefixexp_element(#mcall{as=As0}=M, St0) ->
    {As1,Aslocf,St1} = explist(As0, St0),
    {M#mcall{as=As1},Aslocf,St1}.

%% functiondef(Func, State) -> {Func,LocalFunc,State}.
%%  We return if there are any internal function definitions within
%%  the function.

functiondef(#fdef{ss=Ss0}=F, St0) ->
    {Ss1,Blocf,St1} = stats(Ss0, St0),
    {F#fdef{ss=Ss1,locf=Blocf},Blocf,St1}.

%% tableconstructor(Fields, State) -> {Fields,LocalFunc,State}.

tableconstructor(Fs0, St0) ->
    Fun = fun (#efield{v=V0}=F, {Locf,S0}) ->
		  {V1,Vlocf,S1} = exp(V0, S0),
		  {F#efield{v=V1},{Locf or Vlocf,S1}};
	      (#kfield{k=K0,v=V0}=F, {Locf,S0}) ->
		  {K1,Klocf,S1} = exp(K0, S0),
		  {V1,Vlocf,S2} = exp(V0, S1),
		  {F#kfield{k=K1,v=V1},{Locf or Klocf or Vlocf,S2}}
	  end,
    {Fs1,{Locf,St1}} = lists:mapfoldl(Fun, {false,St0}, Fs0),
    {Fs1,Locf,St1}.
