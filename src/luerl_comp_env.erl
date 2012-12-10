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

%% File    : luerl_comp_env.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does variable and stack analysis in the compiler

-module(luerl_comp_env).

-include("luerl.hrl").

-include("luerl_comp.hrl").

-export([chunk/2]).

%% chunk(St0) -> {ok,St0};
chunk(#chunk{code=C0}=St0, Opts) ->
    {C1,St1} = exp(C0, St0),
    debug_print(Opts, "ce: ~p\n", [C1]),
    {ok,St1#chunk{code=C1}}.

debug_print(Opts, Format, Args) ->
    case lists:member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

%% push_frame(State) -> State.
%% push_frame(Frame, State) -> State.
%% pop_frame(State) -> State.

push_frame(St) -> push_frame(new_frame(), St).

push_frame(F, #chunk{fs=Fs}=St) ->
    St#chunk{fs=[F|Fs]}.

pop_frame(#chunk{fs=[_|Fs]}=St) ->
    St#chunk{fs=Fs}.

get_frame(#chunk{locv=true,locf=F}) -> F;
get_frame(#chunk{locv=false,fs=[F|_]}) -> F.

%% new_frame() -> Frame.
%% add_frame_var(Name, Frame) -> Frame.
%% find_frame_var(Name, Frame) -> {yes,Index} | no.
%% fetch_frame_var(Name, Frame) -> Index.
%%  We know frame will be tuples which we index from 1. Also Lua has
%%  the feature that every time you add a local variable you get a new
%%  version of it which shadows the old one. We handle this by keeping
%%  them in reverse order and always pushing variable to front of
%%  list.

new_frame() -> [].

add_frame_var(N, [{_,I}|_]=F) -> [{N,I+1}|F];
add_frame_var(N, []) -> [{N,1}].

find_frame_var(N, [{N,I}|_]) -> {yes,I};
find_frame_var(N, [_|F]) -> find_frame_var(N, F);
find_frame_var(_, []) -> no.

%% add_fs_var(Name, FrameStack) -> FrameStack.
%% find_fs_var(Name, FrameStack) -> {yes,Depth,Index} | no.

add_fs_var(N, [F|Fs]) -> [add_frame_var(N, F)|Fs].

find_fs_var(N, Fs) -> find_fs_var(N, Fs, 1).

find_fs_var(N, [F|Fs], D) ->
    case find_frame_var(N, F) of
	{yes,I} -> {yes,D,I};
	no -> find_fs_var(N, Fs, D+1)
    end;
find_fs_var(_, [], _) -> no.

add_local_var(N, #chunk{locv=true,locf=F0}=St) ->
    F1 = add_frame_var(N, F0),
    St#chunk{locf=F1};
add_local_var(N, #chunk{locv=false,fs=Fs0}=St) ->
    Fs1 = add_fs_var(N, Fs0),
    St#chunk{fs=Fs1}.

get_var(N, #chunk{locv=true,locf=F,fs=Fs}) ->
    case find_frame_var(N, F) of
	{yes,I} -> #lvar{n=N,i=I};
	no ->
	    case find_fs_var(N, Fs) of
		{yes,D,I} -> #fvar{n=N,d=D,i=I};
		no -> #gvar{n=N}
	    end
    end;
get_var(N, #chunk{locv=false,fs=Fs}) ->
    case find_fs_var(N, Fs) of
	{yes,D,I} -> #fvar{n=N,d=D,i=I};
	no -> #gvar{n=N}
    end.

%% stat(Stats, State) -> {Stats,State}.

stats([S0|Ss0], St0) ->
    {S1,St1} = stat(S0, nul, St0),
    %% io:format("ss1: ~p\n", [{Loc0,Free0,Used0}]),
    {Ss1,St2} = stats(Ss0, St1),
    {[S1|Ss1],St2};
stats([], St) -> {[],St}.

%% stat(Stat, LocalVars, State) -> {Stat,State}.

stat(#assign{}=A, _, St) ->
    assign_stat(A, St);
stat(#return{es=Es0}=R, _, St0) ->
    {Es1,St1} = explist(Es0, St0),
    {R#return{es=Es1},St1};
stat(#break{}=B, _, St) -> {B,St};
stat(#block{}=B, _, St) ->			%Sub-block
    block_stat(B, St);
stat(#while{}=W, _, St) ->
    while_stat(W, St);
stat(#repeat{}=R, _, St) ->
    repeat_stat(R, St);
stat(#'if'{}=I, _, St) ->
    if_stat(I, St);
stat(#nfor{}=F, _, St) ->
    numfor_stat(F, St);
stat(#gfor{}=F, _, St) ->
    genfor_stat(F, St);
stat(#local_assign{}=L, _, St) ->
    local_assign_stat(L, St);
stat(#local_fdef{}=L, _, St) ->
    local_fdef_stat(L, St);
stat(Exp0, _, St0) ->
    {Exp1,St1} = exp(Exp0, St0),
    {Exp1,St1}.

%% assign_stat(Assign, State) -> {Assign,State}.

assign_stat(#assign{vs=Vs0,es=Es0}=A, St0) ->
    {Vs1,St1} = assign_loop(Vs0, St0),
    {Es1,St2} = explist(Es0, St1),
    {A#assign{vs=Vs1,es=Es1},St2}.

assign_loop([V0|Vs0], St0) ->
    {V1,St1} = var(V0, St0),
    {Vs1,St2} = assign_loop(Vs0, St1),
    {[V1|Vs1],St2};
assign_loop([], St) -> {[],St}.

var(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_first(Exp0, St0),
    {Rest1,St2} = var_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},St2};
var(#var{n=N}, St) ->
    V = get_var(N, St),
    {V,St}.

var_rest(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_element(Exp0, St0),
    {Rest1,St2} = var_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},St2};
var_rest(Exp, St) -> var_last(Exp, St).

var_last(#key{k=Exp0}=K, St0) ->
    {Exp1,St1} = exp(Exp0, St0),
    {K#key{k=Exp1},St1}.

%% block_stat(Block, State) -> {Block,State}.

block_stat(Block, St) -> do_block(Block, St).

%% do_block(Block, State) -> {Block,State}.
%%  Do_block never returns external new variables. Fits into stat().

do_block(#block{ss=Ss0,used=U}=B, St0) ->
    Do = fun(S0) ->
		 {Ss1,S1} = stats(Ss0, S0),
		 Frame = get_frame(S1),
		 {{Frame,Ss1},S1}
	 end,
    {{Fr,Ss1},St1} = with_block(Do, U == [], St0),
    {B#block{ss=Ss1,local=Fr},St1}.

%% with_block(Do, LocalBlock, State) -> {Ret,State}.
%%  Do a block initialising/clearing frames.

with_block(Do, true, #chunk{locv=Locv0,locf=Locf0}=St0) ->
    St1 = St0#chunk{locv=true,locf=new_frame()},
    {Ret,St2} = Do(St1),
    St3 = St2#chunk{locv=Locv0,locf=Locf0},
    {Ret,St3};
with_block(Do, false, #chunk{locv=Locv0}=St0) ->
    St1 = St0#chunk{locv=false},
    St2 = push_frame(St1),
    {Ret,St3} = Do(St2),
    St4 = pop_frame(St3),
    St5 = St4#chunk{locv=Locv0},
    {Ret,St5}.

%% while_stat(While, State) -> {While,State}.

while_stat(#while{e=E0,b=B0}=W, St0) ->
    {E1,St1} = exp(E0, St0),
    {B1,St2} = do_block(B0, St1),
    {W#while{e=E1,b=B1},St2}.

%% repeat_stat(Repeat, State) -> {Repeat,State}.

repeat_stat(#repeat{b=B0,e=E0}=R, St0) ->
    {B1,St1} = do_block(B0, St0),
    {E1,St2} = exp(E0, St1),
    {R#repeat{b=B1,e=E1},St2}.

%% if_stat(If, State) -> {If,State}.

if_stat(#'if'{tests=Ts0,else=E0}=I, St0) ->
    {Ts1,St1} = if_tests(Ts0, St0),
    {E1,St2} = do_block(E0, St1),
    {I#'if'{tests=Ts1,else=E1},St2}.

if_tests([{E0,B0}|Ts0], St0) ->
    {E1,St1} = exp(E0, St0),
    {B1,St2} = do_block(B0, St1),
    {Ts1,St3} = if_tests(Ts0, St2),
    {[{E1,B1}|Ts1],St3};
if_tests([], St) -> {[],St}.

%% numfor_stat(For, State) -> {For,State}.

numfor_stat(#nfor{v=V0,init=I0,limit=L0,step=S0,b=B0}=F, St0) ->
    {[I1,L1,S1],St1} = explist([I0,L0,S0], St0),
    {[V1],B1,St2} = for_block([V0], B0, St1),
    {F#nfor{v=V1,init=I1,limit=L1,step=S1,b=B1},St2}.

%% genfor_stat(For, State) -> {For,State}.

genfor_stat(#gfor{vs=Vs0,gens=Gs0,b=B0}=F, St0) ->
    {Gs1,St1} = explist(Gs0, St0),
    {Vs1,B1,St2} = for_block(Vs0, B0, St1),
    {F#gfor{vs=Vs1,gens=Gs1,b=B1},St2}.

for_block(Vs0, #block{ss=Ss0,used=U}=B, St0) ->
    Do = fun (S0) ->
		 Fun = fun (#var{n=N}, Sa) ->
			       Sb = add_local_var(N, Sa),
			       {get_var(N, Sb),Sb}
		       end,
		 {Vs1,S1} = lists:mapfoldl(Fun, S0, Vs0),
		 {Ss1,S2} = stats(Ss0, S1),
		 Frame = get_frame(S2),
		 {{Vs1,Frame,Ss1},S2}
	 end,
    {{Vs1,Fr,Ss1},St1} = with_block(Do, U == [], St0),
    {Vs1,B#block{ss=Ss1,local=Fr},St1}.

%% local_assign_stat(Local, State) -> {Local,State}.

local_assign_stat(#local_assign{vs=Vs0,es=Es0}=L, St0) ->
    {Es1,St1} = explist(Es0, St0),
    Fun = fun (#var{n=N}, S0) ->
		  S1 = add_local_var(N, S0),
		  {get_var(N, S1),S1}
	  end,
    {Vs1,St2} = lists:mapfoldl(Fun, St1, Vs0),
    {L#local_assign{vs=Vs1,es=Es1},St2}.

%% local_fdef_stat(Local, State) -> {Local,State}.

local_fdef_stat(#local_fdef{v=#var{n=N},f=F0}=L, St0) ->
    St1 = add_local_var(N, St0),
    {F1,St2} = functiondef(F0, St1),
    V1 = get_var(N, St2),
    %% io:fwrite("lf: ~p\n", [{St0#chunk.locv,St0#chunk.locf,St0#chunk.fs}]),
    %% io:fwrite("lf: ~p\n", [{St1#chunk.locv,St1#chunk.locf,St1#chunk.fs}]),
    %% io:fwrite("lf: ~p\n", [{St2#chunk.locv,St2#chunk.locf,St2#chunk.fs}]),
    {L#local_fdef{v=V1,f=F1},St2}.

%% explist(Exprs, LocalVars, State) -> {Exprs,FreeVars,State}.
%% exp(Expr, LocalVars, State) -> {Expr,FreeVars,State}.
%%  An expression can never create new local variables.

explist([E0|Es0], St0) ->
    {E1,St1} = exp(E0, St0),
    {Es1,St2} = explist(Es0, St1),
    {[E1|Es1],St2};
explist([], St) -> {[],St}.			%No expressions at all

exp(#lit{}=L, St) -> {L,St};			%Nothing to do
exp(#fdef{}=F, St) -> functiondef(F, St);
exp(#op{as=Es0}=Op, St0) ->
    {Es1,St1} = explist(Es0, St0),
    {Op#op{as=Es1},St1};
exp(#tc{fs=Fs0}=T, St0) ->
    {Fs1,St1} = tableconstructor(Fs0, St0),
    {T#tc{fs=Fs1},St1};
exp(E, St) ->
    prefixexp(E, St).

prefixexp(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_first(Exp0, St0),
    {Rest1,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},St2};
prefixexp(Exp, St) -> prefixexp_first(Exp, St).

prefixexp_first(#single{e=E0}=S, St0) ->
    {E1,St1} = exp(E0, St0),
    {S#single{e=E1},St1};
prefixexp_first(#var{n=N}, St) ->
    V = get_var(N, St),
    {V,St}.

prefixexp_rest(#dot{e=Exp0,r=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_element(Exp0, St0),
    {Rest1,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{e=Exp1,r=Rest1},St2};
prefixexp_rest(Exp, St) -> prefixexp_element(Exp, St).

prefixexp_element(#key{k=E0}=K, St0) ->
    {E1,St1} = exp(E0, St0),
    {K#key{k=E1},St1};
prefixexp_element(#fcall{as=As0}=F, St0) ->
    {As1,St1} = explist(As0, St0),
    {F#fcall{as=As1},St1};
prefixexp_element(#mcall{as=As0}=M, St0) ->
    {As1,St1} = explist(As0, St0),
    {M#mcall{as=As1},St1}.

%% functiondef(Func, State) -> {Func,State}.

functiondef(#fdef{ps=Ps0,ss=Ss0,used=U}=F, St0) ->
    Local = U == [],				%A local stack frame?
    Do = fun (S0) ->
		 Fun = fun (#var{n=N}, Sa) ->
			       Sb = add_local_var(N, Sa),
			       {get_var(N, Sb),Sb}
		       end,
		 {Ps1,S1} = lists:mapfoldl(Fun, S0, Ps0),
		 {Ss1,S2} = stats(Ss0, S1),
		 Frame = get_frame(S2),
		 {{Ps1,Frame,Ss1},S2}
	 end,
    {{Ps1,Fr,Ss1},St1} = with_block(Do, Local, St0),
    {F#fdef{ps=Ps1,ss=Ss1,local=Fr},St1}.

%% tableconstructor(Fields, State) -> {Fields,State}.

tableconstructor(Fs0, St0) ->
    Fun = fun (#efield{v=V0}=F, S0) ->
		  {V1,S1} = exp(V0, S0),
		  {F#efield{v=V1},S1};
	      (#kfield{k=K0,v=V0}=F, S0) ->
		  {K1,S1} = exp(K0, S0),
		  {V1,S2} = exp(V0, S1),
		  {F#kfield{k=K1,v=V1},S2}
	  end,
    {Fs1,St1} = lists:mapfoldl(Fun, St0, Fs0),
    {Fs1,St1}.
