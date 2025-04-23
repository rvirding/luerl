%% Copyright (c) 2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : luerl_comp_env.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.3 compiler for Luerl.

%% Does variable and stack analysis in the compiler

-module(luerl_comp_env).

-include("luerl.hrl").
-include("luerl_comp.hrl").

?MODULEDOC(false).

-export([chunk/2]).

-import(ordsets, [is_element/2,intersection/2,subtract/2]).

%% Local state.
-record(c_env, {lfs=[],				%Variable frames
		efs=[],				%Environment frames
		vars=none,
		fs=[],
		locv=false,			%Local variables
		locf				%Local frame
	      }).

%% chunk(Code, CompInfo) -> {ok,Code}.

chunk(Code0, #cinfo{opts=Opts}=_Ci) ->
    St0 = #c_env{},				%Local state
    {Code1,_} = functiondef(Code0, St0),
    luerl_comp:debug_print(Opts, "ce: ~p\n", [Code1]),
    {ok,Code1}.

%% alloc_frame(State) -> State.
%% pop_frame(State) -> State.
%% get_frame(State) -> Frame.

alloc_frame(#c_env{vars=#vars{},fs=Fs}=St) ->
    F = new_frame(),
    St#c_env{fs=[F|Fs]}.

pop_frame(#c_env{fs=[_|Fs]}=St) -> St#c_env{fs=Fs}.

get_frame(#c_env{fs=[F|_]}) -> F.

%% new_frame(LocalSize, EnvSize) -> Frame.
%%  We know frame will be tuples which we index from 1. Also Lua has
%%  the feature that every time you add a local variable you get a new
%%  version of it which shadows the old one. We handle this by keeping
%%  them in reverse order and always pushing variable to front of
%%  list.
%%
%%  We get the size from the index of the last variable of each type added.
%%
%%  NOTE: We can have empty frames here. The emulator knows about this
%%  and can handle it.
%%
%% Frame :: {LocalIndex,EnvIndex,Vars}
%% Var :: {Name,Type,Index}

new_frame() -> {0,0,[]}.

find_frame_var(N, {_,_,Fs}) ->
    find_frame_var_1(N, Fs).

find_frame_var_1(N, [{N,Type,I}|_]) -> {yes,Type,I};
find_frame_var_1(N, [_|F]) -> find_frame_var_1(N, F);
find_frame_var_1(_, []) -> no.

frame_local_size({Li,_,_}) -> Li.
frame_env_size({_,Ei,_}) -> Ei.

add_frame_local_var(N, {Li,Ei,Fs}) ->
    {Li+1,Ei,[{N,lvar,Li+1}|Fs]}.

add_frame_env_var(N, {Li,Ei,Fs}) ->
    {Li,Ei+1,[{N,evar,Ei+1}|Fs]}.

%% find_fs_var(Name, FrameStack) -> {yes,Type,Depth,Index} | no.
%%  Find a variable in the frame stack returning its depth and
%%  index.

find_fs_var(N, Fs) -> find_fs_var(N, Fs, 1, 1).

find_fs_var(N, [F|Fs], Ld, Ed) ->
    case find_frame_var(N, F) of
	{yes,lvar,Li} -> {yes,lvar,Ld,Li};
	{yes,evar,Ei} -> {yes,evar,Ed,Ei};
	no ->
	    Ld1 = Ld + 1,
	    Ed1 = Ed + 1,
	    find_fs_var(N, Fs, Ld1, Ed1)
    end;
find_fs_var(_, [], _, _) -> no.

%% add_var(Var, State) -> State.
%% get_var(Var, State) -> #lvar{} | #evar{} | #gvar{}.

add_var(#var{name=N}, St) ->
    case var_type(N, St) of
	local -> add_local_var(N, St);
	env -> add_env_var(N, St)
    end.
	    
add_env_var(V, #c_env{fs=[F0|Fs]}=St) ->
    F1 = add_frame_env_var(V, F0),
    St#c_env{fs=[F1|Fs]}.
	    
add_local_var(N, #c_env{fs=[F0|Fs]}=St) ->
    F1 = add_frame_local_var(N, F0),
    St#c_env{fs=[F1|Fs]}.

get_var(#var{l=Line,name=N}, #c_env{fs=Fs}) ->
    case find_fs_var(N, Fs) of
	{yes,lvar,Ld,Li} -> #lvar{l=Line,n=N,d=Ld,i=Li};
	{yes,evar,Ed,Ei} -> #evar{l=Line,n=N,d=Ed,i=Ei};
	no -> #gvar{l=Line,n=N}
    end.

var_type(N, #c_env{vars=#vars{fused=Fused}}) ->
    case is_element(N, Fused) of
	true -> env;
	false -> local
    end.

%% stmt(Stmts, State) -> {Stmts,State}.

stmts([S0|Ss0], St0) ->
    {S1,St1} = stmt(S0, nul, St0),
    %% io:format("ss1: ~p\n", [{Loc0,Free0,Used0}]),
    {Ss1,St2} = stmts(Ss0, St1),
    {[S1|Ss1],St2};
stmts([], St) -> {[],St}.

%% stmt(Stmt, State) -> {Stmt,State}.

stmt(#assign_stmt{}=A, _, St) -> assign_stmt(A, St);
stmt(#call_stmt{}=C, _, St) -> call_stmt(C, St);
stmt(#return_stmt{}=R, _, St) -> return_stmt(R, St);
stmt(#break_stmt{}=B, _, St) -> {B,St};
stmt(#block_stmt{}=B, _, St) -> block_stmt(B, St);
stmt(#while_stmt{}=W, _, St) -> while_stmt(W, St);
stmt(#repeat_stmt{}=R, _, St) -> repeat_stmt(R, St);
stmt(#if_stmt{}=I, _, St) -> if_stmt(I, St);
stmt(#nfor_stmt{}=F, _, St) -> numfor_stmt(F, St);
stmt(#gfor_stmt{}=F, _, St) -> genfor_stmt(F, St);
stmt(#local_assign_stmt{}=L, _, St) ->
    local_assign_stmt(L, St);
stmt(#local_fdef_stmt{}=L, _, St) ->
    local_fdef_stmt(L, St);
stmt(#expr_stmt{}=E, _, St) ->
    expr_stmt(E, St).

%% assign_stmt(Assign, State) -> {Assign,State}.

assign_stmt(#assign_stmt{vars=Vs0,exps=Es0}=A, St0) ->
    {Vs1,St1} = assign_loop(Vs0, St0),
    {Es1,St2} = explist(Es0, St1),
    {A#assign_stmt{vars=Vs1,exps=Es1},St2}.

assign_loop([V0|Vs0], St0) ->
    {V1,St1} = var(V0, St0),
    {Vs1,St2} = assign_loop(Vs0, St1),
    {[V1|Vs1],St2};
assign_loop([], St) -> {[],St}.

var(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_first(Exp0, St0),
    {Rest1,St2} = var_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},St2};
var(#var{}=V0, St) ->
    V1 = get_var(V0, St),
    {V1,St}.

var_rest(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_element(Exp0, St0),
    {Rest1,St2} = var_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},St2};
var_rest(Exp, St) -> var_last(Exp, St).

var_last(#key{key=Exp0}=K, St0) ->
    {Exp1,St1} = exp(Exp0, St0),
    {K#key{key=Exp1},St1}.

%% call_stmt(Call, State) -> {Call,State}.

call_stmt(#call_stmt{call=Exp0}=C, St0) ->
    {Exp1,St1} = exp(Exp0, St0),
    {C#call_stmt{call=Exp1},St1}.

%% return_stmt(Return, State) -> {Return,State}.

return_stmt(#return_stmt{exps=Es0}=R, St0) ->
    {Es1,St1} = explist(Es0, St0),
    {R#return_stmt{exps=Es1},St1}.

%% block_stmt(Block, State) -> {Block,State}.

block_stmt(#block_stmt{body=Ss0,vars=Vars}=B, St0) ->
    Do = fun(S) -> stmts(Ss0, S) end,
    {Ss1,Fr,St1} = with_block(Do, Vars, St0),
    Lsz = frame_local_size(Fr),
    Esz = frame_env_size(Fr),
    {B#block_stmt{body=Ss1,lsz=Lsz,esz=Esz},St1}.

%% do_block(Block, State) -> {Block,State}.

do_block(#block{body=Ss0,vars=Vars}=B, St0) ->
    Do = fun(S) -> stmts(Ss0, S) end,
    {Ss1,Fr,St1} = with_block(Do, Vars, St0),
    Lsz = frame_local_size(Fr),
    Esz = frame_env_size(Fr),
    {B#block{body=Ss1,lsz=Lsz,esz=Esz},St1}.

%% with_block(Do, Vars, State) -> {Ret,State}.
%% with_block(Do, Env, Vars, State) -> {Ret,State}.
%%  Do a block initialising/clearing frames. We always push a local
%%  frame even if it not used.

with_block(Do, Vars, #c_env{vars=OldVars}=St0) ->
    St1 = alloc_frame(St0#c_env{vars=Vars}),
    {Ret,St2} = Do(St1),
    Fr = get_frame(St2),
    St3 = pop_frame(St2),
    {Ret,Fr,St3#c_env{vars=OldVars}}.

%% while_stmt(While, State) -> {While,State}.

while_stmt(#while_stmt{exp=E0,body=B0}=W, St0) ->
    {E1,St1} = exp(E0, St0),
    {B1,St2} = do_block(B0, St1),
    {W#while_stmt{exp=E1,body=B1},St2}.

%% repeat_stmt(Repeat, State) -> {Repeat,State}.

repeat_stmt(#repeat_stmt{body=B0}=R, St0) ->
    {B1,St1} = do_block(B0, St0),
    {R#repeat_stmt{body=B1},St1}.

%% if_stmt(If, State) -> {If,State}.

if_stmt(#if_stmt{tests=Ts0,else_block=E0}=I, St0) ->
    {Ts1,St1} = if_tests(Ts0, St0),
    {E1,St2} = do_block(E0, St1),
    {I#if_stmt{tests=Ts1,else_block=E1},St2}.

if_tests([{E0,B0}|Ts0], St0) ->
    {E1,St1} = exp(E0, St0),
    {B1,St2} = do_block(B0, St1),
    {Ts1,St3} = if_tests(Ts0, St2),
    {[{E1,B1}|Ts1],St3};
if_tests([], St) -> {[],St}.

%% numfor_stmt(For, State) -> {For,State}.

numfor_stmt(#nfor_stmt{var=V0,init=I0,limit=L0,step=S0,body=B0}=F, St0) ->
    {[I1,L1,S1],St1} = explist([I0,L0,S0], St0),
    {[V1],B1,St2} = for_block([V0], B0, St1),
    {F#nfor_stmt{var=V1,init=I1,limit=L1,step=S1,body=B1},St2}.

%% genfor_stmt(For, State) -> {For,State}.

genfor_stmt(#gfor_stmt{vars=Vs0,gens=Gs0,body=B0}=F, St0) ->
    {Gs1,St1} = explist(Gs0, St0),
    {Vs1,B1,St2} = for_block(Vs0, B0, St1),
    {F#gfor_stmt{vars=Vs1,gens=Gs1,body=B1},St2}.

for_block(Vs0, #block{body=Ss0,vars=Vars}=B, St0) ->
    Do = fun (S0) ->
		 Fun = fun (V, Sa) ->
			       Sb = add_var(V, Sa),
			       {get_var(V, Sb),Sb}
		       end,
		 {Vs1,S1} = lists:mapfoldl(Fun, S0, Vs0),
		 {Ss1,S2} = stmts(Ss0, S1),
		 {{Vs1,Ss1},S2}
	 end,
    {{Vs1,Ss1},Fr,St1} = with_block(Do, Vars, St0),
    Lsz = frame_local_size(Fr),
    Esz = frame_env_size(Fr),
    {Vs1,B#block{body=Ss1,lsz=Lsz,esz=Esz},St1}.

%% local_assign_stmt(Local, State) -> {Local,State}.

local_assign_stmt(#local_assign_stmt{vars=Vs0,exps=Es0}=L, St0) ->
    %% io:fwrite("las: ~p\n", [{Es0,St0}]),
    {Es1,St1} = explist(Es0, St0),
    %% io:fwrite("las> ~p\n", [{Es1,St1}]),
    AddVar = fun (V, S0) ->
		     S1 = add_var(V, S0),
		     {get_var(V, S1),S1}
	     end,
    {Vs1,St2} = lists:mapfoldl(AddVar, St1, Vs0),
    %% io:fwrite("las> ~p\n", [{Vs1,St2}]),
    {L#local_assign_stmt{vars=Vs1,exps=Es1},St2}.

%% local_fdef_stmt(Local, State) -> {Local,State}.
%%  Add function name first in case of recursive call.

local_fdef_stmt(#local_fdef_stmt{var=V,func=F0}=L, St0) ->
    St1 = add_var(V, St0),
    {F1,St2} = functiondef(F0, St1),
    V1 = get_var(V, St2),
    %% io:fwrite("lf: ~p\n", [St0]),
    %% io:fwrite("lf: ~p\n", [St1]),
    %% io:fwrite("lf: ~p\n", [St2]),
    {L#local_fdef_stmt{var=V1,func=F1},St2}.

%% expr_stmt(Expr, State) -> {Call,State}.
%%  The expression pseudo statement. This will return a single value.

expr_stmt(#expr_stmt{exp=Exp0}=E, St0) ->
    {Exp1,St1} = exp(Exp0, St0),
    {E#expr_stmt{exp=Exp1},St1}.

%% explist(Exprs, State) -> {Exprs,State}.
%% exp(Expr, State) -> {Expr,State}.
%% prefixexp(Expr, State) -> {Expr,State}.

explist([E0|Es0], St0) ->
    {E1,St1} = exp(E0, St0),
    {Es1,St2} = explist(Es0, St1),
    {[E1|Es1],St2};
explist([], St) -> {[],St}.			%No expressions at all

exp(#lit{}=L, St) -> {L,St};			%Nothing to do
exp(#fdef{}=F, St) -> functiondef(F, St);
exp(#op{args=Es0}=Op, St0) ->
    {Es1,St1} = explist(Es0, St0),
    {Op#op{args=Es1},St1};
exp(#tabcon{fields=Fs0}=T, St0) ->
    {Fs1,St1} = tableconstructor(Fs0, St0),
    {T#tabcon{fields=Fs1},St1};
exp(E, St) ->
    prefixexp(E, St).

prefixexp(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_first(Exp0, St0),
    {Rest1,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},St2};
prefixexp(Exp, St) -> prefixexp_first(Exp, St).

prefixexp_first(#single{exp=E0}=S, St0) ->
    {E1,St1} = exp(E0, St0),
    {S#single{exp=E1},St1};
prefixexp_first(#var{}=V0, St) ->
    V1 = get_var(V0, St),
    {V1,St}.

prefixexp_rest(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,St1} = prefixexp_element(Exp0, St0),
    {Rest1,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},St2};
prefixexp_rest(Exp, St) -> prefixexp_element(Exp, St).

prefixexp_element(#key{key=E0}=K, St0) ->
    {E1,St1} = exp(E0, St0),
    {K#key{key=E1},St1};
prefixexp_element(#fcall{args=As0}=F, St0) ->
    {As1,St1} = explist(As0, St0),
    {F#fcall{args=As1},St1};
prefixexp_element(#mcall{args=As0}=M, St0) ->
    {As1,St1} = explist(As0, St0),
    {M#mcall{args=As1},St1}.

%% functiondef(Func, State) -> {Func,State}.

functiondef(#fdef{pars=Ps0,body=Ss0,vars=Vars}=F, St0) ->
    Do = fun (S0) ->
		 Fun = fun (V, Sa) ->
			       Sb = add_var(V, Sa),
			       {get_var(V, Sb),Sb}
		       end,
		 {Ps1,S1} = lists:mapfoldl(Fun, S0, Ps0),
		 {Ss1,S2} = stmts(Ss0, S1),
		 {{Ps1,Ss1},S2}
	 end,
    {{Ps1,Ss1},Fr,St1} = with_block(Do, Vars, St0),
    Lsz = frame_local_size(Fr),
    Esz = frame_env_size(Fr),
    {F#fdef{pars=Ps1,body=Ss1,lsz=Lsz,esz=Esz},St1}.

%% tableconstructor(Fields, State) -> {Fields,State}.

tableconstructor(Fs0, St0) ->
    Fun = fun (#efield{val=V0}=F, S0) ->
		  {V1,S1} = exp(V0, S0),
		  {F#efield{val=V1},S1};
	      (#kfield{key=K0,val=V0}=F, S0) ->
		  {K1,S1} = exp(K0, S0),
		  {V1,S2} = exp(V0, S1),
		  {F#kfield{key=K1,val=V1},S2}
	  end,
    {Fs1,St1} = lists:mapfoldl(Fun, St0, Fs0),
    {Fs1,St1}.
