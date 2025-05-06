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

%% File    : luerl_comp_locf.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.3 compiler for Luerl.

%% Does local function analysis.

-module(luerl_comp_locf).

-include("luerl.hrl").
-include("luerl_comp.hrl").

?MODULEDOC(false).

-export([chunk/2]).

-import(ordsets, [add_element/2,is_element/2,union/1,union/2,
		  subtract/2,intersection/2]).

%% chunk(Code, CompInfo) ->
%%     {ok,Code} | {ok,Code,Warnings} | {error,Error}.

chunk(Code0, #cinfo{opts=Opts}=_Ci) ->
    {Code1,_,nul} = exp(Code0, nul),		%No local state here!
    luerl_comp:debug_print(Opts, "cf: ~p\n", [Code1]),
    {ok,Code1}.

%% stmt(Stmts, State) -> {Stmts,LocalFunc,State}.

stmts([S0|Ss0], St0) ->
    {S1,Slocf,St1} = stmt(S0, St0),
    {Ss1,Sslocf,St2} = stmts(Ss0, St1),
    Locf = Slocf or Sslocf,
    {[S1|Ss1],Locf,St2};
stmts([], St) -> {[],false,St}.

%% stmt(Stmt, State) -> {Stmt,LocalFunc,State}.

stmt(#assign_stmt{}=A, St) -> assign_stmt(A, St);
stmt(#call_stmt{}=C, St) -> call_stmt(C, St);
stmt(#return_stmt{}=R, St) -> return_stmt(R, St);
stmt(#break_stmt{}=B, St) -> {B,false,St};
stmt(#block_stmt{}=B, St) -> block_stmt(B, St);
stmt(#while_stmt{}=W, St) -> while_stmt(W, St);
stmt(#repeat_stmt{}=R, St) -> repeat_stmt(R, St);
stmt(#if_stmt{}=If, St) -> if_stmt(If, St);
stmt(#nfor_stmt{}=For, St) -> numfor_stmt(For, St);
stmt(#gfor_stmt{}=For, St) -> genfor_stmt(For, St);
stmt(#local_assign_stmt{}=L, St) ->
    local_assign_stmt(L, St);
stmt(#local_fdef_stmt{}=L, St) ->
    local_fdef_stmt(L, St);
stmt(#expr_stmt{}=E, St) ->
    expr_stmt(E, St).

%% assign_stmt(Assign, State) -> {Assign,LocalFunc,State}.

assign_stmt(#assign_stmt{vars=Vs0,exps=Es0}=A, St0) ->
    {Vs1,Vlocf,St1} = assign_loop(Vs0, St0),
    {Es1,Elocf,St2} = explist(Es0, St1),
    Locf = Vlocf or Elocf,
    {A#assign_stmt{vars=Vs1,exps=Es1},Locf,St2}.

assign_loop([V0|Vs0], St0) ->
    {V1,Vlocf,St1} = var(V0, St0),
    {Vs1,Vslocf,St2} = assign_loop(Vs0, St1),
    Locf = Vlocf or Vslocf,
    {[V1|Vs1],Locf,St2};
assign_loop([], St) -> {[],false,St}.

var(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_first(Exp0, St0),
    {Rest1,Rlocf,St2} = var_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},Elocf or Rlocf,St2};
var(V, St) ->
    {V,false,St}.

var_rest(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_element(Exp0, St0),
    {Rest1,Rlocf,St2} = var_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},Elocf or Rlocf,St2};
var_rest(Exp, St) -> var_last(Exp, St).

var_last(#key{key=Exp0}=K, St0) ->
    {Exp1,Elocf,St1} = exp(Exp0, St0),
    {K#key{key=Exp1},Elocf,St1}.

%% call_stmt(Call, State) -> {Call,LocalFunc,State}.

call_stmt(#call_stmt{call=Exp0}=C, St0) ->
    {Exp1,Locf,St1} = exp(Exp0, St0),
    {C#call_stmt{call=Exp1},Locf,St1}.

%% return_stmt(Return, State) -> {Return,LocalFunc,State}.

return_stmt(#return_stmt{exps=Es0}=R, St0) ->
    {Es1,Locf,St1} = explist(Es0, St0),
    {R#return_stmt{exps=Es1},Locf,St1}.

%% block_stmt(Block, State) -> {Block,LocalFunc,State}.

block_stmt(#block_stmt{body=Ss0}=B, St0) ->
    {Ss1,Sslocf,St1} = stmts(Ss0, St0),
    {B#block_stmt{body=Ss1,locf=Sslocf},Sslocf,St1}.

%% do_block(Block, State) -> {Block,LocalFunc,State}.

do_block(#block{body=Ss0}=B, St0) ->
    {Ss1,Sslocf,St1} = stmts(Ss0, St0),
    {B#block{body=Ss1,locf=Sslocf},Sslocf,St1}.

%% while_stmt(While, State) -> {While,LocalFunc,State}.
%%  The test expression is done in the context of the surrounding
%%  block.

while_stmt(#while_stmt{exp=E0,body=B0}=W, St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    {W#while_stmt{exp=E1,body=B1},Elocf or Blocf,St2}.

%% repeat_stmt(Repeat, State) -> {Repeat,LocalFunc,State}.
%%  The test expression is done in the context of the repeat block.

repeat_stmt(#repeat_stmt{body=B0}=R, St0) ->
    {B1,Blocf,St1} = do_block(B0, St0),
    {R#repeat_stmt{body=B1},Blocf,St1}.

%% if_stmt(If, State) -> {If,LocalFunc,State}.
%%  The block info includes anything from the test expressions even
%%  though we keep them separate.

if_stmt(#if_stmt{tests=Ts0,else_block=E0}=If, St0) ->
    {Ts1,Tlocf,St1} = if_tests(Ts0, St0),
    {E1,Elocf,St2} = do_block(E0, St1),
    Locf = Tlocf or Elocf,
    {If#if_stmt{tests=Ts1,else_block=E1},Locf,St2}.

if_tests([{E0,B0}|Ts0], St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    {Ts1,Tslocf,St3} = if_tests(Ts0, St2),
    Locf = Elocf or Blocf or Tslocf,
    {[{E1,B1}|Ts1],Locf,St3};
if_tests([], St) -> {[],false,St}.

%% numfor_stmt(For, State) -> {For,LocalFunc,State}.

numfor_stmt(#nfor_stmt{init=I0,limit=L0,step=S0,body=B0}=For, St0) ->
    {[I1,L1,S1],Eslocf,St1} = explist([I0,L0,S0], St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    Locf = Eslocf or Blocf,
    {For#nfor_stmt{init=I1,limit=L1,step=S1,body=B1},Locf,St2}.

%% genfor_stmt(For, State) -> {For,LocalFunc,State}.

genfor_stmt(#gfor_stmt{gens=Gs0,body=B0}=For, St0) ->
    {Gs1,Glocf,St1} = explist(Gs0, St0),
    {B1,Blocf,St2} = do_block(B0, St1),
    Locf = Glocf or Blocf,
    {For#gfor_stmt{gens=Gs1,body=B1},Locf,St2}.

%% local_assign_stmt(Local, State) -> {Local,LocalFunc,State}.

local_assign_stmt(#local_assign_stmt{exps=Es0}=L, St0) ->
    {Es1,Eslocf,St1} = explist(Es0, St0),
    {L#local_assign_stmt{exps=Es1},Eslocf,St1}.

%% local_fdef_stmt(Local, State) -> {Local,LocalFunc,State}.

local_fdef_stmt(#local_fdef_stmt{func=F0}=L, St0) ->
    {F1,_,St1} = functiondef(F0, St0),		%Don't care what's in func
    {L#local_fdef_stmt{func=F1},true,St1}.

%% expr_stmt(Expr, State) -> {Expr,LocalFunc,State}.
%%  The expression pseudo statement. This will return a single value.

expr_stmt(#expr_stmt{exp=Exp0}=E, St0) ->
    {Exp1,Locf,St1} = exp(Exp0, St0),
    {E#expr_stmt{exp=Exp1},Locf,St1}.

%% explist(Exprs, State) -> {Exprs,LocalFunc,State}.
%% exp(Expr, State) -> {Expr,LocalFunc,State}.

explist([E0|Es0], St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {Es1,Eslocf,St2} = explist(Es0, St1),
    {[E1|Es1],Elocf or Eslocf,St2};
explist([], St) -> {[],false,St}.		%No expressions at all

exp(#lit{}=L, St) -> {L,false,St};		%Nothing to do
exp(#fdef{}=F0, St0) ->
    {F1,_,St1} = functiondef(F0, St0),		%Don't care what's in func
    {F1,true,St1};
exp(#op{args=Es0}=Op, St0) ->
    {Es1,Eslocf,St1} = explist(Es0, St0),
    {Op#op{args=Es1},Eslocf,St1};
exp(#tabcon{fields=Fs0}=T, St0) ->
    {Fs1,Tlocf,St1} = tableconstructor(Fs0, St0),
    {T#tabcon{fields=Fs1},Tlocf,St1};
exp(E, St) ->
    prefixexp(E, St).

prefixexp(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_first(Exp0, St0),
    {Rest1,Rlocf,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},Elocf or Rlocf,St2};
prefixexp(Exp, St) -> prefixexp_first(Exp, St).

prefixexp_first(#single{exp=E0}=S, St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {S#single{exp=E1},Elocf,St1};
prefixexp_first(V, St) ->
    {V,false,St}.

prefixexp_rest(#dot{exp=Exp0,rest=Rest0}=D, St0) ->
    {Exp1,Elocf,St1} = prefixexp_element(Exp0, St0),
    {Rest1,Rlocf,St2} = prefixexp_rest(Rest0, St1),
    {D#dot{exp=Exp1,rest=Rest1},Elocf or Rlocf,St2};
prefixexp_rest(Exp, St) -> prefixexp_element(Exp, St).

prefixexp_element(#key{key=E0}=K, St0) ->
    {E1,Elocf,St1} = exp(E0, St0),
    {K#key{key=E1},Elocf,St1};
prefixexp_element(#fcall{args=As0}=F, St0) ->
    {As1,Aslocf,St1} = explist(As0, St0),
    {F#fcall{args=As1},Aslocf,St1};
prefixexp_element(#mcall{args=As0}=M, St0) ->
    {As1,Aslocf,St1} = explist(As0, St0),
    {M#mcall{args=As1},Aslocf,St1}.

%% functiondef(Func, State) -> {Func,LocalFunc,State}.
%%  We return if there are any internal function definitions within
%%  the function.

functiondef(#fdef{body=Ss0}=F, St0) ->
    {Ss1,Sslocf,St1} = stmts(Ss0, St0),
    {F#fdef{body=Ss1,locf=Sslocf},Sslocf,St1}.

%% tableconstructor(Fields, State) -> {Fields,LocalFunc,State}.

tableconstructor(Fs0, St0) ->
    Fun = fun (#efield{val=V0}=F, {Locf,S0}) ->
		  {V1,Vlocf,S1} = exp(V0, S0),
		  {F#efield{val=V1},{Locf or Vlocf,S1}};
	      (#kfield{key=K0,val=V0}=F, {Locf,S0}) ->
		  {K1,Klocf,S1} = exp(K0, S0),
		  {V1,Vlocf,S2} = exp(V0, S1),
		  {F#kfield{key=K1,val=V1},{Locf or Klocf or Vlocf,S2}}
	  end,
    {Fs1,{Locf,St1}} = lists:mapfoldl(Fun, {false,St0}, Fs0),
    {Fs1,Locf,St1}.
