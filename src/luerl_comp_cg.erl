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

%% File    : luerl_comp_cg.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does code generation in the compiler.

-module(luerl_comp_cg).

-include("luerl.hrl").
-include("luerl_comp.hrl").
-include("luerl_instrs.hrl").

-export([chunk/2]).

-import(ordsets, [add_element/2,is_element/2,union/1,union/2,
		  subtract/2,intersection/2,new/0]).

%% chunk(St0, Opts) -> {ok,St0}.
%%  Return a list of instructions to define the chunk function.

chunk(#code{code=C0}=Code, Opts) ->
    Info = luerl_comp:token_info_new("Not File, chunk", -1, "Chunk"),
    {InternalStatements,nul} = functiondef(C0, nul, Info),		%No local state
    luerl_comp:debug_print(Opts, "cg: ~p\n", [InternalStatements]),
{ok,Code#code{code= InternalStatements}}.

%% set_var(Var) -> SetIs.
%% get_var(Var) -> GetIs.
%%  These return a LIST of instructions for setting/getting variable.

set_var(#lvar{depth =D, index =I}, Info) -> [?STORE_LVAR_INFO(D, I)];
set_var(#evar{depth =D, index =I}, Info) -> [?STORE_EVAR_INFO(D, I)];
set_var(#gvar{name =N}, Info) -> [?STORE_GVAR_INFO(N)].

get_var(#lvar{depth =D, index =I}, Info) -> [?PUSH_LVAR_INFO(D, I)];
get_var(#evar{depth =D, index =I}, Info) -> [?PUSH_EVAR_INFO(D, I)];
get_var(#gvar{name =N}, Info) -> [?PUSH_GVAR_INFO(N)].

info_create_or_use_current(Statement) ->
    case element(2, Statement) of
        TokenInfo = #info_structure{} ->
            TokenInfo;
        Line -> luerl_comp:token_info_new("Unknown Source File",
            Line,
            "unknown token",
            ?TOKEN_POSITION_IN_LINE_DEFAULT)
    end.

stmts([StatementReceived|Ss0], State) ->
    {StatementsWithInfo,St1} = stmt(StatementReceived, nul, State),
    %% io:format("ss1: ~p\n", [{Loc0,Free0,Used0}]),
    {Ss1,St2} = stmts(Ss0, St1), %% RECURSIVE CALL
    {StatementsWithInfo ++ Ss1,St2};
stmts([], St) -> {[],St}.

%% stmt(Stmt, LocalVars, State) -> {Istmt,State}.

stmt(#assign_stmt{}=A,       _, St) -> assign_stmt(A, St);
stmt(#call_stmt{}=C,         _, St) -> call_stmt(C, St);
stmt(#return_stmt{}=R,       _, St) -> return_stmt(R, St);
stmt(#break_stmt{}=Break,    _, St) -> Info= info_create_or_use_current(Break),  {[?BREAK_INFO],St};
stmt(#block_stmt{}=B,        _, St) -> block_stmt(B, St);
stmt(#while_stmt{}=W,        _, St) -> while_stmt(W, St);
stmt(#repeat_stmt{}=R,       _, St) -> repeat_stmt(R, St);
stmt(#if_stmt{}=I,           _, St) -> if_stmt(I, St);
stmt(#nfor_stmt{}=F,         _, St) -> numfor_stmt(F, St);
stmt(#gfor_stmt{}=F,         _, St) -> genfor_stmt(F, St);

stmt(#local_assign_stmt{}=L, _, St) -> local_assign_stmt(L, St);
stmt(#local_fdef_stmt{}=L,   _, St) -> local_fdef_stmt(L, St);
stmt(#expr_stmt{}=E,         _, St) -> expr_stmt(E, St).

%% assign_stmt(Assign, State) -> {AssignIs,State}.
%%  We must evaluate all expressions, even the unneeded ones.

assign_stmt(#assign_stmt{variable_statement=Vs,expressions=Es,info=Info}, St) ->
    assign_loop(Vs, Es, St, Info).

%% assign_loop(Vars, Exps, State) -> {Iassigns,State}.
%%  Must be careful with pushing and popping values here. Make sure
%%  all non-last values are singleton.
%%
%%  This could most likely be folded together with assign_local_loop/3.


%% TODO: I am here,  Info variable refactor into the leaves of the graph.
%% TODO: exp(), var(), other funcs: use Info when they create internal instructions

assign_loop([V], [E], St0, Info) ->			%Remove unnecessary ?PUSH_VALS_EMPTY
    {Ie,St1} = exp(E, single, St0, Info),		%Last argument to one variable
    {Iv,St2} = var(V, St1, Info),
    {Ie ++ Iv,St2};
assign_loop([V|Vs], [E], St0, Info) ->
    {Ie,St1} = exp(E, multiple, St0, Info),		%Last argument to rest of vars
    {Ias,St2} = assign_loop_var(Vs, 1, St1, Info),
    {Iv,St3} = var(V, St2, Info),
    {Ie ++ Ias ++ Iv,St3};
assign_loop([V|Vs], [E|Es], St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),		%Not last argument!
    {Ias,St2} = assign_loop(Vs, Es, St1, Info),
    {Iv,St3} = var(V, St2, Info),
    {Ie ++ Ias ++ Iv,St3};
assign_loop([], Es, St, Info) ->
    assign_loop_exp(Es, St, Info).

assign_loop_var([V|Vs], Vc, St0, Info) ->
    {Ias,St1} = assign_loop_var(Vs, Vc+1, St0, Info),
    {Iv,St2} = var(V, St1, Info),
    {Ias ++ Iv,St2};
assign_loop_var([], Vc, St, Info) ->
    {[?PUSH_VALS_INFO(Vc)],St}.

assign_loop_exp([E|Es], St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),		%It will be dropped anyway
    {Ias,St2} = assign_loop_exp(Es, St1, Info),
    {Ie ++ Ias ++ [?POP_INFO],St2};			%Pop unneeded value off stack
assign_loop_exp([], St, _Info) -> {[],St}.

var(#dot{e=Exp,r=Rest}, St0, Info) ->
    {Ie,St1} = prefixexp_first(Exp, single, St0, Info),
    {Ir,St2} = var_rest(Rest, St1, Info),
    {Ie ++ Ir,St2};
var(V, St, Info) ->
    {set_var(V, Info),St}.

var_rest(#dot{e=Exp,r=Rest}, St0, Info) ->
    {Ie,St1} = prefixexp_element(Exp, single, St0, Info),
    {Ir,St2} = var_rest(Rest, St1, Info),
    {Ie ++ Ir,St2};
var_rest(Exp, St, Info) -> var_last(Exp, St, Info).

var_last(#key{key =#lit{v=K}}, St, Info) ->
    {[?SET_LIT_KEY_INFO(K)],St};			%[?PUSH_LIT_INFO_EMPTY(K),?SET_KEY_INFO_EMPTY]
var_last(#key{key =Exp}, St0, Info) ->
    {Ie,St1} = exp(Exp, single, St0, Info),
    {Ie ++ [?SET_KEY_INFO],St1}.

%% call_stmt(Call, State) -> {CallIs,State}.
%%  Must pop function return value list from stack.

call_stmt(#call_stmt{call=Exp,info=Info}, St0) ->
    {Ie,St1} = exp(Exp, multiple, St0, Info),
    {Ie ++ [?POP_INFO],St1}.

%% return_stmt(Return, State) -> {ReturnIs,State}.
%%  Can ignore any value left on stack here.

return_stmt(#return_stmt{expressions =Es,info=Info}, St0) ->
    {Ies,St1} = explist(Es, multiple, St0, Info),
    {Ies ++ [?RETURN_INFO(length(Es))],St1}.

%% block_stmt(Block, State) -> {BlockIs,State}.

block_stmt(#block_stmt{block_statement =Ss, local_frame_size =Lsz, environment_frame_size =Esz,info=Info}, St0) ->
    {Instructions,St1} = stmts(Ss, St0), %% RECURSIVE
    {[?BLOCK_INFO(Lsz, Esz, Instructions)],St1}.

%% do_block(Block, State) -> {Block,State}.
%%  Do_block never returns external new variables. Fits into stmt().

do_block(#block{sub_blocks =Ss, local_frame_size =Lsz, environment_frame_size =Esz,info=Info}, St0) ->
    {InternalStatements,St1} = stmts(Ss, St0),   %% RECURSIVE CALL
    {[?BLOCK_INFO(Lsz, Esz, InternalStatements)],St1}.

%% while_stmt(While, State) -> {WhileIs,State}.

while_stmt(#while_stmt{expression =E, block =B,info=Info}, St0) ->
    {Ie,St1} = exp(E, single, St0, Info),
    {Ib,St2} = do_block(B, St1),
    {[?WHILE_INFO(Ie, Ib)],St2}.

%% repeat_stmt(Repeat, State) -> {RepeatIs,State}.

repeat_stmt(#repeat_stmt{block =B,info=Info}, St0) ->
    {Ib,St1} = do_block(B, St0),
    {[?REPEAT_INFO(Ib)],St1}.

%% if_stmt(If, State) -> {If,State}.

if_stmt(#if_stmt{tests=Tests,else=Else,info=Info}, St) ->
    if_tests(Tests, Else, St, Info).

if_tests([{E,B}], #block{sub_blocks =[]}, St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),
    {Ib,St2} = do_block(B, St1),
    {Ie ++ [?IF_TRUE_INFO(Ib)],St2};
if_tests([{E,B}|Ts], Else, St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),
    {Ib,St2} = do_block(B, St1),
    {Its,St3} = if_tests(Ts, Else, St2, Info),
    {Ie ++ [?IF_INFO(Ib, Its)],St3};
if_tests([], Else, St0, _Info) ->
    {Ie,St1} = do_block(Else, St0),
    {Ie,St1}.

%% numfor_stmt(For, State) -> {ForIs,State}.

numfor_stmt(#nfor_stmt{v=V,init=I,limit=Limit,step=S, block =B,info=Info}, St0) ->
    {Ies,St1} = explist([I, Limit,S], single, St0, Info),
    {Ib,St2} = do_block(B, St1),
    [?BLOCK_INFO(Lsz, Esz, Is)] = Ib,
    ForBlock = [?BLOCK_INFO(Lsz, Esz, set_var(V, Info) ++ Is)],
    {Ies ++ [?NFOR_INFO(V,ForBlock)],St2}.

%% %% An experiment to put the block *outside* the for loop.
%% numfor_stmt(#nfor_stmt{v=V,init=I,limit=L,step=S,b=B}, St0) ->
%%     {Ies,St1} = explist([I,L,S], single, St0),
%%     {Ib,St2} = do_block(B, St1),
%%     [?BLOCK_INFO_EMPTY(Lsz, Esz, Is)] = Ib,
%%     ForBlock = [?BLOCK_INFO_EMPTY(Lsz, Esz, [?NFOR_INFO_EMPTY(V,set_var(V) ++ Is)])],
%%     {Ies ++ ForBlock,St2}.

%% genfor_stmt(For, State) -> {ForIs,State}.

genfor_stmt(#gfor_stmt{vs=[V|Vs],gens=Gs, block =B, info=Info}, St0) ->
    {Igs,St1} = explist(Gs, multiple, St0, Info),
    {Ias,St2} = assign_local_loop_var(Vs, 1, St1, Info),
    {Ib,St3} = do_block(B, St2),
    [?BLOCK_INFO(Lsz, Esz, Is)] = Ib,
    ForBlock = [?BLOCK_INFO(Lsz, Esz, Ias ++ set_var(V, Info) ++ Is)],
    {Igs ++ [?POP_VALS_INFO(length(Gs))] ++ [?GFOR_INFO(Vs,ForBlock)],St3}.

%% local_assign_stmt(Local, State) -> {Ilocal,State}.
%%  We must evaluate all expressions, even the unneeded ones.

local_assign_stmt(#local_assign_stmt{vs=Vs, expressions =Es,info=Info}, St) ->
    assign_local(Vs, Es, St, Info).

assign_local([V|Vs], [], St0, Info) ->
    {Ias,St1} = assign_local_loop_var(Vs, 1, St0, Info),
    {[?PUSH_LIT_INFO([])] ++ Ias ++ set_var(V, Info),St1};
assign_local(Vs, Es, St, Info) ->
    assign_local_loop(Vs, Es, St, Info).

%% assign_local_loop(Vars, Exps, State) -> {Iassigns,State}.
%%  Must be careful with pushing and popping values here. Make sure
%%  all non-last values are singleton.
%%
%%  This could most likely be folded together with assign_loop/4.

assign_local_loop([V], [E], St0, Info) ->		%Remove unnecessary ?PUSH_VALS_EMPTY
    {Ie,St1} = exp(E, single, St0, Info),		%Last argument to one variable!
    {Ie ++ set_var(V, Info),St1};
assign_local_loop([V|Vs], [E], St0, Info) ->
    {Ie,St1} = exp(E, multiple, St0, Info),		%Last argument to many vars!
    {Ias,St2} = assign_local_loop_var(Vs, 1, St1, Info),
    {Ie ++ Ias ++ set_var(V, Info),St2};
assign_local_loop([V|Vs], [E|Es], St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),		%Not last argument!
    {Ias,St2} = assign_local_loop(Vs, Es, St1, Info),
    {Ie ++ Ias ++ set_var(V, Info),St2};
assign_local_loop([], Es, St, Info) ->
    assign_local_loop_exp(Es, St, Info).

%% assign_local_loop_var([V|Vs], Vc, St0) ->
%%     {Ias,St1} = assign_local_loop_var(Vs, Vc+1, St0),
%%     {Ias ++ [?PUSH_LIT_INFO_EMPTY(nil)] ++ set_var(V),St1};
%% assign_local_loop_var([], Vc, St) ->
%%     {[],St}.
assign_local_loop_var([V|Vs], Vc, St0, Info) ->
    {Ias,St1} = assign_local_loop_var(Vs, Vc+1, St0, Info),
    {Ias ++ set_var(V, Info),St1};
assign_local_loop_var([], Vc, St, Info) ->
    {[?PUSH_VALS_INFO(Vc)],St}.

assign_local_loop_exp([E|Es], St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),		%It will be dropped anyway
    {Ias,St2} = assign_local_loop_exp(Es, St1, Info),
    {Ie ++ Ias ++ [?POP_INFO],St2};			%Pop value off stack
assign_local_loop_exp([], St, _Info) -> {[],St}.

%% local_fdef_stmt(Local, State) -> {ILocal,State}.

local_fdef_stmt(#local_fdef_stmt{v=V,f=F,info=Info}, St0) ->
    {If,St1} = functiondef(F, St0, Info),
    {If ++ set_var(V, Info),St1}.

%% expr_stmt(Expr, State) -> {ExprIs,State}.
%%  The expression pseudo statement. This will return a single value
%%  which we leave on the stack.

expr_stmt(#expr_stmt{exp=Exp,info=Info}, St0) ->
    {Ie,St1} = exp(Exp, single, St0, Info),
    {Ie,St1}.

%% explist(Exprs, Values, State) -> {Instrs,State}.
%% exp(Expr, Values, State) -> {Instrs,State}.
%%  Values determines if we are to only return the first value of a
%%  list of values. Values multiple makes us a return a list!

explist([E], S, St, Info) -> exp(E, S, St, Info);		%Append values to output?
explist([E|Es], S, St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),
    {Ies,St2} = explist(Es, S, St1, Info),
    {Ie ++ Ies,St2};
explist([], _, St, _Info) -> {[],St}.			%No expressions at all

exp(#lit{v=Literal}, S, St, Info) ->
    Is = [?PUSH_LIT_INFO(Literal)],
    {multiple_values(S, Is, Info),St};
exp(#fdef{}=F, S, St0, Info) ->
    {If,St1} = functiondef(F, St0, Info),
    {multiple_values(S, If, Info), St1};
exp(#op{op='and', arguments =[A1,A2]}, S, St0, Info) ->
    {Ia1,St1} = exp(A1, S, St0, Info),
    {Ia2,St2} = exp(A2, S, St1, Info),
    {Ia1 ++ [?AND_THEN_INFO(Ia2)],St2};		%Must handle single/multiple
exp(#op{op='or', arguments =[A1,A2]}, S, St0, Info) ->
    {Ia1,St1} = exp(A1, S, St0, Info),
    {Ia2,St2} = exp(A2, S, St1, Info),
    {Ia1 ++ [?OR_ELSE_INFO(Ia2)],St2};		%Must handle single/multiple
exp(#op{op=Op, arguments = Args}, S, St0, Info) ->
    {Ias,St1} = explist(Args, single, St0, Info),
    Iop = Ias ++ [?OP_INFO(Op,length(Args))],
    {multiple_values(S, Iop, Info),St1};
exp(#table_constructor{fields =Fs}, S, St0, Info) ->
    {Its,Fc,I,St1} = tableconstructor(Fs, St0, Info),
    {Its ++ multiple_values(S, [?BUILD_TAB_INFO(Fc,I)], Info),St1};
exp(#lvar{name = <<"...">>}=V, S, St, Info) ->		%Can be either local or frame
    {single_value(S, get_var(V, Info), Info),St};
exp(#evar{name = <<"...">>}=V, S, St, Info) ->
    {single_value(S, get_var(V, Info), Info),St};
exp(E, S, St, Info) ->
    prefixexp(E, S, St, Info).

%% single_value(Values, Instrs) -> Instrs.
%% multiple_values(Values, Instrs) -> Instrs.
%%  Ensure either single value or multiple value.

single_value(single, Is, Info) -> Is ++ [?SINGLE_INFO];
single_value(multiple, Is, _Info) -> Is.

multiple_values(single, Is, _Info) -> Is;
multiple_values(multiple, Is, Info) -> Is ++ [?MULTIPLE_INFO].

%% prefixexp(Expr, Values, State) -> {Instrs,State}.
%% prefixexp_rest(Expr, Values, State) -> {Instrs,State}.
%% prefixexp_first(Expr, Values, State) -> {Instrs,State}.
%% prefixexp_element(Expr, Values, State) -> {Instrs,State}.
%%  Single determines if we are to only return the first value of a
%%  list of values. Single false makes us a return a list!

prefixexp(#dot{e=Exp,r=Rest}, S, St0, Info) ->
    {Ie,St1} = prefixexp_first(Exp, single, St0, Info),
    {Ir,St2} = prefixexp_rest(Rest, S, St1, Info),
    {Ie ++ Ir,St2};
prefixexp(Exp, S, St, Info) -> prefixexp_first(Exp, S, St, Info).

prefixexp_first(#single{e=E}, S, St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),		%Will make it single
    {multiple_values(S, Ie, Info),St1};
prefixexp_first(Var, S, St, Info) ->
    {multiple_values(S, get_var(Var, Info), Info),St}.

prefixexp_rest(#dot{e=Exp,r=Rest}, S, St0, Info) ->
    {Ie,St1} = prefixexp_element(Exp, single, St0, Info),
    {Ir,St2} = prefixexp_rest(Rest, S, St1, Info),
    {Ie ++ Ir,St2};
prefixexp_rest(Exp, S, St, Info) -> prefixexp_element(Exp, S, St, Info).

prefixexp_element(#key{key =#lit{v=K}}, S, St, Info) ->
    {multiple_values(S, [?GET_LIT_KEY_INFO(K)], Info),St};
prefixexp_element(#key{key =E}, S, St0, Info) ->
    {Ie,St1} = exp(E, single, St0, Info),
    {Ie ++ multiple_values(S, [?GET_KEY_INFO], Info),St1};
prefixexp_element(#fcall{as=[]}, S, St, Info) ->
    Ifs = [?FCALL_INFO(0)],
    {single_value(S, Ifs, Info),St};			%Function call returns list
prefixexp_element(#fcall{as=As}, S, St0, Info) ->
    {Ias,St1} = explist(As, multiple, St0, Info),
    Ifs = Ias ++ [?FCALL_INFO(length(As))],
    {single_value(S, Ifs, Info),St1};			%Function call returns list
prefixexp_element(#method_call{m=#lit{v=K},as=[]}, S, St, Info) ->
    Ims = [?MCALL_INFO(K, 0)],
    {single_value(S, Ims, Info),St};			%Method call returns list
prefixexp_element(#method_call{m=#lit{v=K},as=As}, S, St0, Info) ->
    {Ias,St1} = explist(As, multiple, St0, Info),
    Ims = Ias ++ [?MCALL_INFO(K, length(As))],
    {single_value(S, Ims, Info),St1}.			%Method call returns list

%% functiondef(Func, State) -> {Func,State}.
%%  This will return a single value which we leave on the stack.

functiondef(#fdef{func_parameters = Parameters0, statements = Statements, local_frame_size = LocalFrameSize, environment_frame_size = EnvironmentFrameSize}, St0, Info) ->
    FuncParams = func_pars(Parameters0),
    {StmtsInstructions,St1} = stmts(Statements, St0), %% RECURSIVE CALL
    {[?PUSH_FDEF_INFO(LocalFrameSize, EnvironmentFrameSize, FuncParams, StmtsInstructions)],St1}.

func_pars([#evar{name = <<"...">>, index =I}]) -> -I;	%Tail is index for varargs
func_pars([#lvar{name = <<"...">>, index =I}]) -> I;
func_pars([#evar{index =I}|Ps]) -> [-I|func_pars(Ps)];
func_pars([#lvar{index =I}|Ps]) -> [I|func_pars(Ps)];
func_pars([]) -> [].				%No varargs

%% tableconstructor(Fields, State) -> {Ifields,FieldCount,Index,State}.
%%  FieldCount is how many Key/Value pairs are on the stack, Index is
%%  the index of the next value in the last value pushed. Make sure
%%  that the last value is a multiple.

tableconstructor(Fields, St0, Info) ->
    %% FIXME: WHY WE USE FLOAT AS TABLE INDEX??? INSTEAD OF INTEGER?
    {InternalFields, FieldCount, Index,St1} = tc_fields(Fields, 0.0, St0, Info),
    {InternalFields, FieldCount, Index,St1}.

tc_fields([#efield{value =V}], IndexElemNow, St0, Info) ->
    IndexElemNext = IndexElemNow + 1.0,				%Index of next element
    {InternalFields,St1} = exp(V, multiple, St0, Info),
    {InternalFields,0, IndexElemNext,St1};
tc_fields([#efield{value =V}| Fields], IndexElemNow, St0, Info) ->
    IndexElemNext = IndexElemNow + 1.0,				%Index of next element
    {InternalValue,St1} = exp(V, single, St0, Info),
    {InternalFields, FieldCount, IndexAnother,St2} = tc_fields(Fields, IndexElemNext, St1, Info),
    {[?PUSH_LIT_INFO(IndexElemNext)] ++ InternalValue ++ InternalFields, FieldCount +1, IndexAnother,St2};
tc_fields([#kfield{key =K, value =V}| Fields], IndexElemNow, St0, Info) ->
    {InternalKey,St1} = exp(K, single, St0, Info),
    {InternalValue,St2} = exp(V, single, St1, Info),
    {InternalFields, FieldCount, Index1,St3} = tc_fields(Fields, IndexElemNow, St2, Info),
    {InternalKey ++ InternalValue ++ InternalFields, FieldCount +1, Index1,St3};
tc_fields([], _, St, Info) -> {[?PUSH_LIT_INFO([])],0,1.0,St}.
