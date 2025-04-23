%% Copyright (c) 2019 Robert Virding
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

%% File    : luerl_lint.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.3 error checker for Luerl.

%% There isn't much checking which can be done here as most is allowed
%% and generates run-time errors or strangeness. So far all we can
%% check is the legal use of varargs ... and warn if assignments have
%% different number of variables and expressions.

-module(luerl_comp_lint).

-include("luerl.hrl").
-include("luerl_comp.hrl").

?MODULEDOC(false).

-export([chunk/2,format_error/1]).

-record(lint, {opts=[],                         %Options
               pars=[],                         %Local function parameters
               errors=[],                       %Errors
               warnings=[]                      %Warnings
              }).

%% format_error(Error) -> String.

format_error(illegal_varargs) ->
    "cannot use '...' outside a vararg function";
format_error(assign_mismatch) ->
    "assign mismatch variables and expressions".

%% chunk(Code, Compinfo) -> {ok,Warnings} | {error,Errors,Warnings}.

chunk(Code, #cinfo{opts=Opts}=_Ci) ->
    St0 = #lint{opts=Opts},
    St1 = functiondef(Code, St0),
    return_status(St1).

return_status(#lint{errors=[]}=St) ->
    {ok,St#lint.warnings};
return_status(#lint{errors=Es,warnings=Ws}) ->
    {error,Es,Ws}.

%% stmts(Stmts, State) -> State.

stmts([S|Ss], St0) ->
    St1 = stmt(S, St0),
    stmts(Ss, St1);
stmts([], St) -> St.

%% stmt(Stmt, State) -> State.

stmt(#assign_stmt{}=A, St) -> assign_stmt(A, St);
stmt(#call_stmt{}=C, St) -> call_stmt(C, St);
stmt(#return_stmt{}=R, St) -> return_stmt(R, St);
stmt(#break_stmt{}, St) -> St;
stmt(#block_stmt{}=B, St) -> block_stmt(B, St);
stmt(#while_stmt{}=W, St) -> while_stmt(W, St);
stmt(#repeat_stmt{}=R, St) -> repeat_stmt(R, St);
stmt(#if_stmt{}=If, St) -> if_stmt(If, St);
stmt(#nfor_stmt{}=For, St) -> numfor_stmt(For, St);
stmt(#gfor_stmt{}=For, St) -> genfor_stmt(For, St);
stmt(#local_assign_stmt{}=For, St) -> 
    local_assign_stmt(For, St);
stmt(#local_fdef_stmt{}=For, St) -> 
    local_fdef_stmt(For, St);
stmt(#expr_stmt{}=E, St) ->
    expr_stmt(E, St).

%% assign_stmt(Assign, State) -> State.
%% call_stmt(Call, State) -> State.
%% return_stmt(Return, State) -> State.
%% block_stmt(Block, State) -> State.
%% while_stmt(While, State) -> State.
%% repeat_stmt(Repeat, State) -> State.
%% if_stmt(If, State) -> State.
%% numfor_stmt(Numfor, State) -> State.
%% genfor_stmt(Genfor, State) -> State.
%% local_assign_stmt(Assign, State) -> State.
%% local_fdef_stmt(Fdef, State) -> State.
%% expr_stmt(Expr, State) -> State.

assign_stmt(#assign_stmt{vars=Vs,exps=Es}, St0) ->
    %% Must work more on this to get it right.
    %% St1 = ?IF(length(Vs) =/= length(Es),
    %%           assign_mismatch_warning(Anno, St0), St0),
    St1 = St0,
    St2 = lists:foldl(fun (V, S) -> assign_var(V, S) end, St1, Vs),
    explist(Es, St2).

assign_var(#dot{exp=Exp,rest=Rest}, St0) ->
    St1 = prefixexp_first(Exp, St0),
    assign_var_rest(Rest, St1);
assign_var(#var{l=Anno,name='...'}, St) ->
    %% Not allowed to bind ... .
    illegal_varargs_error(Anno, St);
assign_var(_Var, St) -> St.

assign_var_rest(#dot{exp=Exp,rest=Rest}, St0) ->
    St1 = prefixexp_element(Exp, St0),
    assign_var_rest(Rest, St1);
assign_var_rest(Exp, St) -> assign_var_last(Exp, St).

assign_var_last(#key{key=Exp}, St) ->
    exp(Exp, St).

call_stmt(#call_stmt{call=Exp}, St) ->
    exp(Exp, St).

return_stmt(#return_stmt{exps=Es}, St) ->
    explist(Es, St).

block_stmt(#block_stmt{body=Ss}, St) ->
    stmts(Ss, St).

while_stmt(#while_stmt{exp=Exp,body=Ss}, St0) ->
    St1 = exp(Exp, St0),
    block(Ss, St1).

repeat_stmt(#repeat_stmt{body=Ss}, St) ->
    block(Ss, St).

if_stmt(#if_stmt{tests=Ts,else_block=Else}, St0) ->
    Fun = fun ({E,B}, S0) ->
                  S1 = exp(E, S0),
                  block(B, S1)
          end,
    St1 = lists:foldl(Fun, St0, Ts),
    block(Else, St1).

numfor_stmt(#nfor_stmt{init=I,limit=L,step=S,body=B}, St0) ->
    St1 = explist([I,L,S], St0),
    block(B, St1).

genfor_stmt(#gfor_stmt{gens=Gs,body=B}, St0) ->
    St1 = explist(Gs, St0),
    block(B, St1).

local_assign_stmt(#local_assign_stmt{exps=Es}, St0) ->
    %% Must work more on this to get it right.
    %% St1 = ?IF(length(Vs) =/= length(Es),
    %%           assign_mismatch_warning(Anno, St0), St0),
    St1 = St0,
    explist(Es, St1).

local_fdef_stmt(#local_fdef_stmt{func=F}, St) ->
    functiondef(F, St).

expr_stmt(#expr_stmt{exp=Exp}, St) ->
    exp(Exp, St).

%% block(Block, State) -> State.

block(#block{body=Ss}, St) ->
    stmts(Ss, St).

%% explist(Exprs, State) -> State.
%% exp(Expr, State) -> State.
%% prefixexp(Expr, State) -> State.

explist(Es, St) ->
    lists:foldl(fun (E, S) -> exp(E, S) end, St, Es).

exp(#lit{}, St) -> St;
exp(#fdef{}=F, St) -> functiondef(F, St);
exp(#op{args=Es}, St) ->
    explist(Es, St);
exp(#tabcon{fields=Fs}, St) ->
    tableconstructor(Fs, St);
exp(E, St) ->
    prefixexp(E, St).

prefixexp(#dot{exp=Exp,rest=Rest}, St0) ->
    St1 = prefixexp_first(Exp, St0),
    prefixexp_rest(Rest, St1);
prefixexp(Exp, St) -> prefixexp_first(Exp, St).

prefixexp_first(#single{exp=Exp}, St) ->
    exp(Exp, St);
prefixexp_first(#var{}=V, St) ->
    var(V, St).

prefixexp_rest(#dot{exp=Exp,rest=Rest}, St0) ->
    St1 = prefixexp_element(Exp, St0),
    prefixexp_rest(Rest, St1);
prefixexp_rest(Exp, St) -> prefixexp_element(Exp, St).

prefixexp_element(#key{key=Exp}, St) ->
    exp(Exp, St);
prefixexp_element(#fcall{args=Es}, St) ->
    explist(Es, St);
prefixexp_element(#mcall{meth=Lit,args=Es}, St0) ->
    St1 = lit(Lit, St0),
    explist(Es, St1).

%% functiondef(FuncDef, State) -> State.

functiondef(#fdef{pars=Ps,body=Ss}, #lint{pars=Pars}=St0) ->
    St1 = St0#lint{pars=Ps},                    %Use current parameters
    St2 = stmts(Ss, St1),
    St2#lint{pars=Pars}.                        %Reset previous parameters

%% tableconstructor(Fields, State) -> State.

tableconstructor(Fs, St) ->
    Fun = fun (#efield{val=Exp}, S) -> exp(Exp, S);
              (#kfield{key=Key,val=Val}, S0) ->
                  S1 = exp(Key, S0),
                  exp(Val, S1)
          end,
    lists:foldl(Fun, St, Fs).

%% var(Var, State) -> State.

var(#var{l=Anno,name='...'}, St) ->
    case lists:keymember('...', #var.name, St#lint.pars) of
        true -> St;
        false ->
            illegal_varargs_error(Anno, St)
    end;
var(_Var, St) -> St.

%% lit(Lit, State) -> State.

lit(#lit{l=Anno,val='...'}, St) ->
    case lists:keymember('...', #var.name, St#lint.pars) of
        true -> St;
        false ->
            illegal_varargs_error(Anno, St)
    end;
lit(_Lit, St) -> St.

%% add_error(Annotation, Error, State) -> State.
%% add_warning(Annotation, Warning, State) -> State.
%%  Add errors/warnings to the state.

add_error(Anno, E, #lint{errors=Errs}=St) ->
    L = luerl_anno:line(Anno),
    St#lint{errors=Errs ++ [{L,?MODULE,E}]}.

%% add_warning(Anno, W, #lint{warnings=Warns}=St) ->
%%     L = luerl_anno:line(Anno),
%%     St#lint{warnings=Warns ++ [{L,?MODULE,W}]}.

illegal_varargs_error(Anno, St) ->
    add_error(Anno, illegal_varargs, St).

%% assign_mismatch_warning(Anno, St) ->
%%     add_warning(Anno, assign_mismatch, St).
