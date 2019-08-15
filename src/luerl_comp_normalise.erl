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

%% File    : luerl_comp_normalise.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.3 compiler for Luerl.

%% Here we normalise the code and convert it to an internal form. 

-module(luerl_comp_normalise).

-include("luerl.hrl").
-include("luerl_comp.hrl").

-export([chunk/2]).

%% chunk(Code, Options) -> {ok,Code} | {error,Reason}.

chunk(#code{code=Code0,cst=Cst0}, Opts) ->
    %% The chunk is a function.
    {Code1,Cst1} = functiondef(Code0, Cst0),
    luerl_comp:debug_print(Opts, "cn: ~p\n", [Code1]),
    {ok,#code{code=Code1,cst=Cst1}}.

%% stmts([{local,L,{functiondef,_,Name,_,_}=F}|Ss], St) ->
%%     %% Need to split this to handle recursive definitions.
%%     stmts([{local,L,{assign,L,[Name],[{nil,L}]}},F|Ss], St);
stmts([{';',_}|Ss], St) -> stmts(Ss, St);	%No-op so we drop it
stmts([S0|Ss0], St0) ->
    {S1,St1} = stmt(S0, St0),
    {Ss1,St2} = stmts(Ss0, St1),
    {[S1|Ss1],St2};
stmts([], St) -> {[],St}.

%% stmt(Statement, State) -> {CStat,State}.
%%  Do a statement. The ';' statement will caught and removed in stmts/2.

stmt({assign,Line,Vs,Es}, St) ->
    assign_stmt(Line, Vs, Es, St);
stmt({return,Line,Es}, St) ->
    return_stmt(Line, Es, St);
stmt({break,Info}, St) ->				%Interesting
    {#break_stmt{info=Info},St};
stmt({block,Line,B}, St) ->
    block_stmt(Line, B, St);
stmt({while,Line,Exp,B}, St) ->
    while_stmt(Line, Exp, B, St);
stmt({repeat,Line,B,Exp}, St) ->
    repeat_stmt(Line, B, Exp, St);
stmt({'if',Line,Tests,Else}, St) ->
    if_stmt(Line, Tests, Else, St);
stmt({for,Line,V,I,L,B}, St) ->			%Default step of 1.0
    numfor_stmt(Line, V, I, L, {'NUMERAL',Line,1.0}, B, St);
stmt({for,Line,V,I,L,S,B}, St) ->
    numfor_stmt(Line, V, I, L, S, B, St);
stmt({for,Line,Ns,Gs,B}, St) ->
    genfor_stmt(Line, Ns, Gs, B, St);
stmt({functiondef,Line,Fname,Ps,B}, St) ->
    fdef_stmt(Line, Fname, Ps, B, St);
stmt({local,Line,Local}, St) ->
    local_stmt(Line, Local, St);
stmt(Exp, St) ->				%This is really just a call
    Line = element(2, Exp),
    call_stmt(Line, Exp, St).

%% assign_stmt(Line, Vars, Exps, State) -> {Assign,State}.

assign_stmt(Info, Vs, Es, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {#assign_stmt{info=Info,variable_statement=Cvs,expressions=Ces},St2}.

assign_loop([V|Vs], St0) ->
    {Cv,St1} = var(V, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {[Cv|Cvs],St2};
assign_loop([], St) -> {[],St}.

%% var(VarExp, State) -> {VarExp,State}.
%%  Step down the prefixexp sequence evaluating as we go, stop at the
%%  END and return a key and a table where to put data. This is a
%%  prefixexp with different tail.

var({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var({'NAME',L,N}, St) -> {var_name(L, N),St}.

var_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',Info,N}, St) ->
    %% Transform this to a key_field with the name string. NO!
    {#key{info=Info, key =lit_name(Info, N)},St};
var_last({key_field,Info,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{info=Info, key =Ce},St1}.

%% call_stmt(Line, Exp, State) -> {Call,State}.

call_stmt(Line, Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    Anno = line_file_anno(Line, St1#cst.lua_file),
    % Anno => Info
    {#call_stmt{info=Anno,call=Ce},St1}.

%% return_stmt(Line, Exps, State) -> {Return,State}.

return_stmt(Info, Expressions, St0) ->
    {Ces,St1} = explist(Expressions, St0),
    {#return_stmt{info=Info, expressions =Ces},St1}.

%% block_stmt(Line, Stats, State) -> {Block,Stmte}.

block_stmt(Info, Ss0, St0) ->
    {Ss1,St1} = stmts(Ss0, St0),
    {#block_stmt{info=Info, block_statement =Ss1},St1}.

block(Info, Ss0, St0) ->
    {Ss1,St1} = stmts(Ss0, St0),
    {#block{info=Info, sub_blocks =Ss1},St1}.

%% while_stmt(Line, Exp, Block, State) -> {While,State}.

while_stmt(Info, Exp, B, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Cb,St2} = block(Info, B, St1),
    {#while_stmt{info= Info, expression =Ce, block =Cb},St2}.

%% repeat_stmt(Line, Block, Exp, State) -> {Repeat,State}.
%%  Append the test expression into the block as a single value
%%  expression.

repeat_stmt(Info, B, Exp, St0) ->
    {Cb0,St1} = block(Info, B, St0),
    {Ce,St2} = expr_stmt(Info, {single,Info,Exp}, St1),
    Cb1 = Cb0#block{sub_blocks =Cb0#block.sub_blocks ++ [Ce]},
    {#repeat_stmt{info=Info, block =Cb1},St2}.

%% if_stmt(Line, Test, Else, State) -> {If,State}.

if_stmt(Info, Tests, Else, St0) ->
    {Cts,St1} = if_tests(Info, Tests, St0),
    {Ce,St2} = block(Info, Else, St1),
    {#if_stmt{info=Info,tests=Cts,else=Ce},St2}.

if_tests(L, Ts, St) ->
    Test = fun ({T,B}, S0) ->
		   {Ct,S1} = exp(T, S0),
		   {Cb,S2} = block(L, B, S1),
		   {{Ct,Cb},S2}
	   end,
    lists:mapfoldl(Test, St, Ts).

%% numfor_stmt(Line, Var, Init, Limit, Step, Stmts, State) -> {NumFor,State}.

numfor_stmt(Info, {'NAME',InfoN,N}, I0, L0, S0, Ss, St0) ->
    Var = var_name(InfoN,N),
    {[I1,L1,S1],St1} = explist([I0,L0,S0], St0),
    {B,St2} = block(Info, Ss, St1),
    {#nfor_stmt{info=Info,v=Var,init=I1,limit=L1,step=S1, block =B},St2}.

%% genfor_stmt(Line, Vars, Generators, Stmts, State) -> {GenFor,State}.

genfor_stmt(Info, Vs0, Gs0, Ss, St0) ->
    Vs1 = [ var_name(InfoN, N) || {'NAME',InfoN,N} <- Vs0 ],
    {Gs1,St1} = explist(Gs0, St0),
    {B,St2} = block(Info, Ss, St1),
    {#gfor_stmt{info=Info,vs=Vs1,gens=Gs1, block =B},St2}.

%% fdef_stmt(Line, Name, Pars, Stmts, State) -> {Fdef,State}.
%%  Transform this to an assign.

fdef_stmt(Info, Fname, Ps, B, St0) ->
    {V,F,St1} = functiondef(Info, Fname, Ps, B, St0),
    {#assign_stmt{info=Info,variable_statement=[V],expressions=[F]},St1}.

%% functiondef(FunctionDef, State) -> {CFunc,State}.
%% functiondef(Line, Pars, Block, State) -> {CFunc,State}.
%% functiondef(Line, Name, Pars, Block, State) -> {Var,CFunc,State}.
%%  Have to specially handle the case where the function is a
%%  "method". All this really means is that the function has an extra
%%  parameter 'self' prepended to the parameter list.

functiondef({functiondef, FileAndLine,Ps,B}, St) ->
    functiondef(FileAndLine, Ps, B, St).

functiondef(FileAndLine, Ps, Stmts, St0) ->
    {Cp,Cb,St1} = function_block(Ps, Stmts, St0),
    Anno = line_file_anno(FileAndLine, St1#cst.lua_file),
    {#fdef{info=Anno, func_parameters =Cp, statements =Cb},St1}.

functiondef(FileAndLine, Name0, Ps0, B, St0) ->
    %% Check if method and transform method to 'NAME' and add self to vars.
    case is_method(Name0) of			%Export Name1 and Ps1

    % Name1 can't be _Name1 because 'Name1' unsafe...
	{yes,Name1} -> Ps1 = [{'NAME',FileAndLine,self}|Ps0];
	no -> Name1 = Name0, Ps1 = Ps0
    end,
    {Var,St1} = funcname(Name1, St0),
    {F0,St2} = functiondef(FileAndLine, Ps1, B, St1),
    %% Add the function name to the annotations.
    Anno = luerl_anno:set(name, flat_funcname(Name1), F0#fdef.info),
    F1 = F0#fdef{info=Anno},
    {Var,F1,St2}.

is_method({'NAME',_,_}) -> no;
is_method({'.',L,N,Rest0}) ->
    case is_method(Rest0) of
        {yes,Rest1} -> {yes,{'.',L,N,Rest1}};
        no -> no                                %No change
    end;
is_method({method,_,{'NAME',_,_}=N}) -> {yes,N}.

flat_funcname(Name) ->
    list_to_binary(flat_funcname(Name, [])).

flat_funcname({'NAME',_,N}, Rest) -> [N|Rest];
flat_funcname({'.',_,L,R}, Rest) ->
    flat_funcname(L, [<<".">>|flat_funcname(R, Rest)]).

%% funcname(FuncNameExp, State) -> {CFuncNameExp,State}.

funcname({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_first(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
funcname({'NAME',L,N}, St) ->
    {var_name(L, N),St}.

funcname_first({'NAME',L,N}, St) ->
    {var_name(L, N),St}.

funcname_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_element(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
funcname_rest(Exp, St) ->
    funcname_last(Exp, St).

funcname_element({'NAME',Info,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{info=Info, key =lit_name(Info, N)},St}.

%% Method call has been transformed away
funcname_last({'NAME',Info,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{info=Info, key =lit_name(Info, N)},St}.

%% local_stmt(Line, Local, State) -> {Assign,State}.
%%  Create and assign local variables.

local_stmt(Info, {functiondef,Lf,Name,Ps,B}, St0) ->
    {Var,F,St1} = functiondef(Lf, Name, Ps, B, St0),
    {#local_fdef_stmt{info=Info,v=Var,f=F},St1};
local_stmt(Info, {assign,_,Ns,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cns,St2} = lists:mapfoldl(fun (V, St) -> var(V, St) end, St1, Ns),
    {#local_assign_stmt{info=Info,vs=Cns, expressions =Ces},St2}.

%% expr_stmt(Line, Exp, State) -> {Call,State}.
%%  The expression pseudo statement. This will return a single value.

expr_stmt(Info, Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#expr_stmt{info=Info,exp=Ce},St1}.

%% explist(Exprs, State) -> {Ins,State}.
%% exp(Expression, State) -> {Ins,State}.

explist([E|Es], St0) ->
    {Ce,St1} = exp(E, St0),
    {Ces,St2} = explist(Es, St1),
    {[Ce|Ces],St2};
explist([], St) -> {[],St}.			%No expressions at all

exp({nil,Info}, St) -> {#lit{info=Info,v=nil},St};
exp({false,Info}, St) -> {#lit{info=Info,v=false},St};
exp({true,Info}, St) -> {#lit{info=Info,v=true},St};
exp({'NUMERAL',Info,N}, St) -> {#lit{info=Info,v=N},St};
exp({'LITERALSTRING',Info,S}, St) -> {#lit{info=Info,v=S},St};
exp({'...',Info}, St) ->
    {var_name(Info, '...'),St};
    %% {#lit{info=Info,v='...'},St};
exp({functiondef,Info,Ps,B}, St0) ->
    {Cf,St1} = functiondef(Info, Ps, B, St0),
    {Cf,St1};
exp({table,Info,Fs}, St0) ->
    {Cfs,St1} = tableconstructor(Fs, St0),
    {#table_constructor{info=Info, fields =Cfs},St1};
exp({op,Info,Op,A1,A2}, St0) ->
    {Ca1,St1} = exp(A1, St0),
    {Ca2,St2} = exp(A2, St1),
    {#op{info=Info,op=Op, arguments =[Ca1,Ca2]},St2};
exp({op,Info,Op,A}, St0) ->
    {Ca,St1} = exp(A, St0),
    {#op{info=Info,op=Op, arguments =[Ca]},St1};
exp(E, St) ->
    prefixexp(E, St).

%% prefixexp(PrefixExp, State) -> {CPrefixExp,State}.

prefixexp({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp(P, St) -> prefixexp_first(P, St).

prefixexp_first({'NAME',L,N}, St) ->
    {var_name(L, N),St};
prefixexp_first({single,Info,E}, St0) ->
    {Ce,St1} = exp(E, St0),
    {#single{info=Info,e=Ce},St1}.

prefixexp_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',Info,N}, St) ->
    %% Transform this to a key_field with the name string
    {#key{info=Info, key =lit_name(Info, N)},St};
prefixexp_element({key_field,Info,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{info=Info, key =Ce},St1};
prefixexp_element({functioncall,Info,Args}, St0) ->
    {Cas,St1} = explist(Args, St0),
    Anno = line_file_anno(Info, St1#cst.lua_file),
    {#fcall{info=Anno,as=Cas},St1};
prefixexp_element({methodcall,InfoM,{'NAME',InfoN,N},Args}, St0) ->
    {Args1,St1} = explist(Args, St0),
    {#method_call{info=InfoM,m=lit_name(InfoN, N),as=Args1},St1}.

dot(Info, Exp, Rest) -> #dot{info=Info,e=Exp,r=Rest}.

function_block(Pars, Stmts, St0)->
    {Cps,St1} = make_local_pars(Pars, St0),
    {Cs,St2} = stmts(Stmts, St1),
    %% io:format("fb: ~p\n", [{St3#comp.fs}]),
    {Cps,Cs,St2}.

make_local_pars(Ps, St) ->
    Add = fun ({'NAME',Info,N}, S) -> {var_name(Info, N),S};
	      ({'...',Info}, S) -> {var_name(Info, '...'),S}
	  end,
    lists:mapfoldl(Add, St, Ps).

%% tableconstrutor(Fields, State) -> {Instrs,State}.
%%  Build the instructions to construct a table. We could be smarter
%%  here and recognise already uses keys and only actually insert the
%%  last one. Or we could pre-order the table elements so the keys are
%%  already sorted. We can't unpack the last field if it is a multiple
%%  value efield as this must be done at run-time.

tableconstructor(Fs, St0) ->
    %% N.B. this fun is for a MAPFOLDL!!
    Fun = fun ({exp_field,Info,Ve}, S0) ->
		       {Ce,S1} = exp(Ve, S0),	%Value
		       {#efield{info=Info, value =Ce},S1};

	    ({name_field,Info,{'NAME',InfoN,N},Ve}, S0) ->
		       {Ce,S1} = exp(Ve, S0),	%Value
	         {#kfield{info=Info, key =lit_name(InfoN, N), value =Ce},S1};

	      ({key_field,Info,Ke,Ve}, S0) ->
		  {Ck,S1} = exp(Ke, S0),	%Key
		  {Cv,S2} = exp(Ve, S1),	%Value
		  {#kfield{info=Info, key =Ck, value =Cv},S2}
	  end,
    {Cfs,St1} = lists:mapfoldl(Fun, St0, Fs),
    {Cfs,St1}.

%% var_name(Line, Name) -> #var{}.
%% lit_name(Line, Name) -> #lit{}.

lit_name(Info, N) -> #lit{info=Info,v=N}.

var_name(Info, N) -> #var{info=Info, name =N}.

%% line_file_anno(Line, File) -> Anno.
%% set_anno(KeyList, Anno) -> Anno.

line_file_anno( #info_structure{linenum = Line}, LuaFileName) -> % interesting: functions knows the source file name without token info
    Anno = luerl_anno:new(Line),
    luerl_anno:set(file, LuaFileName, Anno).

set_anno(Ps, Anno) ->
    lists:foldl(fun ({Key,Val}, A) -> luerl_anno:set(Key, Val, A) end,
		Anno, Ps).
