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

?MODULEDOC(false).

-export([chunk/2]).

%% chunk(Code, CompInfo) -> {ok,Code} | {error,Reason}.

chunk(Code0, #cinfo{opts=Opts}=Ci0) ->
    %% The chunk is a function.
    {Code1,_Ci1} = functiondef(Code0, Ci0),
    luerl_comp:debug_print(Opts, "cn: ~p\n", [Code1]),
    {ok,Code1}.

stmts([{local,L,{functiondef,Lf,Name,Pars,Block}}|Ss], St) ->
    %% Need to split this up to handle references to Name in the function.
    Fdef = {functiondef,Lf,Pars,Block},
    stmts([{local, L, {assign, L, [Name], [{nil,L}]}},
           {';',L},
           {local, Lf, {assign, Lf, [Name], [Fdef]}} | Ss],
          St);
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
stmt({break,L}, St) ->				%Interesting
    {#break_stmt{l=L},St};
stmt({block,Line,B}, St) ->
    block_stmt(Line, B, St);
stmt({while,Line,Exp,B}, St) ->
    while_stmt(Line, Exp, B, St);
stmt({repeat,Line,B,Exp}, St) ->
    repeat_stmt(Line, B, Exp, St);
stmt({'if',Line,Tests,Else}, St) ->
    if_stmt(Line, Tests, Else, St);
stmt({for,Line,V,I,L,B}, St) ->			%Default step of 1
    numfor_stmt(Line, V, I, L, {'NUMERAL',Line,1}, B, St);
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

assign_stmt(Line, Vs, Es, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    Anno = line_file_anno(Line, St2),
    {#assign_stmt{l=Anno,vars=Cvs,exps=Ces},St2}.

assign_loop([V|Vs], St0) ->
    {Cv,St1} = var(V, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {[Cv|Cvs],St2};
assign_loop([], St) -> {[],St}.

%% var(VarExp, State) -> {VarExp,State}.
%%  Step down the prefixexp sequence evaluating as we go, stop at the
%%  END and return a key and a table where to put data. This is a
%%  prefixexp with different tail. Attributes are only allowed in
%%  local assigns and for now we just ignore them.

var({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var({{'NAME',L,N},_Attribute}, St) ->
    %% For now we ignore attributes.
    {var_name(L, N),St};
var({'NAME',L,N}, St) -> {var_name(L, N),St}.

var_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string. NO!
    {#key{l=L,key=lit_name(L, N)},St};
var_last({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{l=L,key=Ce},St1}.

%% call_stmt(Line, Exp, State) -> {Call,State}.

call_stmt(Line, Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    Anno = line_file_anno(Line, St1),
    {#call_stmt{l=Anno,call=Ce},St1}.

%% return_stmt(Line, Exps, State) -> {Return,State}.

return_stmt(Line, Es, St0) ->
    {Ces,St1} = explist(Es, St0),
    Anno = line_file_anno(Line, St1),
    {#return_stmt{l=Anno,exps=Ces},St1}.

%% block_stmt(Line, Stats, State) -> {Block,Stmte}.

block_stmt(Line, Ss0, St0) ->
    {Ss1,St1} = stmts(Ss0, St0),
    Anno = line_file_anno(Line, St1),
    {#block_stmt{l=Anno,body=Ss1},St1}.

block(Line, Ss0, St0) ->
    {Ss1,St1} = stmts(Ss0, St0),
    {#block{l=Line,body=Ss1},St1}.

%% while_stmt(Line, Exp, Block, State) -> {While,State}.

while_stmt(Line, Exp, B, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Cb,St2} = block(Line, B, St1),
    Anno = line_file_anno(Line, St2),
    {#while_stmt{l=Anno,exp=Ce,body=Cb},St2}.

%% repeat_stmt(Line, Block, Exp, State) -> {Repeat,State}.
%%  Append the test expression into the block as a single value
%%  expression.

repeat_stmt(Line, B, Exp, St0) ->
    {Cb0,St1} = block(Line, B, St0),
    {Ce,St2} = expr_stmt(Line, {single,Line,Exp}, St1),
    Cb1 = Cb0#block{body=Cb0#block.body ++ [Ce]},
    Anno = line_file_anno(Line, St2),
    {#repeat_stmt{l=Anno,body=Cb1},St2}.

%% if_stmt(Line, Test, Else, State) -> {If,State}.

if_stmt(Line, Tests, Else, St0) ->
    {Cts,St1} = if_tests(Line, Tests, St0),
    {Ce,St2} = block(Line, Else, St1),
    Anno = line_file_anno(Line, St2),
    {#if_stmt{l=Anno,tests=Cts,else_block=Ce},St2}.

if_tests(L, Ts, St) ->
    Test = fun ({T,B}, S0) ->
		   {Ct,S1} = exp(T, S0),
		   {Cb,S2} = block(L, B, S1),
		   {{Ct,Cb},S2}
	   end,
    lists:mapfoldl(Test, St, Ts).

%% numfor_stmt(Line, Var, Init, Limit, Step, Stmts, State) -> {NumFor,State}.

numfor_stmt(Line, {'NAME',Ln,N}, I0, L0, S0, Ss, St0) ->
    Var = var_name(Ln, N),
    {[I1,L1,S1],St1} = explist([I0,L0,S0], St0),
    {B,St2} = block(Line, Ss, St1),
    Anno = line_file_anno(Line, St2),
    {#nfor_stmt{l=Anno,var=Var,init=I1,limit=L1,step=S1,body=B},St2}.

%% genfor_stmt(Line, Vars, Generators, Stmts, State) -> {GenFor,State}.

genfor_stmt(Line, Vs0, Gs0, Ss, St0) ->
    Vs1 = [ var_name(Ln, N) || {'NAME',Ln,N} <- Vs0 ],
    {Gs1,St1} = explist(Gs0, St0),
    {B,St2} = block(Line, Ss, St1),
    Anno = line_file_anno(Line, St2),
    {#gfor_stmt{l=Anno,vars=Vs1,gens=Gs1,body=B},St2}.

%% fdef_stmt(Line, Name, Pars, Stmts, State) -> {Fdef,State}.
%%  Transform this to an assign.

fdef_stmt(Line, Fname, Ps, B, St0) ->
    {V,F,St1} = functiondef(Line, Fname, Ps, B, St0),
    Anno = line_file_anno(Line, St1),
    {#assign_stmt{l=Anno,vars=[V],exps=[F]},St1}.

%% functiondef(FunctionDef, State) -> {CFunc,State}.
%% functiondef(Line, Pars, Block, State) -> {CFunc,State}.
%% functiondef(Line, Name, Pars, Block, State) -> {Var,CFunc,State}.
%%  Have to specially handle the case where the function is a
%%  "method". All this really means is that the function has an extra
%%  parameter 'self' prepended to the parameter list.

functiondef({functiondef,L,Ps,B}, St) ->
    functiondef(L, Ps, B, St).

functiondef(L, Ps, Stmts, St0) ->
    {Cp,Cb,St1} = function_block(Ps, Stmts, St0),
    Anno = line_file_anno(L, St1),
    {#fdef{l=Anno,pars=Cp,body=Cb},St1}.

functiondef(L, Name0, Ps0, B, St0) ->
    %% Check if method and transform method to 'NAME' and add self to vars.
    case is_method(Name0) of			%Export Name1 and Ps1
	{yes,Name1} -> Ps1 = [{'NAME',L,<<"self">>}|Ps0];
	no -> Name1 = Name0, Ps1 = Ps0
    end,
    {Var,St1} = funcname(Name1, St0),
    {F0,St2} = functiondef(L, Ps1, B, St1),
    %% Add the function name to the annotations.
    Anno = luerl_anno:set(name, flat_funcname(Name1), F0#fdef.l),
    F1 = F0#fdef{l=Anno},
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

funcname_element({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{l=L,key=lit_name(L, N)},St}.

%% Method call has been transformed away
funcname_last({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    {#key{l=L,key=lit_name(L, N)},St}.

%% local_stmt(Line, Local, State) -> {Assign,State}.
%%  Create and assign local variables.

%% local_stmt(Line, {functiondef,Lf,Name,Ps,B}, St0) ->
%%     {Var,F,St1} = functiondef(Lf, Name, Ps, B, St0),
%%     Anno = line_file_anno(Line, St1),
%%     {#local_fdef_stmt{l=Anno,var=Var,func=F},St1};
local_stmt(Line, {assign,_,Ns,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cns,St2} = lists:mapfoldl(fun (V, St) -> var(V, St) end, St1, Ns),
    Anno = line_file_anno(Line, St2),
    {#local_assign_stmt{l=Anno,vars=Cns,exps=Ces},St2}.

%% expr_stmt(Line, Exp, State) -> {Call,State}.
%%  The expression pseudo statement. This will return a single value.

expr_stmt(Line, Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    Anno = line_file_anno(Line, St1),
    {#expr_stmt{l=Anno,exp=Ce},St1}.

%% explist(Exprs, State) -> {Ins,State}.
%% exp(Expression, State) -> {Ins,State}.

explist([E|Es], St0) ->
    {Ce,St1} = exp(E, St0),
    {Ces,St2} = explist(Es, St1),
    {[Ce|Ces],St2};
explist([], St) -> {[],St}.			%No expressions at all

exp({nil,L}, St) -> {#lit{l=L,val=nil},St};
exp({false,L}, St) -> {#lit{l=L,val=false},St};
exp({true,L}, St) -> {#lit{l=L,val=true},St};
exp({'NUMERAL',L,N}, St) -> {#lit{l=L,val=N},St};
exp({'LITERALSTRING',L,S}, St) -> {#lit{l=L,val=S},St};
exp({'...',L}, St) ->
    {var_name(L, '...'),St};
    %% {#lit{l=L,v='...'},St};
exp({functiondef,L,Ps,B}, St0) ->
    {Cf,St1} = functiondef(L, Ps, B, St0),
    {Cf,St1};
exp({table,L,Fs}, St0) ->
    {Cfs,St1} = tableconstructor(Fs, St0),
    {#tabcon{l=L,fields=Cfs},St1};
exp({op,L,Op,A1,A2}, St0) ->
    {Ca1,St1} = exp(A1, St0),
    {Ca2,St2} = exp(A2, St1),
    {#op{l=L,op=Op,args=[Ca1,Ca2]},St2};
exp({op,L,Op,A}, St0) ->
    {Ca,St1} = exp(A, St0),
    {#op{l=L,op=Op,args=[Ca]},St1};
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
prefixexp_first({single,L,E}, St0) ->
    {Ce,St1} = exp(E, St0),
    {#single{l=L,exp=Ce},St1}.

prefixexp_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {dot(L, Ce, Cr),St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string
    {#key{l=L,key=lit_name(L, N)},St};
prefixexp_element({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {#key{l=L,key=Ce},St1};
prefixexp_element({functioncall,L,Args}, St0) ->
    {Cas,St1} = explist(Args, St0),
    Anno = line_file_anno(L, St1),
    {#fcall{l=Anno,args=Cas},St1};
prefixexp_element({methodcall,Lm,{'NAME',Ln,N},Args}, St0) ->
    {Args1,St1} = explist(Args, St0),
    Anno = line_file_anno(Lm, St1),
    {#mcall{l=Anno,meth=lit_name(Ln, N),args=Args1},St1}.

dot(L, Exp, Rest) -> #dot{l=L,exp=Exp,rest=Rest}.

function_block(Pars, Stmts, St0)->
    {Cps,St1} = make_local_pars(Pars, St0),
    {Cs,St2} = stmts(Stmts, St1),
    %% io:format("fb: ~p\n", [{St3#comp.fs}]),
    {Cps,Cs,St2}.

make_local_pars(Ps, St) ->
    Add = fun ({'NAME',L,N}, S) -> {var_name(L, N),S};
	      ({'...',L}, S) -> {var_name(L, '...'),S}
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
    Fun = fun ({exp_field,L,Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {#efield{l=L,val=Ce},S1};
	      ({name_field,L,{'NAME',Ln,N},Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {#kfield{l=L,key=lit_name(Ln, N),val=Ce},S1};
	      ({key_field,L,Ke,Ve}, S0) ->
		  {Ck,S1} = exp(Ke, S0),	%Key
		  {Cv,S2} = exp(Ve, S1),	%Value
		  {#kfield{l=L,key=Ck,val=Cv},S2}
	  end,
    {Cfs,St1} = lists:mapfoldl(Fun, St0, Fs),
    {Cfs,St1}.

%% var_name(Line, Name) -> #var{}.
%% lit_name(Line, Name) -> #lit{}.

lit_name(L, N) -> #lit{l=L,val=N}.

var_name(L, N) -> #var{l=L,name=N}.

%% line_file_anno(Line, State) -> Anno.
%% set_anno(KeyList, Anno) -> Anno.

line_file_anno(L, St) ->
    Anno = luerl_anno:new(L),
    luerl_anno:set(file, St#cinfo.lfile, Anno).

%% set_anno(Ps, Anno) ->
%%     lists:foldl(fun ({Key,Val}, A) -> luerl_anno:set(Key, Val, A) end,
%% 		Anno, Ps).
