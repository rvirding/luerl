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

%% File    : luerl_comp.erl
%% Author  : Robert Virding
%% Purpose : A very basic LUA 5.2 compiler.

%% This version of the compiler just handles local/global
%% variables. The basic AST is preserved but a size field has been
%% added to blocks and function definitions and all variables are now
%% explicitly local (and where) or global. This checking of variables
%% means that no 'NAME's are returned and variables used as table
%% lookup keys are now converted to 'STRING's.
%%
%% We explicitly mirror the parser rules which generate the AST and do
%% not try to fold similar structures into common code. While this
%% means we get more code it also becomes more explicit and clear what
%% we are doing. It may also allow for specific optimisations. And
%% example is that we DON'T fold 'var' and 'funcname' even though they
%% are almost the same.

-module(luerl_comp).

-include("luerl.hrl").

-export([string/1,fix_labels/1]).

-export([chunk/1]).

string(S) ->
    {ok,C} = luerl:load(S),
    Is = chunk(C),
    {C,Is}.

-record(lint, {					%Lint information
	 }).

-record(comp, {fs=[],				%Variable frames
	       locv=false,			%Local variables
	       lvs=[],
	       locf=false,			%Local functions
	       ups=[],				%Upvalues, free variables
	       bd=0,				%Current block depth
	       lab=0,				%Label index
	       lint=#lint{}			%Lint data
	      }).

%% push_frame(State) -> State.
%% pop_frame(State) -> State.
%% frame_size(State) -> Size.

push_frame(#comp{fs=Fs}=St) -> St#comp{fs=[orddict:new()|Fs]}.

pop_frame(#comp{fs=Fs}=St) -> St#comp{fs=tl(Fs)}.

frame_size(#comp{fs=Fs}) -> length(hd(Fs)).

%% add_local_var(Var, State) -> State.
%% add_local_vars(Vars, State) -> State.
%% find_var(Var, State) -> {stack,Depth,Index} | global.
%%  When we add an already existing variable we jst reuse its
%%  slot. This is space efficient and safe in one block. NOTE: THIS IS
%%  NOT SAFE WE WANT TO KEEP NESTED BLOCKS ONE FRAME!!

add_local_var(N, #comp{fs=[F0|Fs]}=St) ->
    %% F1 = orddict:store(N, length(F0)+1, F0),
    F1 = case orddict:find(N, F0) of
	     {ok,_} -> F0;			%Reuse existing slot
	     error ->				%New slot
		 orddict:store(N, length(F0)+1, F0)
	 end,
    St#comp{fs=[F1|Fs]}.

add_local_vars(Ns, St) ->
    lists:foldl(fun (N, S) -> add_local_var(N, S) end, St, Ns).

find_var(N, #comp{fs=Fs}) ->
    find_var(N, 1, Fs).

find_var(N, D, [Fs|Fss]) ->
    case orddict:find(N, Fs) of
	{ok,I} -> {stack,D,I};			%On the stack at depth D
	error -> find_var(N, D+1, Fss)
    end;
find_var(_, _, []) -> global.

%% make_local_var(Line, Name, State) -> {{local_var,Line,Name,D,I},State}.
%% make_local_vars(Line, Vars, State) -> {[Var],State}.
%% get_var(Line, Name, State) ->
%%     {local_var,Line,Name,D,I} | {global_var,Line,Name}.

make_local_var(L, N, St0) ->
    St1 = add_local_var(N, St0),
    {stack,1,I} = find_var(N, St1),
    {{local_var,L,N,I},St1}.

make_local_vars(L, Ns, St) ->
    lists:mapfoldl(fun (N, S) -> make_local_var(L, N, S) end, St, Ns).

get_var(L, N, St) ->
    case find_var(N, St) of			%Stack or global variable?
	{stack,1,I} -> {local_var,L,N,I};	%Local
	{stack,D,I} -> {stack_var,L,N,D,I};	%On the stack
	global -> {global_var,L,N}		%Global
    end.

%% chunk(Chunk) -> {ok,Code} | {error,Reason}.

chunk({functiondef,L,Ps,B}) ->
    St0 = #comp{},
    {Cf,_} = exp({functiondef,L,Ps,B}, St0),
    {ok,Cf}.

%% chunk(Code) ->
%%     St0 = #comp{},
%%     %% {{_,_,Is},_} = function_block([{'...',0}], Code, St0),
%%     {Is0,_} = block(Code, St0),
%%     Is1 = fix_labels(Is0),
%%     list_to_tuple(Is1).

with_block(Do, St0) ->
    St1 = push_frame(St0),			%Push a new variable scope
    {C,St2} = Do(St1#comp{}),
    %%io:format("wb: ~p\n", [{St2#comp.fs}]),
    St3 = pop_frame(St2),			%Restore old variable scope
    {C,frame_size(St2),St3}.

block(Stats, St) ->
    Do = fun (S) -> stats(Stats, S) end,
    with_block(Do, St).

stats([{';',_}|Ss], St) -> stats(Ss, St);	%No-op so we drop it
stats([S|Ss], St0) ->
    {Is,St1} = stat(S, St0),
    {Iss,St2} = stats(Ss, St1),
    {[Is|Iss],St2};
stats([], St) -> {[],St}.

%% stat(Statement, State) -> {CStat,State}.

stat({';',_}=Semi, St) -> {Semi,St};		%No-op
stat({assign,L,Vs,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {{assign,L,Cvs,Ces},St2};
stat({return,L,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {{return,L,Ces},St1};
stat({break,L}, St) ->				%Interesting
    {{break,L},St};
stat({block,L,B}, St0) ->
    {Cb,Sz,St1} = block(B, St0),
    {{block,L,Sz,Cb},St1};
stat({'while',L,Exp,Body}, St0) ->
    {Ce,Sz,Cb,St1} = do_while(Exp, Body, St0),
    {{'while',L,Ce,Sz,Cb},St1};
stat({'repeat',L,Body,Exp}, St0) ->
    {Sz,Cb,Ce,St1} = do_repeat(Body, Exp, St0),
    {{'repeat',L,Sz,Cb,Ce},St1};
stat({'if',L,Tests,Else}, St0) ->
    {Cts,Ce,St1} = do_if(Tests, Else, St0),
    {{'if',L,Cts,Ce},St1};
stat({for,Line,V,I,L,S,B}, St0) ->
    {Cv,Ci,Cl,Cs,Sz,Cb,St1} = numeric_for(V, I, L, S, B, St0),
    {{for,Line,Cv,Ci,Cl,Cs,Sz,Cb},St1};
stat({for,Line,V,I,L,B}, St0) ->			%Default step of 1.0
    {Cv,Ci,Cl,Cs,Sz,Cb,St1} = numeric_for(V, I, L, {'NUMBER',Line,1.0}, B, St0),
    {{for,Line,Cv,Ci,Cl,Cs,Sz,Cb},St1};
stat({for,Line,Ns,Gens,B}, St0) ->
    {Cvs,Cg,Sz,Cb,St1} = generic_for(Ns, Gens, B, St0),
    {{for,Line,Cvs,Cg,Sz,Cb},St1};
stat({functiondef,L,Fname,Ps,B}, St) ->
    functiondef(L, Fname, Ps, B, St);
    %% stat({assign,L,[Fname],[{functiondef,L,Ps,B}]}, St);
stat({local,Local}, St0) ->
    {Cloc,St1} = local(Local, St0),
    {{local,Cloc},St1};
stat(Exp, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Ce,St1}.

assign_loop([V|Vs], St0) ->
    {Cv,St1} = var(V, St0),
    {Cvs,St2} = assign_loop(Vs, St1),
    {[Cv|Cvs],St2};
assign_loop([], St) -> {[],St}.

%% var(VarExp, State) -> {VarExp,State}.
%%  Step down the prefixexp sequence evaluating as we go, stop at the
%%  end and return a key and a table where to put data. This is a
%%  prefixexp with different tail.

var({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {{'.',L,Ce,Cr},St2};
var({'NAME',L,N}, St) -> {get_var(L, N, St),St}.

var_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = var_rest(Rest, St1),
    {{'.',L,Ce,Cr},St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string
    S = name_string(N),
    {{key_field,L,{'STRING',L,S}},St};
var_last({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {{key_field,L,Ce},St1}.

%% do_while(Test, Body, State) -> {Test,Body,State}.

do_while(Exp, B, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Cb,Sz,St2} = block(B, St1),
    {Ce,Sz,Cb,St2}.

%% do_repeat(Body, Test, State) -> {Instrs,State}.

do_repeat(B, Exp, St0) ->
    RB = fun (S0) ->				%Repeat block
		 {Cb,S1} = stats(B, S0),
		 {Ce,S2} = exp(Exp, S1),
		 {{Cb,Ce},S2}
	 end,
    {{Cb,Ce},Sz,St1} = with_block(RB, St0),
    {Sz,Cb,Ce,St1}.

%% do_if(Tests, Else, State) -> {Test,Else,State}.

do_if(Tests, Else, St0) ->
    {Cts,St1} = do_if_tests(Tests, St0),
    {Ce,Esz,St2} = block(Else, St1),
    {Cts,{Esz,Ce},St2}.

do_if_tests([{Exp,B}|Ts], St0) ->
    {Ce,St1} = exp(Exp, St0),
    {Cb,Sz,St2} = block(B, St1),
    {Cts,St3} = do_if_tests(Ts, St2),
    {[{Ce,Sz,Cb}|Cts],St3};
do_if_tests([], St) -> {[],St}.

%% numeric_for(Var, Init, Limit, Step, Block, State) -> {Instrs,State}.
%%  This is more or less how the Lua machine does it, but I don't
%%  really like it.

numeric_for({'NAME',Ln,N}, I, L, S, B, St0) ->
    {[Ci,Cl,Cs],St1} = explist([I,L,S], St0),
    For = fun (S0) ->
		  {V,S1} = make_local_var(Ln, N, S0),
		  {Cb,S2} = stats(B, S1),
		  {{V,Cb},S2}
	  end,
    {{V,Cb},Sz,St2} = with_block(For, St1),
    {V,Ci,Cl,Cs,Sz,Cb,St2}.

%% generic_for(Names, Gens, Block, State) -> {Instrs,State}.

generic_for(Ns, Gens, B, St0) ->
    {Cgs,St1} = explist(Gens, St0),
    For = fun (S0) ->
		  {Vs,S1} = add_local_pars(Ns, S0),
		  {Cb,S2} = stats(B, S1),
		  {{Vs,Cb},S2}
	  end,
    {{Cvs,Cb},Sz,St2} = with_block(For, St1),
    {Cvs,Cgs,Sz,Cb,St2}.

functiondef(L, Ps, B, St0) ->
    {Cp,Cb,Sz,St1} = function_block(Ps, B, St0),
    {{functiondef,L,Sz,Cp,Cb},St1}.

%% functiondef(Line, Name, Pars, Block, State) -> {CFunc,State}.
%%  Have to handle the case where the function is a "method". All this
%%  really means is that the function has an extra parameter 'self'
%%  prepended to the paramter list.

functiondef(L, Name0, Ps0, B, St0) ->
    %% Check if method and transform method to 'NAME'.
    case is_method(Name0) of			%Export Name1 and Ps1
	{yes,Name1} -> Ps1 = [{'NAME',L,self}|Ps0];
	no -> Name1 = Name0, Ps1 = Ps0
    end,
    {Cn,St1} = funcname(Name1, St0),
    {Cp,Cb,Sz,St2} = function_block(Ps1, B, St1),
    {{functiondef,L,Cn,Sz,Cp,Cb},St2}.

is_method({'NAME',_,_}) -> no;
is_method({'.',L,N,Rest0}) ->
    case is_method(Rest0) of
        {yes,Rest1} -> {yes,{'.',L,N,Rest1}};
        no -> no                                %No change
    end;
is_method({method,_,{'NAME',_,_}=N}) -> {yes,N}.

%% funcname(FuncNameExp, State) -> {CFuncNameExp,State}.

funcname({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_first(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {{'.',L,Ce,Cr},St2};
funcname({'NAME',L,N}, St) ->
    {get_var(L, N, St),St}.

funcname_first({'NAME',L,N}, St) ->
    {get_var(L, N, St),St}.

funcname_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = funcname_element(Exp, St0),
    {Cr,St2} = funcname_rest(Rest, St1),
    {{'.',L,Ce,Cr},St2};
funcname_rest(Exp, St) ->
    funcname_last(Exp, St).

funcname_element({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    S = name_string(N),
    {{key_field,L,{'STRING',L,S}},St}.

funcname_last({'NAME',L,N}, St) ->
    %% Transform this to key_field with the name string.
    S = name_string(N),
    {{key_field,L,{'STRING',L,S}},St};
funcname_last({method,L,{'NAME',L,N}}, St) ->	%This will never occur
    %% Transform this to method with the name string.
    S = name_string(N),
    {{method,L,{'STRING',L,S}},St}.

%% local(Local, State) -> {Instrs,State}.
%% Create and assign local variables.

local({assign,L,Ns,Es}, St0) ->
    {Ces,St1} = explist(Es, St0),
    {Cns,St2} = assign_local_loop(Ns, St1),
    {{assign,L,Cns,Ces},St2};
local({functiondef,Lf,{'NAME',_,N}=Name,Ps,B}, St0) ->
    %% Set name separately first so recursive call finds right Name.
    St1 = add_local_var(N, St0),		%Make local!
    functiondef(Lf, Name, Ps, B, St1).

assign_local_loop([{'NAME',L,N}|Ns], St0) ->
    {Var,St1} = make_local_var(L, N, St0),
    {Vs,St2} = assign_local_loop(Ns, St1),
    {[Var|Vs],St2};
assign_local_loop([], St) -> {[],St}.

explist([E], St0) ->
    {I,St1} = exp(E, St0),
    {[I],St1};
explist([E|Es], St0) ->
    {Ies,St1} = exp(E, St0),
    {Iess,St2} = explist(Es, St1),
    {[Ies|Iess],St2};
explist([], St) -> {[],St}.			%No expressions at all

%% exp(Expression, State) -> {Ins,State}.
%%  Single determines if we are to only return the first value of a
%%  list of values. Single false does not make us a return a list.

exp({nil,_}=Nil, St) -> {Nil,St};
exp({false,_}=F, St) -> {F,St};
exp({true,_}=T, St) -> {T,St};
exp({'NUMBER',_,_}=N, St) -> {N,St};
exp({'STRING',_,_}=S, St) -> {S,St};
exp({'...',L}, St) ->
    Var = get_var(L, '...', St),
    {Var,St};
exp({functiondef,L,Ps,B}, St0) ->
    {Cf,St1} = functiondef(L, Ps, B, St0),
    {Cf,St1};
exp({table,L,Fs}, St0) ->
    {Cfs,St1} = tableconstructor(Fs, St0),
    {{table,L,Cfs},St1};
%% 'and' and 'or' short-circuit so need special handling.
%% exp({op,_,'and',A1,A2}, St0) ->
%%     {After,St1} = new_label(St0),
%%     {Ia1s,St2} = exp(A1, St1),
%%     {Ia2s,St3} = exp(A2, St2),
%%     %% A bit convoluted with the stack here.
%%     {Ia1s ++ [dup,{br_false,After},pop] ++ Ia2s ++ [{label,After}], St3};
%% exp({op,_,'or',A1,A2}, St0) ->
%%     {After,St1} = new_label(St0),
%%     {Ia1s,St2} = exp(A1, St1),
%%     {Ia2s,St3} = exp(A2, St2),
%%     %% A bit convoluted with the stack here.
%%     {Ia1s ++ [dup,{br_true,After},pop] ++ Ia2s ++ [{label,After}], St3};
%% All the other operators are strict.
exp({op,L,Op,A1,A2}, St0) ->
    {Ca1,St1} = exp(A1, St0),
    {Ca2,St2} = exp(A2, St1),
    {{op,L,Op,Ca1,Ca2},St2};
exp({op,L,Op,A}, St0) ->
    {Ca,St1} = exp(A, St0),
    {{op,L,Op,Ca},St1};
exp(E, St) ->
    prefixexp(E, St).

%% prefixexp(PrefixExp, State) -> {CPrefixExp,State}.

prefixexp({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_first(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {{'.',L,Ce,Cr},St2};
prefixexp(P, St) -> prefixexp_first(P, St).

prefixexp_first({'NAME',L,N}, St) ->
    {get_var(L, N, St),St};
prefixexp_first({single,L,E}, St0) ->
    {Ce,St1} = exp(E, St0),
    {{single,L,Ce},St1}.

prefixexp_rest({'.',L,Exp,Rest}, St0) ->
    {Ce,St1} = prefixexp_element(Exp, St0),
    {Cr,St2} = prefixexp_rest(Rest, St1),
    {{'.',L,Ce,Cr},St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',L,N}, St) ->
    %% Transform this to a key_field with the name string
    S = name_string(N),
    {{key_field,L,{'STRING',L,S}},St};
prefixexp_element({key_field,L,Exp}, St0) ->
    {Ce,St1} = exp(Exp, St0),
    {{key_field,L,Ce},St1};
prefixexp_element({functioncall,L,Args}, St0) ->
    {Cas,St1} = explist(Args, St0),
    {{functioncall,L,Cas},St1};
prefixexp_element({method,Lm,{'NAME',Ln,N},Args}, St0) ->
    {Args1,St1} = explist(Args, St0),
    S = name_string(N),
    {{method,Lm,{'STRING',Ln,S},Args1},St1}.

add_local_pars(Ps, St) ->
    Add = fun ({'NAME',L,N}, St0) -> make_local_var(L, N, St0);
	      ({'...',L}, S) -> make_local_var(L, '...', S)
	  end,
    lists:mapfoldl(Add, St, Ps).

function_block(Pars, Stats, St0)->
    St1 = push_frame(St0),
    {Cps,St2} = add_local_pars(Pars, St1),
    {Cs,St3} = stats(Stats, St2),
    %% io:format("fb: ~p\n", [{St3#comp.fs}]),
    St4 = pop_frame(St3),
    {Cps,Cs,frame_size(St3),St4}.

fix_labels(Is) ->
    Ls = get_labels(Is, 1, []),
    insert_offs(Is, 1, Ls).

get_labels([{label,L}|Is], O, Ls) -> get_labels(Is, O, [{L,O}|Ls]);
get_labels([_|Is], O, Ls) -> get_labels(Is, O+1, Ls);
get_labels([], _, Ls) -> Ls.

insert_offs([{forloop,L}|Is], O, Ls) ->
    {_,Lo} = lists:keyfind(L, 1, Ls),		%Stupid function
    [{forloop,Lo-(O+1)}|insert_offs(Is, O+1, Ls)];	%Just jump
insert_offs([{tforloop,L}|Is], O, Ls) ->
    {_,Lo} = lists:keyfind(L, 1, Ls),		%Stupid function
    [{tforloop,Lo-(O+1)}|insert_offs(Is, O+1, Ls)];	%Just jump
insert_offs([{Br,L}|Is], O, Ls)
  when Br =:= br; Br =:= br_true; Br =:= br_false ->
    {_,Lo} = lists:keyfind(L, 1, Ls),		%Stupid function
    %% Pc incremented before adding offset!
    [{Br,Lo-(O+1)}|insert_offs(Is, O+1, Ls)];
insert_offs([{Jmp,L}|Is], O, Ls)		%Not used yet!
  when Jmp =:= jmp; Jmp =:= jmp_true; Jmp =:= jmp_false ->
    {_,Lo} = lists:keyfind(L, 1, Ls),		%Stupid function
    [{Jmp,Lo}|insert_offs(Is, O+1, Ls)];	%Just jump
insert_offs([{label,_}|Is], O, Ls) -> insert_offs(Is, O, Ls);
insert_offs([I|Is], O, Ls) -> [I|insert_offs(Is, O+1, Ls)];
insert_offs([], _, _) -> [].

%% tableconstrutor(Fields, State) -> {Instrs,State}.
%%  Build the instructions to construct a table. We could be smarter
%%  here and recognise already uses keys and only actually insert the
%%  last one. Or we could pre-order the table elements so the keys are
%%  already sorted, but that would mean adding stack-ops.

tableconstructor(Fs, St0) ->
    %% N.B. this fun is for a MAPFOLDL!!
    Fun = fun ({exp_field,L,Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  {{exp_field,L,Ce},S1};
	      ({name_field,L,{'NAME',Ln,N},Ve}, S0) ->
		  {Ce,S1} = exp(Ve, S0),	%Value
		  S = name_string(N),
		  {{name_field,L,{'STRING',Ln,S},Ce},S1};
	      ({key_field,L,Ke,Ve}, S0) ->
		  {Ck,S1} = exp(Ke, S0),	%Key
		  {Cv,S2} = exp(Ve, S1),	%Value
		  {{key_field,L,Ck,Cv},S2}
	  end,
    {Cfs,St1} = lists:mapfoldl(Fun, St0, Fs),
    {Cfs,St1}.

%% name_string(Name) -> String.
%%  We do this a lot!

name_string(Name) -> atom_to_binary(Name, latin1).
