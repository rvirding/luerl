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

-module(luerl_comp).

-include("luerl.hrl").

-export([string/1,fix_labels/1]).

-export([chunk/1]).

string(S) ->
    {ok,C} = luerl:compile(S),
    Is = chunk(C),
    {C,Is}.

-record(lint, {					%Lint information
	 }).

-record(comp, {locv=false,			%Local variables
	       lvs=[],
	       locf=false,			%Local functions
	       ups=[],				%Upvalues, free variables
	       bd=0,				%Current block depth
	       lab=0,				%Label index
	       lint=#lint{}			%Lint data
	      }).

chunk(Code) ->
    St0 = #comp{},
    %% {{_,_,Is},_} = function_block([{'...',0}], Code, St0),
    {Is0,_} = block(Code, St0),
    Is1 = fix_labels(Is0),
    list_to_tuple(Is1).

block(Stats, St0) ->
    Locf0 = St0#comp.locf,			%"Global" locf
    Bd0 = St0#comp.bd,				%Block depth
    {Is0,St1} = stats(Stats, St0#comp{locv=false,lvs=[],locf=false,bd=Bd0+1}),
    Locv1 = St1#comp.locv,			%"Local" locv and locf
    Locf1 = St1#comp.locf,
    Is1  = case {Locv1,Locf1} of
	       {true,false} -> [push_env|Is0 ++ [pop_env_free]];
	       {true,true} -> [push_env|Is0 ++ [pop_env]];
	       {false,_} -> Is0
	   end,
    {Is1,St0#comp{locf=Locf0 or Locf1}}.	%Use the original

stats([S|Ss], St0) ->
    {Is,St1} = stat(S, St0),
    {Iss,St2} = stats(Ss, St1),
    {Is ++ Iss,St2};
stats([], St) -> {[],St}.

stat({';',_}, St) -> {[],St};			%No-op
stat({assign,_,Vs,Es}, St0) ->
    {Ias,St1} = assign(Vs, Es, St0),
    {Ias,St1};
stat({return,_,Es}, St0) ->
    {Iess,St1} = explist(Es, St0),
    {Iess ++ [{pack_vals,length(Es)},return],St1};
stat({break,_}, St) ->				%Interesting
    {[{pop,St#comp.bd}],St};
stat({block,_,B}, St0) ->
    {Ibs,St1} = block(B, St0),
    {Ibs,St1};
stat({functiondef,_,Fname,Ps,B}, St) ->
    functiondef(Fname, Ps, B, St);
stat({'while',_,Exp,Body}, St) ->
    do_while(Exp, Body, St);
stat({'repeat',_,Body,Exp}, St) ->
    do_repeat(Body, Exp, St);
stat({'if',_,Tests,Else}, St) ->
    do_if(Tests, Else, St);
stat({for,_,V,I,L,S,B}, St) ->
    numeric_for(V, I, L, S, B, St);
stat({for,Line,V,I,L,B}, St) ->			%Default step of 1.0
    numeric_for(V, I, L, {'NUMBER',Line,1.0}, B, St);
stat({local,Local}, St0) ->
    local(Local, St0);
stat(Exp, St0) ->
    {Ies,St1} = exp(Exp, false, St0),		%It will be dropped anyway
    {Ies ++ [pop],St1}.				%Drop value

assign(Vs, Es, St0) ->
    assign_loop(Vs, 0, Es, 0, St0).

%% Not quite left-to-right, we evaluate the exps left-to-right but
%% before all the vars which we do right to left. Should split set_var
%% into two parts: everything but the last and the last.

assign_loop([V|Vs], Vc, [E], Ec, St0) ->
    {Ies,St1} = exp(E, false, St0),
    {Iass,St2} = assign_loop(Vs, Vc+1, [], Ec+1, St1),
    {Ivs,St3} = set_var(V, St2),
    {Ies ++ Iass ++ Ivs,St3};
assign_loop([V|Vs], Vc, [E|Es], Ec, St0) ->
    {Ies,St1} = exp(E, true, St0),
    {Iass,St2} = assign_loop(Vs, Vc+1, Es, Ec+1, St1),
    {Ivs,St3} = set_var(V, St2),
    {Ies ++ Iass ++ Ivs,St3};
assign_loop([V|Vs], Vc, [], Ec, St0) ->
    {Iass,St1} = assign_loop(Vs, Vc+1, [], Ec, St0),
    {Ivs,St2} = set_var(V, St1),
    {Iass ++ Ivs,St2};				%unpack_vals will give nil's
assign_loop([], Vc, [E|Es], Ec, St0) ->		%No more variables
    {Ies,St1} = exp(E, false, St0),		%It will be dropped anyway
    {Iass,St2} = assign_loop([], Vc, Es, Ec, St1),
    {Ies ++ [pop] ++ Iass,St2};
assign_loop([], Vc, [], Ec, St) -> {[{unpack_vals,Vc,Ec}],St}.

set_var({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_first(Exp, true, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
set_var({'NAME',_,N}, St) ->
    Set = case lists:member(N, St#comp.lvs) of	%Is it a local variable?
	      true -> set_local;
	      false -> set_env
	  end,
    {[name_op(Set, N)],St}.

var_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, true, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',_,N}, St) ->
    {[name_op(set_key, N)],St};
var_last({key_field,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, true, St0),
    {Is ++ [set_key],St1};
var_last({method,_,{'NAME',_,N}}, St0) ->
    %% Must fix function definition here!
    Is = ['*',name_op(push, N),set_key],
    {Is,St0}.

functiondef(Fname0, Ps0, B, St0) ->
    %% Check if funcname a "method", if so add self to pars.
    case is_method(Fname0) of			%Export Fname1, Ps1
	{yes,Fname1} -> Ps1 = [{'NAME',999,'self'}|Ps0];
	no -> Fname1 = Fname0, Ps1 = Ps0
    end,
    {Ifs,St1} = functiondef(Ps1, B, St0),
    {Isvs,St2} = set_var(Fname1, St1),
    {Ifs ++ Isvs,St2#comp{locf=true}}.

functiondef(Ps, B, St0) ->
    {{Locv,Locf,Is},St1} = function_block(Ps, B, St0),
    {[{build_func,Locv,Locf,list_to_tuple(Is)}],St1}.

is_method({'NAME',_,_}) -> no;
is_method({'.',L,N,Rest0}) ->
    case is_method(Rest0) of
	{yes,Rest1} -> {yes,{'.',L,N,Rest1}};
	no -> no				%No change
    end;
is_method({method,_,{'NAME',_,_}=N}) -> {yes,N}.

%% do_while(Test, Body, State) -> {Instrs,State}.

do_while(Exp, B, St0) ->
    {Start,St1} = new_label(St0),		%The beginning
    {End,St2} = new_label(St1),			%The end
    {Ies,St3} = exp(Exp, true, St2),
    {Ibs,St4} = block(B, St3),
    Is = [{label,Start}] ++ Ies ++ [{br_false,End}] ++
	Ibs ++ [{br,Start},{label,End}],
    {Is,St4}.

%% do_repeat(Body, Test, State) -> {Instrs,State}.

do_repeat(B, Exp, St0) ->
    {Start,St1} = new_label(St0),		%The beginning
    {End,St2} = new_label(St1),			%The end
    {Ibs,St3} = block(B, St2),
    {Ies,St4} = exp(Exp, true, St3),
    Is = [{label,Start}] ++ Ibs ++ Ies ++ [{br_false,Start},{label,End}],
    {Is,St4}.

%% do_if(Tests, Else, State) -> {Instrs,State}.

do_if(Tests, Else, St0) ->
    {End,St1} = new_label(St0),
    {Ifs,St2} = do_if_tests(Tests, End, St1),
    {Ies,St3} = block(Else, St2),
    {Ifs ++ Ies ++ [{label,End}],St3}.

do_if_tests([{Exp,B}|Ts], End, St0) ->
    {Ies,St1} = exp(Exp, true, St0),
    {Ibs,St2} = block(B, St1),
    {Ifs,St3} = do_if_tests(Ts, End, St2),
    {Next,St4} = new_label(St3),
    Its = Ies ++ [{br_false,Next}] ++ Ibs ++ [{br,End}],
    {Its ++ [{label,Next}] ++ Ifs,St4};
do_if_tests([], _, St) -> {[],St}.

%% numeric_for(Var, Init, Limit, Step, Block, State) -> {Instrs,State}.
%%  This is more or less how the Lua machine does it, but I don't
%%  really like it.

numeric_for({'NAME',_,N}, I, L, S, B, St0) ->
    {Start,St1} = new_label(St0),		%Labels
    {End,St2} = new_label(St1),
    {Is,St3} = exp(I, true, St2),
    {Ls,St4} = exp(L, true, St3),
    {Ss,St5} = exp(S, true, St4),
    %% Initialse the values: 
    %% [Step,Limit,Init|_] -> forprep -> [Init,Limit,Step|_]
    Ipre = [push_env] ++ Is ++ Ls ++ Ss ++ [forprep,{br,End}] ++ 
	[{label,Start},name_op(set_local,N)],
    {Ibs,St6} = block(B, St5),
    Ipost = [{label,End},{forloop,Start},{pop,3}|if St6#comp.locf -> [pop_env];
						    true -> [pop_env_free]
						 end],
    {Ipre ++ Ibs ++ Ipost,St6}.

local({assign,_,Vs,Es}, St) ->
    assign_local_loop(Vs, 0, Es, 0, St#comp{locv=true});
local({functiondef,_,{'NAME',_,N},Ps,B}, St0) ->
    In = name_op(set_local, N),
    {Ifs,St1} = functiondef(Ps, B, St0),
    {[push_nil,In] ++ Ifs ++ [In],
     St1#comp{locv=true,lvs=[N|St0#comp.lvs],locf=true}}.

assign_local_loop([{'NAME',_,N}|Vs], Vc, [E], Ec, St0) ->
    {Ies,St1} = exp(E, false, St0),
    {Iass,St2} = assign_local_loop(Vs, Vc+1, [], Ec+1, St1),
    St3 = St2#comp{lvs=[N|St2#comp.lvs]},
    {Ies ++ Iass ++ [name_op(set_local, N)],St3};
assign_local_loop([{'NAME',_,N}|Vs], Vc, [E|Es], Ec, St0) ->
    {Ies,St1} = exp(E, true, St0),
    {Iass,St2} = assign_local_loop(Vs, Vc+1, Es, Ec+1, St1),
    St3 = St2#comp{lvs=[N|St2#comp.lvs]},
    {Ies ++ Iass ++ [name_op(set_local, N)],St3};
assign_local_loop([{'NAME',_,N}|Vs], Vc, [], Ec, St0) ->
    {Iass,St1} = assign_local_loop(Vs, Vc+1, [], Ec, St0),
    St2 = St1#comp{lvs=[N|St1#comp.lvs]},
    {Iass ++ [name_op(set_local, N)],St2};
assign_local_loop([], Vc, [E|Es], Ec, St0) ->	%No more variables
    {Ies,St1} = exp(E, false, St0),		%It will be dropped anyway
    {Iass,St2} = assign_local_loop([], Vc, Es, Ec, St1),
    {Ies ++ [pop] ++ Iass,St2};
assign_local_loop([], Vc, [], Ec, St) -> {[{unpack_vals,Vc,Ec}],St}.

explist([E], St) -> exp(E, false, St);		%Append values to output
explist([E|Es], St0) ->
    {Ies,St1} = exp(E, true, St0),
    {Iess,St2} = explist(Es, St1),
    {Ies ++ Iess,St2};
explist([], St) -> {[],St}.

%% exp(Expression, Single, State) -> {Ins,State}.
%%  Single determines if we are to only return the first value of a
%%  list of values. Single false does not make us a return a list.

exp({nil,_}, _, St) -> {[push_nil],St};
exp({false,_}, _, St) -> {[{push,false}],St};
exp({true,_}, _, St) -> {[{push,true}],St};
exp({'NUMBER',_,N}, _, St) -> {[{push,N}],St};
exp({'STRING',_,S}, _, St) -> {[{push,S}],St};
exp({'...',_}, S, St) ->
    {first_value(S, [{get_local,'...'}]),St};
exp({functiondef,_,Ps,B}, _, St0) ->
    {Ifs,St1} = functiondef(Ps, B, St0),
    {Ifs,St1#comp{locf=true}};
exp({table,_,Fs}, _, St0) ->
    {Ifs,St1} = tableconstructor(Fs, St0),
    {Ifs ++ [{build_tab,length(Fs)}],St1};
exp({op,_,Op,A1,A2}, S, St0) ->
    {Ia1s,St1} = exp(A1, true, St0),
    {Ia2s,St2} = exp(A2, true, St1),
    {Ia1s ++ Ia2s ++ first_value(S, [{op2,Op}]),St2};
exp({op,_,Op,A}, S, St0) ->
    {Ias,St1} = exp(A, true, St0),
    {Ias ++ first_value(S, [{op1,Op}]),St1};
exp(E, S, St) ->
    prefixexp(E, S, St).

first_value(true, Is) -> Is ++ [first_value];
first_value(false, Is) -> Is.

prefixexp({'.',_,Exp,Rest}, S, St0) ->
    {Ies,St1} = prefixexp_first(Exp, true, St0),
    {Irs,St2} = prefixexp_rest(Rest, S, St1),
    {Ies ++ Irs,St2};
prefixexp(P, S, St) -> prefixexp_first(P, S, St).

prefixexp_first({'NAME',_,N}, _, St) ->
    Get = case lists:member(N, St#comp.lvs) of	%Is it a local variable?
	      true -> get_local;
	      false -> get_env
	  end,
    {[name_op(Get, N)],St};
prefixexp_first({single,_,E}, _, St0) ->
    exp(E, true, St0).

prefixexp_rest({'.',_,Exp,Rest}, S, St0) ->
    {Ies,St1} = prefixexp_element(Exp, true, St0),
    {Irs,St2} = prefixexp_rest(Rest, S, St1),
    {Ies ++ Irs,St2};
prefixexp_rest(Exp, S, St) ->
    prefixexp_element(Exp, S, St).

prefixexp_element({'NAME',_,N}, _, St) ->
    {[name_op(get_key, N)],St};
prefixexp_element({key_field,_,Exp}, _, St0) ->
    {Is,St1} = exp(Exp, true, St0),
    {Is ++ [get_key],St1};
prefixexp_element({functioncall,_,Args}, S, St0) ->
    {Ias,St1} = explist(Args, St0),
    %% [an,...,a1,func|_] -> [as,func|_]
    Ics = Ias ++ [{pack_vals,length(Args)},call],
    %%Ics = Ias ++ [{call,length(Args)}],		%Optimisation!
    {first_value(S, Ics),St1};
prefixexp_element({method,_,{'NAME',_,N},Args}, S, St0) ->
    %% [meth|_] -> [meth,meth|_] -> [func,meth|_] -> [meth,func|_]
    Im = [dup,name_op(get_key, N),swap],
    %% [meth,func|_] -> [an,..,a1,meth,func|_]
    {Ias,St1} = explist(Args, St0),
    %% [an,..,a1,meth,func|_] -> [as,func|_]
    Ims = Im ++ Ias ++ [{pack_vals,length(Args)+1},call],
    %% Ims = Im ++ Ias ++ [{call,length(Args)+1}],	%Optimisation!
    {first_value(S, Ims),St1}.

function_block(Pars, Stats, St0)->
    Args = Pars =/= [],				%Do we have pars?
    Locf0 = St0#comp.locf,			%"Global" locf
    St1 = St0#comp{locv=false,lvs=[],locf=false,bd=0},
    %% Add instrs for unpacking args or popping unused args.
    if Args ->					%Export Ipre0, St2
	    Iup = case lists:last(Pars) of
		      {'...',_} -> {unpack_args,length(Pars)};
		      _ -> {unpack_vals,length(Pars),1}
		  end,
	    {Iass,St2} = assign_pars_loop(Pars, St1),
	    Ipre0 = [Iup] ++ Iass;
       true -> Ipre0 = [pop],
	       St2 = St1
    end,
    {Iss0,St3} = stats(Stats, St2),
    Iss1 = fix_labels(Iss0),
    Locv1 = St3#comp.locv,			%"Local" locv and locf
    Locf1 = St3#comp.locf,
    %% Do we need an environment here for this function?
    Ipre1 = if Args or Locf1 -> [push_env,swap] ++ Ipre0;
	       true -> Ipre0
	    end,
    Ipost = [{push,[]},return],
    St4 = St0#comp{locf=Locf0 or Locf1},	%Use the original
    {{Args or Locv1,Locf1,Ipre1 ++ Iss1 ++ Ipost},St4}.

assign_pars_loop([{'NAME',_,N}|Vs], St0) ->
    {Iass,St1} = assign_pars_loop(Vs, St0),
    St2 = St1#comp{lvs=[N|St1#comp.lvs]},
    {Iass ++ [name_op(set_local, N)],St2};
assign_pars_loop([{'...',_}], St) ->		%Vararg
    {[{push,'...'},set_local],St};
assign_pars_loop([], St) -> {[],St}.

fix_labels(Is) ->
    Ls = get_labels(Is, 1, []),
    insert_offs(Is, 1, Ls).

get_labels([{label,L}|Is], O, Ls) -> get_labels(Is, O, [{L,O}|Ls]);
get_labels([_|Is], O, Ls) -> get_labels(Is, O+1, Ls);
get_labels([], _, Ls) -> Ls.

insert_offs([{forloop,L}|Is], O, Ls) ->
    {_,Lo} = lists:keyfind(L, 1, Ls),		%Stupid function
    [{forloop,Lo-(O+1)}|insert_offs(Is, O+1, Ls)];	%Just jump
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

new_label(#comp{lab=L}=St) ->
    {L,St#comp{lab=L+1}}.

%% tableconstrutor(Fields, State) -> {Instrs,State}.
%%  Build the instructions to construct a table. We could be smarter
%%  here and recognise already uses keys and only actually insert the
%%  last one. Or we could pre-order the table elements so the keys are
%%  already sorted, but that would mean adding stack-ops.

tableconstructor(Fs, St0) ->
    %% N.B. this fun is for a FOLDL!!
    Fun = fun ({exp_field,_,Ve}, {Ifs,I,S0}) ->
		  {Ivs,S1} = exp(Ve, true, S0),	%Value
		  {Ifs ++ [{push,I}] ++ Ivs,I+1,S1};
	      ({name_field,_,{'NAME',_,N},Ve}, {Ifs,I,S0}) ->
		  {Ivs,S1} = exp(Ve, true, S0),	%Value
		  {Ifs ++ [name_op(push, N)] ++ Ivs,I,S1};
	      ({key_field,_,Ke,Ve}, {Ifs,I,S0}) ->
		  {Iks,S1} = exp(Ke, true, S0),	%Key
		  {Ivs,S2} = exp(Ve, true, S1),	%Value
		  {Ifs ++ Iks ++ Ivs,I,S2}
	  end,
    {Its,_,St1} = lists:foldl(Fun, {[],1.0,St0}, Fs),
    {Its,St1}.

%% name_op(Op, Name) -> Instr.
%%  We do this a lot!

name_op(Op, Name) ->
    {Op,atom_to_binary(Name, latin1)}.
