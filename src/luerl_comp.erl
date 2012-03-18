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

-export([string/1]).

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
	       lint=#lint{}			%Lint data
	      }).

chunk(Code) ->
    St0 = #comp{},
    {Is,_} = block(Code, St0),
    Is.

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
stat({'if',_,Tests,Else}, St) ->
    do_if(Tests, Else, St);
stat({local,Local}, St0) ->
    {Ils,St1} = local(Local, St0),
    {Ils,St1};
stat(Exp, St0) ->
    {Ies,St1} = exp(Exp, St0),
    {Ies ++ [pop],St1}.				%Drop value

assign(Vs, Es, St0) ->
    assign_loop(Vs, length(Vs), Es, length(Es), St0).

%% Not quite left-to-right, we evaluate the exps left-to-right but
%% before all the vars which we do right to left. Should split set_var
%% into two parts: everything but the last and the last.

assign_loop([V|Vs], Vc, [E|Es], Ec, St0) ->
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_loop(Vs, Vc, Es, Ec, St1),
    {Ivs,St3} = set_var(V, St2),
    {Ies ++ Iass ++ Ivs,St3};
assign_loop([V|Vs], Vc, [], Ec, St0) ->
    {Ivs,St1} = set_var(V, St0),
    {Iass,St2} = assign_loop(Vs, Vc, [], Ec, St1),
    {Iass ++ Ivs,St2};				%unpack_vals will give nil's
assign_loop([], Vc, [E|Es], Ec, St0) ->		%No more variables
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_loop([], Vc, Es, Ec, St1),
    {Ies ++ Iass,St2};				%unpack_vals will pop this

%%     {Ies ++ [pop] ++ Iass,St2};			%Drop value of this
assign_loop([], Vc, [], Ec, St) -> {[{unpack_vals,Vc,Ec}],St}.

set_var({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_first(Exp, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
set_var({'NAME',_,N}, St) ->
    Set = case lists:member(N, St#comp.lvs) of	%Is it a local variable?
	      true -> set_local;
	      false -> set_env
	  end,
    {[name_op(Set, N)],St}.

var_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',_,N}, St) ->
    {[name_op(set_key, N)],St};
var_last({key_field,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, St0),
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
    {[{build_func,Locv,Locf,Is}],St1}.

is_method({'NAME',_,_}) -> no;
is_method({'.',L,N,Rest0}) ->
    case is_method(Rest0) of
	{yes,Rest1} -> {yes,{'.',L,N,Rest1}};
	no -> no				%No change
    end;
is_method({method,_,{'NAME',_,_}=N}) -> {yes,N}.

%% do_if(Tests, Else, State) -> {Instrs,State}.

do_if(Tests, Else, St) -> {[],St}.

local({assign,_,Vs,Es}, St) ->
    assign_local_loop(Vs, length(Vs), Es, length(Es), St#comp{locv=true});
local({functiondef,_,{'NAME',_,N},Ps,B}, St0) ->
    In = name_op(set_local, N),
    {Ifs,St1} = functiondef(Ps, B, St0),
    {[push_nil,In] ++ Ifs ++ [In],
     St1#comp{locv=true,lvs=[N|St0#comp.lvs],locf=true}}.

assign_local_loop([{'NAME',_,N}|Vs], Vc, [E|Es], Ec, St0) ->
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_local_loop(Vs, Vc, Es, Ec, St1),
    St3 = St2#comp{lvs=[N|St2#comp.lvs]},
    {Ies ++ Iass ++ [name_op(set_local, N)],St3};
assign_local_loop([{'NAME',_,N}|Vs], Vc, [], Ec, St0) ->
    {Iass,St1} = assign_local_loop(Vs, Vc, [], Ec, St0),
    St2 = St1#comp{lvs=[N|St1#comp.lvs]},
    {Iass ++ [name_op(set_local, N)],St2};
assign_local_loop([], Vc, [E|Es], Ec, St0) ->	%No more variables
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_local_loop([], Vc, Es, Ec, St1),
    {Ies ++ Iass,St2};				%unpack_vals will pop this
%%     {Ies ++ [pop] ++ Iass,St2};
assign_local_loop([], Vc, [], Ec, St) -> {[{unpack_vals,Vc,Ec}],St}.

explist([E], St) -> exp(E, St);			%Append values to output
explist([E|Es], St0) ->
    {Ies,St1} = exp(E, St0),
    {Iess,St2} = explist(Es, St1),
    {Ies ++ Iess,St2};
%%     {Ies ++ [first_value] ++ Iess,St2};
explist([], St) -> {[],St}.

exp({nil,_}, St) -> {[push_nil],St};
exp({false,_}, St) -> {[{push,false}],St};
exp({true,_}, St) -> {[{push,true}],St};
exp({'NUMBER',_,N}, St) -> {[{push,N}],St};
exp({'STRING',_,S}, St) -> {[{push,S}],St};
exp({'...',_}, St) ->
    {[{get_local,'...'}],St};
exp({functiondef,_,Ps,B}, St0) ->
    {Ifs,St1} = functiondef(Ps, B, St0),
    {Ifs,St1#comp{locf=true}};
exp({table,_,Fs}, St0) ->
    {Ifs,St1} = tableconstructor(Fs, St0),
    {Ifs ++ [{build_tab,length(Fs)}],St1};
exp({op,_,Op,A1,A2}, St0) ->
    {Ia1s,St1} = exp(A1, St0),
    {Ia2s,St2} = exp(A2, St1),
    {Ia1s ++ Ia2s ++ [{op2,Op}],St2};
exp({op,_,Op,A}, St0) ->
    {Ias,St1} = exp(A, St0),
    {Ias ++ [{op1,Op}],St1};
exp(E, St) ->
    prefixexp(E, St).

prefixexp({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_first(Exp, St0),
    {Irs,St2} = prefixexp_rest(Rest, St1),
    {Ies ++ Irs,St2};
prefixexp(P, St) -> prefixexp_first(P, St).

prefixexp_first({'NAME',_,N}, St) ->
    Get = case lists:member(N, St#comp.lvs) of	%Is it a local variable?
	      true -> get_local;
	      false -> get_env
	  end,
    {[name_op(Get, N)],St};
prefixexp_first({single,_,E}, St0) ->
    {Is,St1} = exp(E, St0),
    {Is,St1}.

prefixexp_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, St0),
    {Irs,St2} = prefixexp_rest(Rest, St1),
    {Ies ++ Irs,St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',_,N}, St) ->
    {[name_op(get_key, N)],St};
prefixexp_element({key_field,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, St0),
    {Is ++ [get_key],St1};
prefixexp_element({functioncall,_,Args}, St0) ->
    {Ias,St1} = explist(Args, St0),
    %% [an,...,a1,func|_] -> [as,func|_]
    {Ias ++ [{pack_vals,length(Args)},call],St1};
prefixexp_element({method,_,{'NAME',_,N},Args}, St0) ->
    %% [meth|_] -> [meth,meth|_] -> [func,meth|_] -> [meth,func|_]
    Im = [dup,name_op(get_key, N),swap],
    %% [meth,func|_] -> [an,..,a1,meth,func|_]
    {Ias,St1} = explist(Args, St0),
    %% [an,..,a1,meth,func|_] -> [as,func|_]
    {Im ++ Ias ++ [{pack_vals,length(Args)+1},call],St1}.

function_block(Pars, Stats, St0)->
    Args = Pars =/= [],				%Do we have pars?
    Locf0 = St0#comp.locf,			%"Global" locf
    St1 = St0#comp{locv=false,lvs=[],locf=false,bd=0},
    if Args ->					%Export Ipre, St2
	    Iup = case lists:last(Pars) of
		      {'...',_} -> {unpack_args,length(Pars)};
		      _ -> {unpack_vals,length(Pars),1}
		  end,
	    {Iass,St2} = assign_pars_loop(Pars, St1),
	    Ipre = [Iup] ++ Iass;
       true -> Ipre = [],
	       St2 = St1
    end,
    {Iss,St3} = stats(Stats, St2),
    Locv1 = St3#comp.locv,			%"Local" locv and locf
    Locf1 = St3#comp.locf,
    Ipost = [{push,[]},return],
    St4 = St0#comp{locf=Locf0 or Locf1},	%Use the original
    {{Args or Locv1,Locf1,Ipre ++ Iss ++ Ipost},St4}.

%% function_block() ->
%%     {Ipre,Ipost} = if Pars =:= [] -> {[],[]};
%% 		      true ->
%% 			   Iup = case lists:last(Pars) of
%% 				     {'...',_} -> {unpack_args,length(Pars)};
%% 				     _ -> {unpack_vals,length(Pars)}
%% 				 end,
%% 			   {[push_env,swap,Iup],[pop_env]}
%% 		   end,
%%     {Iass,St1} = assign_pars_loop(Pars, St0),
%%     {Ibs,St2} = block(B, St1),
%%     {{function,Ipre ++ Iass ++ Ibs ++ Ipost},St2}.

%%     Locv0 = St0#comp.locv,			%"Global" locv and locf
%%     Locf0 = St0#comp.locf,
%%     {Is0,St1} = stats(Stats, St0#comp{locv=false,locf=false}),
%%     %% #comp{locv=Locv1,locf=Locf1} = St1,
%%     Locv1 = St1#comp.locv,			%"Local" locv and locf
%%     Locf1 = St1#comp.locf,
%%     Is1  = case {Locv1,Locf1} of
%% 	       {true,false} -> [push_env|Is0 ++ [pop_env_free]];
%% 	       {true,true} -> [push_env|Is0 ++ [pop_env]];
%% 	       {false,_} -> Is0
%% 	   end,
%%     {Is1,St1#comp{locv=Locv0,locf=Locf0 or Locf1}}.

assign_pars_loop([{'NAME',_,N}|Vs], St0) ->
    {Iass,St1} = assign_pars_loop(Vs, St0),
    St2 = St1#comp{lvs=[N|St1#comp.lvs]},
    {Iass ++ [name_op(set_local, N)],St2};
assign_pars_loop([{'...',_}], St) ->		%Vararg
    {[{push,'...'},set_local],St};
assign_pars_loop([], St) -> {[],St}.

%% tableconstrutor(Fields, State) -> {Instrs,State}.
%%  Build the instructions to construct a table. We could be smarter
%%  here and recognise already uses keys and only actually insert the
%%  last one. Or we could pre-order the table elements so the keys are
%%  already sorted, but that would mean adding stack-ops.

tableconstructor(Fs, St0) ->
    %% N.B. this fun is for a FOLDL!!
    Fun = fun ({exp_field,_,Ve}, {Ifs,I,S0}) ->
		  {Ivs,S1} = exp(Ve, S0),	%Value
		  {Ifs ++ [{push,I}] ++ Ivs ++ [first_value],I+1,S1};
	      ({name_field,_,{'NAME',_,N},Ve}, {Ifs,I,S0}) ->
		  {Ivs,S1} = exp(Ve, S0),	%Value
		  {Ifs ++ [name_op(push, N)] ++ Ivs ++ [first_value],I,S1};
	      ({key_field,_,Ke,Ve}, {Ifs,I,S0}) ->
		  {Iks,S1} = exp(Ke, S0),	%Key
		  {Ivs,S2} = exp(Ve, S1),	%Value
		  {Ifs ++ Iks ++ [first_value] ++ Ivs ++ [first_value],I,S2}
	  end,
    {Its,_,St1} = lists:foldl(Fun, {[],1.0,St0}, Fs),
    {Its,St1}.

%% name_op(Op, Name) -> Instr.
%%  We do this a lot!

name_op(Op, Name) ->
    {Op,atom_to_binary(Name, latin1)}.
