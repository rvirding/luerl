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

-export([chunk/1]).

-record(lint, {
	 }).

-record(comp, {locv=false,			%Local variables
	       locf=false,			%Local functions
	       bd=0,				%Current block depth
	       lint=#lint{}			%Lint data
	      }).

chunk(Code) ->
    St0 = #comp{},
    {Is,_} = block(Code, St0),
    Is.

block(Stats, St0) ->
    Locv0 = St0#comp.locv,			%"Global" locv and locf
    Locf0 = St0#comp.locf,
    Bd0 = St0#comp.bd,				%Block depth
    {Is0,St1} = stats(Stats, St0#comp{locv=false,locf=false,bd=Bd0+1}),
    %% #comp{locv=Locv1,locf=Locf1} = St1,
    Locv1 = St1#comp.locv,			%"Local" locv and locf
    Locf1 = St1#comp.locf,
    Is1  = case {Locv1,Locf1} of
	       {true,false} -> [push_env|Is0 ++ [pop_env_free]];
	       {true,true} -> [push_env|Is0 ++ [pop_env]];
	       {false,_} -> Is0
	   end,
    {Is1,St1#comp{locv=Locv0,locf=Locf0 or Locf1,bd=Bd0}}.

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
stat({functiondef,_,Fname,Ps,B}, St0) ->
    {{Locv,Locf,Is},St1} = function_block(Ps, B, St0),
    {Isvs,St2} = set_var(Fname, St1),
    {[{build_func,Locv,Locf,Is}] ++ Isvs,St2#comp{locf=true}};
stat({'if',_,Tests,Else}, St) ->
    do_if(Tests, Else, St);
stat({local,Local}, St0) ->
    {Ils,St1} = local(Local, St0),
    {Ils,St1};
stat(Exp, St0) ->
    {Ies,St1} = exp(Exp, St0),
    {Ies ++ [pop],St1}.				%Drop value

assign(Vs, Es, St0) ->
    assign_loop(Vs, length(Vs), Es, St0).

%% Not quite left-to-right, we evaluate the exps left-to-right but
%% before all the vars which we do right to left. Should split set_var
%% into two parts: everything but the last and the last.

assign_loop([V|Vs], Vc, [E|Es], St0) ->
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_loop(Vs, Vc, Es, St1),
    {Ivs,St3} = set_var(V, St2),
    {Ies ++ Iass ++ Ivs,St3};
assign_loop([V|Vs], Vc, [], St0) ->
    {Ivs,St1} = set_var(V, St0),
    {Iass,St2} = assign_loop(Vs, Vc, [], St1),
    {Iass ++ Ivs,St2};				%unpack_vals will give nil's
assign_loop([], Vc, [E|Es], St0) ->		%No more variables
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_loop([], Vc, Es, St1),
    {Ies ++ [pop] ++ Iass,St2};			%Drop value of this
assign_loop([], Vc, [], St) -> {[{unpack_vals,Vc}],St}.

set_var({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_first(Exp, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
set_var({'NAME',_,N}, St) ->
    {[{set_env,atom_to_binary(N, latin1)}],St}.

var_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',_,N}, St) ->
    {[{set_key,atom_to_binary(N, latin1)}],St};
var_last({key_field,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, St0),
    {Is ++ [set_key],St1}.
%% var_last({method,_,{'NAME',_,N}}, St0) ->
%%     %% Must fix function definition here!
%%     Is = [{push,atom_to_binary(N, latin1)},set_key],
%%     {Is,St0}.

%% do_if(Tests, Else, State) -> {Instrs,State}.

do_if(Tests, Else, St) -> {[],St}.

local({assign,_,Vs,Es}, St) ->
    assign_local_loop(Vs, length(Vs), Es, St#comp{locv=true});
local({functiondef,_,{'NAME',_,N},Ps,B}, St0) ->
    Nb = atom_to_binary(N, latin1),
    {{Locv,Locf,Is},St1} = function_block(Ps, B, St0),
    {[push_nil,{set_local,Nb},		%Make function recursive
      {build_func,Locv,Locf,Is},{set_local,Nb}],
     St1#comp{locv=true,locf=true}}.

assign_local_loop([{'NAME',_,N}|Vs], Vc, [E|Es], St0) ->
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_local_loop(Vs, Vc, Es, St1),
    {Ies ++ Iass ++ [{set_local,atom_to_binary(N, latin1)}],St2};
assign_local_loop([{'NAME',_,N}|Vs], Vc, [], St0) ->
    {Iass,St1} = assign_local_loop(Vs, Vc, [], St0),
    {Iass ++ [{set_local,atom_to_binary(N, latin1)}],St1};
assign_local_loop([], Vc, [E|Es], St0) ->	%No more variables
    {Ies,St1} = exp(E, St0),
    {Iass,St2} = assign_local_loop([], Vc, Es, St1),
    {Ies ++ [pop] ++ Iass,St2};
assign_local_loop([], Vc, [], St) -> {[{unpack_vals,Vc}],St}.

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
    {{Locv,Locf,Is},St1} = function_block(Ps, B, St0),
    {[{build_func,Locv,Locf,Is}],St1#comp{locf=true}};
exp({table,_,Fs}, St) -> {[{build_tab,length(Fs)}],St};
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
    {[{get_env,atom_to_binary(N, latin1)}],St};
prefixexp_first({single,_,E}, St0) ->
    {Is,St1} = exp(E, St0),
    {Is,St1}.
%%     {Is ++ [first_value],St1}.

prefixexp_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, St0),
    {Irs,St2} = prefixexp_rest(Rest, St1),
    {Ies ++ Irs,St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',_,N}, St) ->
    {[{get_key,atom_to_binary(N, latin1)}],St};
prefixexp_element({key_field,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, St0),
    {Is ++ [get_key],St1};
prefixexp_element({functioncall,_,Args}, St0) ->
    {Ias,St1} = explist(Args, St0),
    %% [an,...,a1,func|_] -> [as,func|_]
    {Ias ++ [{pack_vals,length(Args)},call],St1};
prefixexp_element({method,_,{'NAME',_,N},Args}, St0) ->
    %% [meth|_] -> [meth,meth|_] -> [func,meth|_] -> [meth,func|_]
    Im = [dup,{get_key,atom_to_binary(N, latin1)},swap],
    %% [meth,func|_] -> [an,..,a1,meth,func|_]
    {Ias,St1} = explist(Args, St0),
    %% [an,..,a1,meth,func|_] -> [as,func|_]
    {Im ++ Ias ++ [{pack_vals,length(Args)+1},call],St1}.

function_block(Pars, Stats, St0)->
    Args = Pars =/= [],				%Do we have pars?
    Locv0 = St0#comp.locv,			%"Global" locv and locf
    Locf0 = St0#comp.locf,
    Bd0 = St0#comp.bd,				%Block depth
    {Iss,St1} = stats(Stats, St0#comp{locv=false,locf=false,bd=0}),
    Locv1 = St1#comp.locv,			%"Local" locv and locf
    Locf1 = St1#comp.locf,
    if Args ->
	    Iup = case lists:last(Pars) of
		      {'...',_} -> {unpack_args,length(Pars)};
		      _ -> {unpack_vals,length(Pars)}
		  end,
	    Iass = assign_pars_loop(Pars),
	    Ipre = [Iup] ++ Iass;
       true -> Ipre = []
    end,
    Ipost = [{push,[]},return],
    St2 = St1#comp{locv=Locv0,locf=Locf0 or Locf1,bd=Bd0},
    {{Args or Locv1,Locf1,Ipre ++ Iss ++ Ipost},St2}.

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

assign_pars_loop([{'NAME',_,N}|Vs]) ->
    Iass = assign_pars_loop(Vs),
    Iass ++ [{set_local,atom_to_binary(N, latin1)}];
assign_pars_loop([{'...',_}]) ->		%Vararg
    [{push,'...'},set_local];
assign_pars_loop([]) -> [].
