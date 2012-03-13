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

-record(comp, {locv=false,			%Local variables
	       locf=false			%Local functions
	      }).

chunk(Code) ->
    St0 = #comp{},
    {Is,_} = block(Code, St0),
    Is.

block(Stats, St0) ->
    #comp{locv=LocV0,locf=LocF0} = St0,
    {Is0,St1} = stats(Stats, St0#comp{locv=false,locf=false}),
    #comp{locv=LocV1,locf=LocF1} = St1,
    Is1  = case {LocV1,LocF1} of
	       {true,false} -> [push_env|Is0 ++ [pop_env_free]];
	       {true,true} -> [push_env|Is0 ++ [pop_env]];
	       {false,_} -> Is0
	   end,
    {Is1,St1#comp{locv=LocV0}}.

stats([{block,_,B}|Ss], St0) ->
    {Ibs,St1} = block(B, St0),
    {Iss,St2} = stats(Ss, St1),
    {Ibs ++ Iss,St2};
stats([{assign,_,Vs,Es}|Ss], St0) ->
    {Ias,St1} = assign(Vs, Es, St0),
    {Iss,St2} = stats(Ss, St1),
    {Ias ++ Iss,St2};
stats([{local,Local}|Ss], St0) ->
    {Ils,St1} = local(Local, St0),
    {Iss,St2} = stats(Ss, St1),
    {Ils ++ Iss,St2};
stats([Exp|Ss], St0) ->
    {Ies,St1} = exp(Exp, St0),
    {Iss,St2} = stats(Ss, St1),
    {Ies ++ Iss,St2};
stats([], St) -> {[],St}.

assign(Vs, Es, St0) ->
    {Iess,St1} = explist(Es, St0),
    {Ivss,St2} = assign_loop(Vs, St1),
    {Iess ++ Ivss,St2}.

assign_loop([Pre|Pres], St0) ->
    {Ips,St1} = set_var(Pre, St0),
    {Ipss,St2} = assign_loop(Pres, St1),
    {Ips ++ Ipss,St2};
assign_loop([], St) -> {[],St}.

set_var({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_first(Exp, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Irs ++ Ies,St2};
set_var({'NAME',_,N}, St) ->
    {[{push,atom_to_binary(N, latin1)},set_env],St}.

var_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, St0),
    {Irs,St2} = var_rest(Rest, St1),
    {Ies ++ Irs,St2};
var_rest(Exp, St) ->
    var_last(Exp, St).

var_last({'NAME',_,N}, St) ->
    {[{push,atom_to_binary(N, latin1)},set_key],St};
var_last({keyfield,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, St0),
    {Is ++ [set_key],St1}.
%% var_last({method,_,{'NAME',_,N}}, St0) ->
%%     {Ies,St1} = explist(Args, St0),
%%     {Ies ++ [{build_args,length(Args)},call],St1}.

local({assign,_,Vs,Es}, St0) ->
    {Iess,St1} = explist(Es, St0),
    {Ivss,St2} = assign_local_loop(Vs, St1),
    {Iess ++ Ivss,St2#comp{locv=true}}.

assign_local_loop([{'NAME',_,N}|Vs], St0) ->
    {Ivss,St1} = assign_local_loop(Vs, St0),
    {Ivss ++ [{push,atom_to_binary(N, latin1)},set_local],St1};
assign_local_loop([], St) -> {[],St}.

explist([E], St) -> exp(E, St);			%Append values to output
explist([E|Es], St0) ->
    {Ies,St1} = exp(E, St0),
    {Iess,St2} = explist(Es, St1),
    {Ies ++ [first_value] ++ Iess,St2};
explist([], St) -> {[],St}.

exp({nil,_}, St) -> {[{push,nil}],St};
exp({false,_}, St) -> {[{push,false}],St};
exp({true,_}, St) -> {[{push,true}],St};
exp({'NUMBER',_,N}, St) -> {[{push,N}],St};
exp({'STRING',_,S}, St) -> {[{push,S}],St};
exp({table,_,Fs}, St) -> {[{build_tab,length(Fs)}],St};
exp(E, St) ->
    prefixexp(E, St).

prefixexp({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_first(Exp, St0),
    {Irs,St2} = prefixexp_rest(Rest, St1),
    {Ies ++ Irs,St2};
prefixexp(P, St) -> prefixexp_first(P, St).

prefixexp_first({'NAME',_,N}, St) ->
    {[{push,atom_to_binary(N, latin1)},get_env],St};
prefixexp_first({single,_,E}, St0) ->
    {Is,St1} = exp(E, St0),
    {Is ++ [first_value],St1}.

prefixexp_rest({'.',_,Exp,Rest}, St0) ->
    {Ies,St1} = prefixexp_element(Exp, St0),
    {Irs,St2} = prefixexp_rest(Rest, St1),
    {Ies ++ Irs,St2};
prefixexp_rest(Exp, St) ->
    prefixexp_element(Exp, St).

prefixexp_element({'NAME',_,N}, St) ->
    {[{push,atom_to_binary(N, latin1)},get_key],St};
prefixexp_element({keyfield,_,Exp}, St0) ->
    {Is,St1} = exp(Exp, St0),
    {Is ++ [get_key],St1};
prefixexp_element({functioncall,_,Args}, St0) ->
    {Ies,St1} = explist(Args, St0),
    {Ies ++ [{build_args,length(Args)},call],St1}.
