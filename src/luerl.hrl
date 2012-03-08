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

%% File    : luerl.hrl
%% Author  : Robert Virding
%% Purpose : The basic macros/records for Luerl.

%% We include the whole environment in one structure even if fields
%% come from logically different parts. This make it easier to pass
%% around but does mean that there will be more explicit fiddleling to
%% get it right. See block/2 and functioncall/4 for examples of this.

-record(luerl, {tabs,free,next,			%Table structure
		meta=[],			%Data type metatables
		env,				%Environment
		locf=false,			%Started local function
		tag				%Unique tag
	       }).

%% -record(etab, {tabs=[],free=[],next=0}).	%Tables structure
%% -record(eenv, {env=[]}).			%Environment
%% -record(luerl, {tabs,env}).			%Full state

%% Metatables for atomic datatypes.

-record(meta, {number=nil,
	       string=nil,
	       userdata=nil}).

-define(IS_INTEGER(N), (float(round(N)) =:= N)).
-define(IS_TRUE(X), (((X) =/= nil) and ((X) =/= false))).

%% Set which table store to use.
-define(USE_ARRAY, true).

-ifdef(USE_ORDDICT).
%% Using orddict to handle tables.
-define(MAKE_TABLE(), orddict:new()).
-define(GET_TABLE(N, Ts), orddict:fetch(N, Ts)).
-define(SET_TABLE(N, T, Ts), orddict:store(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), orddict:update(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), orddict:erase(N, Ts)).
-define(FILTER_TABLES(Pred, Ts), orddict:filter(Pred, Ts)).
-define(FOLD_TABLES(Fun, Acc, Ts), orddict:fold(Fun, Acc, Ts)).
-endif.

-ifdef(USE_ARRAY).
%% Use arrays to handle tables.
-define(MAKE_TABLE(), array:new()).
-define(GET_TABLE(N, Ar), array:get(N, Ar)).
-define(SET_TABLE(N, T, Ar), array:set(N, T, Ar)).
-define(UPD_TABLE(N, Upd, Ar),
	array:set(N, (Upd)(array:get(N, Ar)), Ar)).
-define(DEL_TABLE(N, Ar), array:reset(N, Ar)).
-define(FILTER_TABLES(Pred, Ar),
	((fun (___Def) ->
		  Fil = fun (___K, ___V) ->
				case Pred(___K, ___V) of
				    true -> ___V;
				    false -> ___Def
				end
			end,
		  array:sparse_map(Fil, Ar)
	  end)(array:default(Ar)))).
-define(FOLD_TABLES(Fun, Acc, Ar), array:sparse_foldl(Fun, Acc, Ar)).
-endif.

-ifdef(USE_PD).
%% Use the process dictionary to handle tables.
-define(MAKE_TABLE(), ok).
-define(GET_TABLE(N, Pd), get(N)).
-define(SET_TABLE(N, T, Pd), put(N, T)).
-define(UPD_TABLE(N, Upd, Pd), put(N, (Upd)(get(N)))).
-define(DEL_TABLE(N, Pd), erase(N)).
-define(FILTER_TABLES(Pred, Pd), Pd).		%This needs work
-define(FOLD_TABLES(Fun, Acc, Pd), Pd).		%This needs work
-endif.

-ifdef(USE_ETS).
%% Use ETS to handle tables. Must get return values right!
-define(MAKE_TABLE(),ets:new(luerl_tables, [set])).
-define(GET_TABLE(N, E), ets:lookup_element(E, N, 2)).
-define(SET_TABLE(N, T, E), begin ets:insert(E, {N,T}), E end).
-define(UPD_TABLE(N, Upd, E),
	begin ets:update_element(E, N, {2,(Upd)(ets:lookup_element(E, N, 2))}),
	      E end).
-define(DEL_TABLE(N, E), begin ets:delete(E, N), E end).
-define(FILTER_TABLES(Pred, E), E).		%This needs work
-define(FOLD_TABLES(Fun, Acc, E),
	ets:foldl(fun ({___K, ___T}, ___Acc) -> Fun(___K, ___T, ___Acc) end,
		  Acc, E)).
-endif.
