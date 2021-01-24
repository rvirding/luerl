%% Copyright (c) 2013-2019 Robert Virding
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

%% File    : luerl.hrl
%% Author  : Robert Virding
%% Purpose : The basic macros/records for Luerl.

%% We include the whole environment in one structure even if fields
%% come from logically different parts. This make it easier to pass
%% around but does mean that there will be more explicit fiddleling to
%% get it right. See block/2 and functioncall/4 for examples of this.

-record(luerl, {tabs,                           %Table table
                envs,                           %Environment table
                usds,                           %Userdata table
                fncs,                           %Function table
		g,				%Global table
		%%
		stk=[],				%Current stack
		cs=[],				%Current call stack
		%%
		meta=[],			%Data type metatables
		rand,				%Random state
                tag,                            %Unique tag
                trace_func=none,                %Trace function
                trace_data                      %Trace data
               }).

%% Table structure.
-record(tstruct, {data,                         %Data table/array
                  free,                         %Index free list
                  next                          %Next index
                 }).

%% Metatables for atomic datatypes.

-record(meta, {nil=nil,
	       boolean=nil,
	       number=nil,
	       string=nil
	      }).

%% Frames for the call stack.
%% Call return frame
-record(call_frame, {func,args,			%Function, arguments
		     lvs,			%Local variables
		     env,			%Environment
		     is=[],cont=[]		%Instructions, continuation
		    }).
%% Loop break frame
-record(loop_frame, {lvs,			%Local variables
		     stk,			%Stack
		     env,			%Environment
		     is=[],cont=[]		%Instructions, continuation
		    }).
%% Current line
-record(current_line, {line,			%Line
		       file			%File name
		      }).

%% Data types.

-record(tref, {i}).				%Table reference, index
-define(IS_TREF(T), is_record(T, tref)).

-record(table, {a,d=[],meta=nil}).		%Table type, array, dict, meta

-record(eref, {i}).				%Environment reference, index
-define(IS_EREF(E), is_record(E, eref)).

-record(usdref, {i}).                           %Userdata reference, index
-define(IS_USDREF(U), is_record(U, usdref)).

-record(userdata, {d,meta=nil}).		%Userdata type, data and meta

-record(thread, {}).				%Thread type

%% There are two function types, the Lua one, and the Erlang one.

%% The environment with upvalues is defined when the function is
%% referenced and can vary if the function is referenced many
%% times. Hence it is in the reference not in the the definition.

-record(funref, {i,env=[]}).			%Function reference
-define(IS_FUNREF(F), is_record(F, funref)).

-record(lua_func,{anno=[],			%Annotation
		  funrefs=[],			%Functions directly referenced
		  lsz,				%Local var size
		  %% loc=not_used,		%Local var block template
		  esz,				%Env var size
		  %% env=not_used,		%Local env block template
		  pars,				%Parameter types
		  b}).				%Code block
-define(IS_LUAFUNC(F), is_record(F, lua_func)).

-record(erl_func,{code}).			%Erlang code (fun)
-define(IS_ERLFUNC(F), is_record(F, erl_func)).

%% Test if it a function, of either sort.
-define(IS_FUNCTION(F), (?IS_FUNREF(F) orelse ?IS_ERLFUNC(F))).

%% Testing for integers/integer floats or booleans.
-define(IS_FLOAT_INT(N), (round(N) == N)).
-define(IS_FLOAT_INT(N,I), ((I=round(N)) == N)).
-define(IS_TRUE(X), (((X) =/= nil) and ((X) =/= false))).

%% Different methods for storing tables in the global data #luerl{}.
%% Access through macros to allow testing with different storage
%% methods. This is inefficient with ETS tables where it would
%% probably be better to use bags and acces with match/select.

%% Set which table store to use. We check if we have full maps before
%% we use them just to protect ourselves.
-ifdef(HAS_FULL_KEYS).
-define(TS_USE_MAPS, true).
-else.
-define(TS_USE_ARRAY, true).
-endif.
%% -define(TS_USE_ARRAY, true).

-ifdef(TS_USE_MAPS).
-define(MAKE_TABLE(), maps:new()).
-define(GET_TABLE(N, Ts), maps:get(N, Ts)).
-define(SET_TABLE(N, T, Ts), maps:put(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), maps:update_with(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), maps:remove(N, Ts)).
-define(FILTER_TABLES(Pred, Ts), maps:filter(Pred, Ts)).
-define(FOLD_TABLES(Fun, Acc, Ts), maps:fold(Fun, Acc, Ts)).
-endif.

-ifdef(TS_USE_ARRAY).
%% Use arrays to handle tables.
-define(MAKE_TABLE(), array:new()).
-define(GET_TABLE(N, Ar), array:get(N, Ar)).
-define(SET_TABLE(N, T, Ar), array:set(N, T, Ar)).
-define(UPD_TABLE(N, Upd, Ar),
	array:set(N, (Upd)(array:get(N, Ar)), Ar)).
-define(DEL_TABLE(N, Ar), array:reset(N, Ar)).
-define(FILTER_TABLES(Pred, Ar),
	((fun (___Def) ->
		  ___Fil = fun (___K, ___V) ->
				   case Pred(___K, ___V) of
				       true -> ___V;
				       false -> ___Def
				   end
			   end,
		  array:sparse_map(___Fil, Ar)
	  end)(array:default(Ar)))).
-define(FOLD_TABLES(Fun, Acc, Ar), array:sparse_foldl(Fun, Acc, Ar)).
-endif.

-ifdef(TS_USE_ORDDICT).
%% Using orddict to handle tables.
-define(MAKE_TABLE(), orddict:new()).
-define(GET_TABLE(N, Ts), orddict:fetch(N, Ts)).
-define(SET_TABLE(N, T, Ts), orddict:store(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), orddict:update(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), orddict:erase(N, Ts)).
-define(FILTER_TABLES(Pred, Ts), orddict:filter(Pred, Ts)).
-define(FOLD_TABLES(Fun, Acc, Ts), orddict:fold(Fun, Acc, Ts)).
-endif.

-ifdef(TS_USE_PD).
%% Use the process dictionary to handle tables.
-define(MAKE_TABLE(), ok).
-define(GET_TABLE(N, Pd), get(N)).
-define(SET_TABLE(N, T, Pd), put(N, T)).
-define(UPD_TABLE(N, Upd, Pd), put(N, (Upd)(get(N)))).
-define(DEL_TABLE(N, Pd), erase(N)).
-define(FILTER_TABLES(Pred, Pd), Pd).		%This needs work
-define(FOLD_TABLES(Fun, Acc, Pd), Pd).		%This needs work
-endif.

-ifdef(TS_USE_ETS).
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

%% Define CATCH to handle deprecated get_stacktrace/0
-ifdef(NEW_STACKTRACE).
-define(CATCH(C, E, S), C:E:S ->).
-else.
-define(CATCH(C, E, S), C:E -> S = erlang:get_stacktrace(),).
-endif.
