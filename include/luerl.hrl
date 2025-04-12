%% Copyright (c) 2013-2024 Robert Virding
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

%Table table
-record(luerl, {
    tabs,
    %Environment table
    envs,
    %Userdata table
    usds,
    %Function table
    fncs,
    %Global table
    g,
    %%

    %Current stack
    stk = [],
    %Current call stack
    cs = [],
    %%

    %Data type metatables
    meta = [],
    %Random state
    rand,
    %Unique tag
    tag,
    %Trace function
    trace_func = none,
    %Trace data
    trace_data,
    private = #{}
}).

%% Table structure.

%Data table/array
-record(tstruct, {
    data,
    %Index free list
    free,
    %Next index
    next
}).

%% Metatables for atomic datatypes.

-record(meta, {
    nil = nil,
    boolean = nil,
    number = nil,
    string = nil
}).

%% Frames for the call stack.
%% Call return frame

%Function, arguments
-record(call_frame, {
    func,
    args,
    %Local variables
    lvs,
    %Environment
    env,
    %Instructions
    is = [],
    %Continuation
    cont = []
}).
%% Loop break frame

%Local variables
-record(loop_frame, {
    lvs,
    %Stack
    stk,
    %Environment
    env,
    %Instructions
    is = [],
    %Continuation
    cont = []
}).
%% Current line

%Line
-record(current_line, {
    line,
    %File name
    file
}).

%% Return

%Return values
-record(return, {rets}).

%% Data types.

%Table reference, index
-record(tref, {i}).
-define(IS_TREF(T), is_record(T, tref)).

%Table type, array, dict, meta
-record(table, {a, d = [], meta = nil}).

%Environment reference, index
-record(eref, {i}).
-define(IS_EREF(E), is_record(E, eref)).

%Userdata reference, index
-record(usdref, {i}).
-define(IS_USDREF(U), is_record(U, usdref)).

%Userdata type, data and meta
-record(userdata, {d, meta = nil}).

%Thread type
-record(thread, {}).

%% There are two function types, the Lua one, and the Erlang one.

%% The environment with upvalues is defined when the function is
%% referenced and can vary if the function is referenced many
%% times. Hence it is in the reference not in the the definition.

%Function reference
-record(funref, {i, env = []}).
-define(IS_FUNREF(F), is_record(F, funref)).

%Annotation
-record(lua_func, {
    anno = [],
    %Functions directly referenced
    funrefs = [],
    %Local var size
    lsz,
    %% loc=not_used,		%Local var block template

    %Env var size
    esz,
    %% env=not_used,		%Local env block template

    %Parameter types
    pars,
    %Code block
    body
}).
-define(IS_LUAFUNC(F), is_record(F, lua_func)).

%Erlang code (fun)
-record(erl_func, {code}).
-define(IS_ERLFUNC(F), is_record(F, erl_func)).

%Erlang code (MFA)
-record(erl_mfa, {m, f, a}).
-define(IS_ERLMFA(F), is_record(F, erl_mfa)).

%% Test if it a function, of either sort.
-define(IS_FUNCTION(F), (?IS_FUNREF(F) orelse ?IS_ERLFUNC(F) orelse ?IS_ERLMFA(F))).

%% Testing for integers/integer floats or booleans.
-define(IS_FLOAT_INT(N), (round(N) == N)).
-define(IS_FLOAT_INT(N, I), ((I = round(N)) == N)).
-define(IS_TRUE(X), (((X) =/= nil) and ((X) =/= false))).

%% Different methods for storing tables in the global data #luerl{}.
%% Access through macros to allow testing with different storage
%% methods. This is inefficient with ETS tables where it would
%% probably be better to use bags and access with match/select.

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
-define(CHK_TABLE(N, Ts), maps:is_key(N, Ts)).
-define(FILTER_TABLES(Pred, Ts), maps:filter(Pred, Ts)).
-define(FOLD_TABLES(Fun, Acc, Ts), maps:fold(Fun, Acc, Ts)).
-endif.

-ifdef(TS_USE_ARRAY).
%% Use arrays to handle tables. We leave the default value as undefined.
-define(MAKE_TABLE(), array:new()).
-define(GET_TABLE(N, Ar), array:get(N, Ar)).
-define(SET_TABLE(N, T, Ar), array:set(N, T, Ar)).
-define(UPD_TABLE(N, Upd, Ar),
    array:set(N, (Upd)(array:get(N, Ar)), Ar)
).
-define(DEL_TABLE(N, Ar), array:reset(N, Ar)).
-define(CHK_TABLE(N, Ar),
    ((N >= 0) andalso (array:get(N, Ar) =/= undefined))
).
-define(FILTER_TABLES(Pred, Ar),
    ((fun(___Def) ->
        ___Fil = fun(___K, ___V) ->
            case Pred(___K, ___V) of
                true -> ___V;
                false -> ___Def
            end
        end,
        array:sparse_map(___Fil, Ar)
    end)(
        array:default(Ar)
    ))
).
-define(FOLD_TABLES(Fun, Acc, Ar), array:sparse_foldl(Fun, Acc, Ar)).
-endif.

-ifdef(TS_USE_ORDDICT).
%% Using orddict to handle tables.
-define(MAKE_TABLE(), orddict:new()).
-define(GET_TABLE(N, Ts), orddict:fetch(N, Ts)).
-define(SET_TABLE(N, T, Ts), orddict:store(N, T, Ts)).
-define(UPD_TABLE(N, Upd, Ts), orddict:update(N, Upd, Ts)).
-define(DEL_TABLE(N, Ts), orddict:erase(N, Ts)).
-define(CHK_TABLE(N, Ts), orddict:is_key(N, Ts)).
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
-define(CHK_TABLE(N, Pd), (get(N) =/= undefined)).
%This needs work
-define(FILTER_TABLES(Pred, Pd), Pd).
%This needs work
-define(FOLD_TABLES(Fun, Acc, Pd), Pd).
-endif.

-ifdef(TS_USE_ETS).
%% Use ETS to handle tables. Must get return values right!
-define(MAKE_TABLE(), ets:new(luerl_tables, [set])).
-define(GET_TABLE(N, E), ets:lookup_element(E, N, 2)).
-define(SET_TABLE(N, T, E), begin
    ets:insert(E, {N, T}),
    E
end).
-define(UPD_TABLE(N, Upd, E), begin
    ets:update_element(E, N, {2, (Upd)(ets:lookup_element(E, N, 2))}),
    E
end).
-define(DEL_TABLE(N, E), begin
    ets:delete(E, N),
    E
end).
%This needs work
-define(FILTER_TABLES(Pred, E), E).
-define(FOLD_TABLES(Fun, Acc, E),
    ets:foldl(
        fun({___K, ___T}, ___Acc) -> Fun(___K, ___T, ___Acc) end,
        Acc,
        E
    )
).
-endif.

%% Define CATCH to handle deprecated get_stacktrace/0
-ifdef(NEW_STACKTRACE).
-define(CATCH(C, E, S), C:E:S ->).
-else.
-define(CATCH(C, E, S), C:E -> S = erlang:get_stacktrace(),).
-endif.
