%% Copyright (c) 2020-2024 Robert Virding
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

%% File    : luerl_heap.erl
%% Author  : Robert Virding
%% Purpose : Implements the heap section of the Luerl state.
%%
%% Note that here we only handle the data in the heap and never call
%% anything in either Luerl or Erlang. Those cases where this could
%% happen we return values informing the caller to do it. For example
%% in the set_table_key and get_table_key functions.

-module(luerl_heap).


-include("luerl.hrl").

?MODULEDOC(false).

-export([init/0]).

%% External interface.
-export([gc/1,
         alloc_table/1,alloc_table/2,free_table/2,
         get_table/2,set_table/3,upd_table/3,chk_table/2,
         get_global_key/2,set_global_key/3,
         get_table_key/3,set_table_key/4,
         raw_get_table_key/3,raw_set_table_key/4,
         alloc_userdata/2,alloc_userdata/3,free_userdata/2,
         get_userdata/2,set_userdata/3,upd_userdata/3,chk_userdata/2,
         set_userdata_data/3,get_userdata_data/2,
         alloc_funcdef/2,get_funcdef/2,set_funcdef/3,
         alloc_environment/2,get_env_var/3,set_env_var/4,
         get_metamethod/3,get_metamethod/4,
         get_metatable/2, set_metatable/3
        ]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).

%% init() -> State
%%
%% Initialise the heap section of the state and return the state.

init() ->
    St0 = #luerl{meta=#meta{},tag=make_ref()},
    init_tables(St0).

init_tables(St) ->
    %% Initialise the table handling.
    Tst = init_tstruct(),
    %% Initialise the environment handling.
    Est = init_tstruct(),
    %% Initialise the userdata handling.
    Ust = init_tstruct(),
    %% Initialise the function def handling.
    Fst = init_tstruct(),
    St#luerl{tabs=Tst,envs=Est,usds=Ust,fncs=Fst}.

%% init_tstruct() -> #tstruct{}.
%% alloc_tstruct(Val, #tstruct{}) -> {Index,#tstruct{}}.
%% set_tstruct(Index, Val, #tstruct{}) -> #tstruct{}.
%% upd_tstruct(Index, UpdFun, #tstruct{}) -> #tstruct{}.
%% del_tstruct(Index, #tstruct{}) -> #tstruct{}.
%% chk_tstruct(Index, #tstruct{}) -> ok | error.
%%
%%  Functions for accessing tstructs.

init_tstruct() ->
    #tstruct{data=?MAKE_TABLE(),free=[],next=0}.

alloc_tstruct(Val, #tstruct{data=D0,free=[N|Ns]}=Tstr) ->
    D1 = ?SET_TABLE(N, Val, D0),
    {N,Tstr#tstruct{data=D1,free=Ns}};
alloc_tstruct(Val, #tstruct{data=D0,free=[],next=N}=Tstr) ->
    D1 = ?SET_TABLE(N, Val, D0),
    {N,Tstr#tstruct{data=D1,next=N+1}}.

set_tstruct(N, Val, #tstruct{data=D0}=Tstr) ->
    D1 = ?SET_TABLE(N, Val, D0),
    Tstr#tstruct{data=D1}.

upd_tstruct(N, Upd, #tstruct{data=D0}=Tstr) ->
    D1 = ?UPD_TABLE(N, Upd, D0),
    Tstr#tstruct{data=D1}.

del_tstruct(N, #tstruct{data=D0,free=Ns}=Tstr) ->
    D1 = ?DEL_TABLE(N, D0),
    Tstr#tstruct{data=D1,free=[N|Ns]}.

-compile({inline,[get_tstruct/2]}).             %Such a simple function
get_tstruct(N, Tstr) ->
    ?GET_TABLE(N, Tstr#tstruct.data).

chk_tstruct(N, Tstr) ->
    case ?CHK_TABLE(N, Tstr#tstruct.data) of
        true -> ok;
        false -> error
    end.

%% alloc_table(State) -> {Tref,State}
%%
%% Allocate an empty table.

alloc_table(St) -> alloc_table([], St).

%% alloc_table(InitialTable, State) -> {Tref,State}
%%
%% The InitialTable is [{Key,Value}] or map, there is no longer any
%% need to have it as an orddict.

alloc_table(Itab, #luerl{tabs=Tst0}=St) ->
    Tab = create_table(Itab),
    {N,Tst1} = alloc_tstruct(Tab, Tst0),
    {#tref{i=N},St#luerl{tabs=Tst1}}.

create_table(Itab) when is_map(Itab) ->
    create_table(maps:to_list(Itab));
create_table(Itab) ->
    D0 = ttdict:new(),
    A0 = array:new([{default,nil}]),            %Arrays with 'nil' as default
    Init = fun ({_,nil}, {D,A}) -> {D,A};       %Ignore nil values
               ({K,V}, {D,A}) when is_integer(K), K >= 1 ->
                   {D,array:set(K, V, A)};
               ({K,V}, {D,A}) when is_float(K) ->
                   case ?IS_FLOAT_INT(K, I) of
                       true when I >= 1 -> {D,array:set(I, V, A)};
                       _NegFalse -> {ttdict:store(K, V, D),A}
                   end;
               ({K,V}, {D,A}) -> {ttdict:store(K, V, D),A}
           end,
    {D1,A1} = lists:foldl(Init, {D0,A0}, Itab),
    #table{a=A1,d=D1,meta=nil}.

%% free_table(Tref, State) -> State
%%
%% Delete a table freeing its space.

free_table(#tref{i=N}, #luerl{tabs=Tst0}=St) ->
    Tst1 = del_tstruct(N, Tst0),
    St#luerl{tabs=Tst1}.

%% get_table(Tref, State) -> Table
%%
%% Get the table referred to by Tref.

get_table(#tref{i=N}, #luerl{tabs=Tst}) ->
    get_tstruct(N, Tst).

%% set_table(Tref, Table, State) -> State
%%
%% Set a new table at the location referred to by Tref
%% overwriting the existing one.

set_table(#tref{i=N}, Tab, #luerl{tabs=Tst0}=St) ->
    Tst1 = set_tstruct(N, Tab, Tst0),
    St#luerl{tabs=Tst1}.

%% upd_table(Tref, Fun, State) -> State
%%
%% Update the table at the location referred to by Tref.

upd_table(#tref{i=N}, Upd, #luerl{tabs=Tst0}=St) ->
    Tst1 = upd_tstruct(N, Upd, Tst0),
    St#luerl{tabs=Tst1}.

%% chk_table(Tref, State) -> ok | error.
%%
%% Check the table referenced by Tref actually exists.

chk_table(#tref{i=N}, #luerl{tabs=Tst}) ->
    chk_tstruct(N, Tst).

%% set_global_key(Key, Value, State) ->
%%     {value,Value,State} | {meta,Method,Args,State} | {error,Error,State}
%%
%% Set a key in the global name table _G to value.

set_global_key(Key, Val, #luerl{g=G}=St) ->
    set_table_key(G, Key, Val, St).

%% get_global_key(Key, State) ->
%%     {value,Value,State} | {meta,Method,Args,State} | {error,Error,State}
%%
%% Get the value of a key in the global name table, _G.

get_global_key(Key, #luerl{g=G}=St) ->
    get_table_key(G, Key, St).

%% set_table_key(Table, Key, Val, State) ->
%%     {value,Value,State} | {meta,Method,Args,State} | {error,Error,State}
%%
%% We don't make calls to meta methods or generate errors but
%% return a value indicating this. Setting a value to 'nil' will clear
%% it from the table and the array. We won't add a nil value.

set_table_key(#tref{}=Tref, Key, Val, St) when is_integer(Key), Key >= 1 ->
    set_table_key_int(Tref, Key, Key, Val, St);
set_table_key(#tref{}=Tref, Key, Val, St) when is_float(Key) ->
    case ?IS_FLOAT_INT(Key, I) of
        true when I >= 1 -> set_table_key_int(Tref, Key, I, Val, St);
        _NegFalse -> set_table_key_key(Tref, Key, Val, St)
    end;
set_table_key(Tab, nil=Key, _, St) ->
    {error,{illegal_index,Tab,Key},St};
set_table_key(#tref{}=Tref, Key, Val, St) ->
    set_table_key_key(Tref, Key, Val, St);
set_table_key(Other, Key, Val, St) ->
    Meta = get_metamethod(Other, <<"__newindex">>, St),
    %% io:format("stk ~p ~p ~p -> ~p\n", [Other,Key,Val,aMeta]),
    case Meta of
        nil ->
            {error,{illegal_index,Other,Key},St};
        Meth when ?IS_FUNCTION(Meth) ->
            {meta,Meth,[Other,Key,Val],St};
        Meth ->                                 %Recurse down the metatable
            set_table_key(Meth, Key, Val, St)
    end.

set_table_key_key(#tref{i=N}=Tab, Key, Val, #luerl{tabs=Tst0}=St) ->
    Ts0 = Tst0#tstruct.data,
    #table{d=Dict0,meta=Meta}=T = ?GET_TABLE(N, Ts0),
    case ttdict:find(Key, Dict0) of
        {ok,_} ->                               %Key exists
            Dict1 = if Val =:= nil -> ttdict:erase(Key, Dict0);
                       true -> ttdict:store(Key, Val, Dict0)
                    end,
            Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
            Tst1 = Tst0#tstruct{data=Ts1},
            {value,[],St#luerl{tabs=Tst1}};
        error ->                                %Key does not exist
            case get_metamethod_tab(Meta, <<"__newindex">>, Ts0) of
                nil ->
                    %% Only add non-nil value.
                    Dict1 = if Val =:= nil -> Dict0;
                               true -> ttdict:store(Key, Val, Dict0)
                            end,
                    Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
                    Tst1 = Tst0#tstruct{data=Ts1},
                    {value,[],St#luerl{tabs=Tst1}};
                Meth when ?IS_FUNCTION(Meth) ->
                    {meta,Meth,[Tab,Key,Val],St};
                Meth -> set_table_key(Meth, Key, Val, St)
            end
    end.

set_table_key_int(#tref{i=N}=Tab, Key, I, Val, #luerl{tabs=Tst0}=St) ->
    Ts0 = Tst0#tstruct.data,
    #table{a=Arr0,meta=Meta}=T = ?GET_TABLE(N, Ts0),
    case array:get(I, Arr0) of
        nil ->                                  %Key does not exist
            case get_metamethod_tab(Meta, <<"__newindex">>, Ts0) of
                nil ->
                    %% Only add non-nil value, slightly faster (?)
                    Arr1 = if Val =:= nil -> Arr0;
                              true -> array:set(I, Val, Arr0)
                           end,
                    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
                    Tst1 = Tst0#tstruct{data=Ts1},
                    {value,[],St#luerl{tabs=Tst1}};
                Meth when ?IS_FUNCTION(Meth) ->
                    {meta,Meth,[Tab,Key,Val],St};
                Meth -> set_table_key(Meth, Key, Val, St)
            end;
        _ ->                                    %Key exists
            %% Can do this as 'nil' is default value of array.
            Arr1 = array:set(I, Val, Arr0),
            Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
            Tst1 = Tst0#tstruct{data=Ts1},
            {value,[],St#luerl{tabs=Tst1}}
    end.

%% get_table_key(Table, Key, State) ->
%%     {value,Value,State} | {meta,Method,Args,State} | {error,Error,State}
%%
%% We don't make calls to meta methods or generate errors but
%% return value indicating this.

get_table_key(#tref{}=Tref, Key, St) when is_integer(Key), Key >= 1 ->
    get_table_key_int(Tref, Key, Key, St);
get_table_key(#tref{}=Tref, Key, St) when is_float(Key) ->
    case ?IS_FLOAT_INT(Key, I) of
        true when I >= 1 -> get_table_key_int(Tref, Key, I, St);
        _NegFalse -> get_table_key_key(Tref, Key, St)
    end;
get_table_key(#tref{}=Tref, Key, St) ->
    get_table_key_key(Tref, Key, St);
get_table_key(Other, Key, St) ->                %Just find the metamethod
    Meta = get_metamethod(Other, <<"__index">>, St),
    %% io:format("gtk ~p ~p -> ~p\n", [Other,Key,Meta]),
    case Meta of
        nil ->
            {error,{illegal_index,Other,Key},St};
        Meth when ?IS_FUNCTION(Meth) ->
            {meta,Meth,[Other,Key],St};
        Meth ->                                 %Recurse down the metatable
            get_table_key(Meth, Key, St)
    end.

get_table_key_key(#tref{i=N}=Tab, Key, #luerl{tabs=#tstruct{data=Ts}}=St) ->
    #table{d=Dict,meta=Meta} = ?GET_TABLE(N, Ts),
    case ttdict:find(Key, Dict) of
        {ok,Val} -> {value,Val,St};
        error ->
            %% Key not present so try metamethod
            get_table_key_metamethod(Tab, Meta, Key, Ts, St)
    end.

get_table_key_int(#tref{i=N}=T, Key, I, #luerl{tabs=#tstruct{data=Ts}}=St) ->
    #table{a=A,meta=Meta} = ?GET_TABLE(N, Ts),  %Get the table.
    case array:get(I, A) of
        nil ->
            %% Key not present so try metamethod
            get_table_key_metamethod(T, Meta, Key, Ts, St);
        Val -> {value,Val,St}
    end.

get_table_key_metamethod(Tab, Meta, Key, Ts, St) ->
    case get_metamethod_tab(Meta, <<"__index">>, Ts) of
        nil -> {value,nil,St};
        Meth when ?IS_FUNCTION(Meth) ->
            {meta,Meth,[Tab,Key],St};
        Meth ->                         %Recurse down the metatable
            get_table_key(Meth, Key, St)
    end.

%% raw_get_table_key(Table, Key, State) -> Value.
%% raw_set_table_key(Table, Key, Value, State) -> State.
%%
%% Get/set key values in tables without metamethods.

raw_get_table_key(#tref{i=N}, Key, #luerl{tabs=Tst})
  when is_integer(Key), Key >= 1 ->
    raw_get_table_key_int(N, Key, Tst);
raw_get_table_key(#tref{i=N}, Key, #luerl{tabs=Tst})
  when is_float(Key) ->
    case ?IS_FLOAT_INT(Key, I) of
        true when I >= 1 ->
            raw_get_table_key_int(N, I, Tst);
        _NegFalse ->
            raw_get_table_key_key(N, Key, Tst)
    end;
raw_get_table_key(#tref{i=N}, Key, #luerl{tabs=Tst}) ->
    raw_get_table_key_key(N, Key, Tst).

raw_get_table_key_key(N, Key, Tst) ->
    #table{d=Dict} = get_tstruct(N, Tst),
    case ttdict:find(Key, Dict) of
        {ok,Val} -> Val;
        error -> nil
    end.

raw_get_table_key_int(N, Key, Tst) ->
    #table{a=Arr} = get_tstruct(N, Tst),
    array:get(Key, Arr).

raw_set_table_key(#tref{}=Tref, Key, Val, #luerl{tabs=Tst0}=St)
  when is_integer(Key), Key >= 1 ->
    Tst1 = raw_set_table_key_int(Tref, Key, Val, Tst0),
    St#luerl{tabs=Tst1};
raw_set_table_key(#tref{}=Tref, Key, Val, #luerl{tabs=Tst0}=St)
  when is_float(Key) ->
    Tst1 = case ?IS_FLOAT_INT(Key, I) of
               true when I >= 1 ->
                   raw_set_table_key_int(Tref, I, Val, Tst0);
               _NegFalse ->
                   raw_set_table_key_key(Tref, Key, Val, Tst0)
           end,
    St#luerl{tabs=Tst1};
raw_set_table_key(#tref{}=Tref, Key, Val, #luerl{tabs=Tst0}=St) ->
    Tst1 = raw_set_table_key_key(Tref, Key, Val, Tst0),
    St#luerl{tabs=Tst1}.

raw_set_table_key_key(#tref{i=N}, Key, Val, Tst0) ->
    Fun = fun (#table{d=Dict0}=Tab) ->
                  Dict1 = if Val =:= nil -> ttdict:erase(Key, Dict0);
                             true -> ttdict:store(Key, Val, Dict0)
                          end,
                  Tab#table{d=Dict1}
          end,
    upd_tstruct(N, Fun, Tst0).

raw_set_table_key_int(#tref{i=N}, Key, Val, Tst0) ->
    Fun = fun (#table{a=Arr0}=Tab) ->
                  %% Default array value is nil.
                  Arr1 = array:set(Key, Val, Arr0),
                  Tab#table{a=Arr1}
          end,
    upd_tstruct(N, Fun, Tst0).

%% alloc_userdata(Data, State) -> {Usdref,State}
%%
%% Allocate userdata with empty metadata.

alloc_userdata(Data, St) ->
    alloc_userdata(Data, nil, St).

%% alloc_userdata(Data, Meta, State) -> {Usdref,State}.
%%
%% Allocate userdata setting its metadata.

alloc_userdata(Data, Meta, #luerl{usds=Ust0}=St) ->
    Ud = #userdata{d=Data,meta=Meta},
    {N,Ust1} = alloc_tstruct(Ud, Ust0),
    {#usdref{i=N},St#luerl{usds=Ust1}}.

%% free_userdata(Usdref, State) -> State
%%
%% Delete a table freeing its space.

free_userdata(#usdref{i=N}, #luerl{usds=Ust0}=St) ->
    Ust1 = del_tstruct(N, Ust0),
    St#luerl{usds=Ust1}.

%% get_userdata(Usdref, State) -> {UserData,State}
%%
%% Get the userdata refered to by Usdref,

get_userdata(#usdref{i=N}, #luerl{usds=Ust}=St) ->
    #userdata{} = Udata = get_tstruct(N, Ust),
    {Udata,St}.

%% set_userdata(Usdref, Data, State) -> State
%%
%% Set a new userdata at the location referred to by Usdref
%% overwriting the existing one.

set_userdata(#usdref{i=N}, #userdata{}=Udata, #luerl{usds=Ust0}=St) ->
    Ust1 = set_tstruct(N, Udata, Ust0),
    St#luerl{usds=Ust1}.

%% upd_userdata(Usdref, Fun, State) -> State
%%
%% Update the data in the userdata referred to by Usdref.

upd_userdata(#usdref{i=N}, Upd, #luerl{usds=Ust0}=St) ->
    Ust1 = upd_tstruct(N, Upd, Ust0),
    St#luerl{usds=Ust1}.

%% chk_userdata(Usdref, State) -> ok | error.
%%
%% Check the userdata referenced by Tref actually exists.

chk_userdata(#usdref{i=N}, #luerl{usds=Ust}) ->
    chk_tstruct(N, Ust).

%% get_userdata_data(Usdref, State) -> {Data,State}
%%
%% Get the data form the userdata refered to by Usdref.

get_userdata_data(#usdref{i=N}, #luerl{usds=Ust}=St) ->
    Udata = get_tstruct(N, Ust),
    {Udata#userdata.d,St}.

%% set_userdata_data(Usdref, Data, State) -> State
%%
%% Set a new userdata at the location referred to by Usdref
%% overwriting the existing one.

set_userdata_data(#usdref{i=N}, Data, #luerl{usds=Ust0}=St) ->
    Ust1 = upd_tstruct(N, fun (Ud) -> Ud#userdata{d=Data} end, Ust0),
    St#luerl{usds=Ust1}.

%% make_userdata(Data) -> make_userdata(Data, nil).
%% make_userdata(Data, Meta) -> #userdata{d=Data,meta=Meta}.

%% alloc_funcdef(Def, State) -> {FunRef,State}

alloc_funcdef(Func, #luerl{fncs=Fst0}=St) ->
    {N,Fst1} = alloc_tstruct(Func, Fst0),
    {#funref{i=N},St#luerl{fncs=Fst1}}.

%% get_funcdef(Funref, State) -> {Fdef,State}
%%
%% Get the function data referred to by Fref.

get_funcdef(#funref{i=N}, #luerl{fncs=Fst}=St) ->
    Fdef = get_tstruct(N, Fst),
    {Fdef,St}.

%% set_funcdef(Funref, Fdef, State) -> State.
%%
%% Set the function data referred to by Fref.

set_funcdef(#funref{i=N}, Func, #luerl{fncs=Fst0}=St) ->
    Fst1 = set_tstruct(N, Func, Fst0),
    St#luerl{fncs=Fst1}.

%% get_metamethod(Object, Event, State) -> Method | nil
%% get_metamethod(Object1, Object2, Event, State) -> Method | nil
%%
%% Get the metamethod for object(s).

get_metamethod(O1, O2, E, St) ->
    case get_metamethod(O1, E, St) of
        nil -> get_metamethod(O2, E, St);
        M -> M
    end.

get_metamethod(O, E, St) ->
    Meta = get_metatable(O, St),                %Can be nil
    %% io:format("gm ~p ~p -> ~p\n", [O,E,Meta]),
    get_metamethod_tab(Meta, E, St#luerl.tabs#tstruct.data).

get_metamethod_tab(#tref{i=M}, E, Ts) ->
    #table{d=Mdict} = ?GET_TABLE(M, Ts),
    case ttdict:find(E, Mdict) of
        {ok,Mm} -> Mm;
        error -> nil
    end;
get_metamethod_tab(_, _, _) -> nil.             %Other types have no metatables

%% get_metatable(Obj, State) -> MetaTable
%%
%% Get the metatable of an object or its type metatable.

get_metatable(#tref{i=T}, #luerl{tabs=Tst}) ->
    (get_tstruct(T, Tst))#table.meta;
get_metatable(#usdref{i=U}, #luerl{usds=Ust}) ->
    (get_tstruct(U, Ust))#userdata.meta;
get_metatable(nil, #luerl{meta=Meta}) -> Meta#meta.nil;
get_metatable(B, #luerl{meta=Meta}) when is_boolean(B) ->
    Meta#meta.boolean;
get_metatable(N, #luerl{meta=Meta}) when is_number(N) ->
    Meta#meta.number;
get_metatable(S, #luerl{meta=Meta}) when is_binary(S) ->
    Meta#meta.string;
get_metatable(_, _) -> nil.                     %Other types have no metatables

%% set_metatable(Obj, MetaTable, State) -> State
%%
%% Set the metatable of an object or its type metatable.

set_metatable(#tref{i=N}, M, #luerl{tabs=Tst0}=St) ->
    Tst1 = upd_tstruct(N, fun (Tab) -> Tab#table{meta=M} end, Tst0),
    St#luerl{tabs=Tst1};
set_metatable(#usdref{i=N}, M, #luerl{usds=Ust0}=St) ->
    Ust1 = upd_tstruct(N, fun (Ud) -> Ud#userdata{meta=M} end, Ust0),
    St#luerl{usds=Ust1};
set_metatable(nil, M, #luerl{meta=Meta0}=St) ->
    Meta1 = Meta0#meta{nil=M},
    St#luerl{meta=Meta1};
set_metatable(B, M, #luerl{meta=Meta0}=St) when is_boolean(B) ->
    Meta1 = Meta0#meta{boolean=M},
    St#luerl{meta=Meta1};
set_metatable(N, M, #luerl{meta=Meta0}=St) when is_number(N) ->
    Meta1 = Meta0#meta{number=M},
    St#luerl{meta=Meta1};
set_metatable(B, M, #luerl{meta=Meta0}=St) when is_binary(B) ->
    Meta1 = Meta0#meta{string=M},
    St#luerl{meta=Meta1};
set_metatable(_, _, St) ->                      %Do nothing for the rest
    St.

%% alloc_environment(Size, State) -> {Fref,State}
%%
%% Allocate the environment in the environment table and return
%% its eref.

alloc_environment(Size, #luerl{envs=Est0}=St) ->
    Fr = erlang:make_tuple(Size, nil),
    {N,Est1} = alloc_tstruct(Fr, Est0),
    {#eref{i=N},St#luerl{envs=Est1}}.

%% get_env_var(Eref, Index, State) -> Value.
%% set_env_var(Eref, Index, Val, State) -> State.

get_env_var(#eref{i=N}, Index, #luerl{envs=Etab}) ->
    element(Index, get_tstruct(N, Etab)).
%%     element(Index, ?GET_TABLE(N, Etab#tstruct.data)).

set_env_var(#eref{i=N}, Index, Val, #luerl{envs=Est0}=St) ->
    Est1 = upd_tstruct(N, fun (Fr) -> setelement(Index, Fr, Val) end, Est0),
    St#luerl{envs=Est1}.

%% gc(State) -> State.
%%  The garbage collector. Its main job is to reclaim unused tables
%%  and frames. It is a mark/sweep collector which passes over all
%%  objects and marks tables and frames which it has seen. All unseen
%%  tables and frames are then freed and their indexes added to the
%%  free lists.

-record(gct, {t,s}).                            %Gc table info table, seen

gc(#luerl{tabs=#tstruct{data=Tt0,free=Tf0}=Tab0,
          envs=#tstruct{data=Et0,free=Ef0}=Env0,
          usds=#tstruct{data=Ut0,free=Uf0}=Usd0,
          fncs=#tstruct{data=Ft0,free=Ff0}=Fnc0,
          g=G,stk=Stk,cs=Cs,meta=Meta}=St) ->
    %% The root set consisting of global table and stack.
    Root = [Meta#meta.nil,Meta#meta.boolean,Meta#meta.number,Meta#meta.string,
            G|Stk],
    %% Mark all seen tables and frames, i.e. return them.
    GcT = #gct{t=Tt0,s=[]},
    GcE = #gct{t=Et0,s=[]},
    GcU = #gct{t=Ut0,s=[]},
    GcF = #gct{t=Ft0,s=[]},
    {SeenT,SeenE,SeenU,SeenF} = mark(Root, [Cs], GcT, GcE, GcU, GcF),
    %% io:format("gc: ~p\n", [{SeenT,SeenF,SeenU}]),
    %% Free unseen tables and add freed to free list.
    {Tf1,Tt1} = filter_tables(SeenT, Tf0, Tt0),
    {Ef1,Et1} = filter_environment(SeenE, Ef0, Et0),
    {Uf1,Ut1} = filter_userdata(SeenU, Uf0, Ut0),
    {Ff1,Ft1} = filter_funcdefs(SeenF, Ff0, Ft0),
    %% And update the tables.
    Tab1 = Tab0#tstruct{data=Tt1,free=Tf1},
    Env1 = Env0#tstruct{data=Et1,free=Ef1},
    Usd1 = Usd0#tstruct{data=Ut1,free=Uf1},
    Fnc1 = Fnc0#tstruct{data=Ft1,free=Ff1},
    St#luerl{tabs=Tab1,envs=Env1,usds=Usd1,fncs=Fnc1}.

%% mark(ToDo, MoreTodo, GcTabs, GcEnv, GcUserdata, GcFuncdefs) ->
%%     {SeenTabs,SeenFrames,SeenUserdata,SeenFuncdefs}.
%% Scan over all live objects and mark seen tables by adding them to
%% the seen list.

mark([{in_table,_}=_T|Todo], More, GcT, GcE, GcU, GcF) ->
    %%io:format("gc: ~p\n", [_T]),
    mark(Todo, More, GcT, GcE, GcU, GcF);
mark([#tref{i=T}|Todo], More, #gct{t=Tt,s=Ts0}=GcT, GcE, GcU, GcF) ->
    case ordsets:is_element(T, Ts0) of
        true ->                                 %Already done
            mark(Todo, More, GcT, GcE, GcU, GcF);
        false ->                                %Mark it and add to todo
            Ts1 = ordsets:add_element(T, Ts0),
            #table{a=Arr,d=Dict,meta=Meta} = ?GET_TABLE(T, Tt),
            %% Have to be careful when adding Tab and Meta as Tab is
            %% [{Key,Val}], Arr is array and Meta is
            %% nil|#tref{i=M}. We want lists.
            Aes = array:sparse_to_list(Arr),
            Des = ttdict:to_list(Dict),
            mark([Meta|Todo], [[{in_table,T}],Des,Aes,[{in_table,-T}]|More],
                 GcT#gct{s=Ts1}, GcE, GcU, GcF)
    end;
mark([#eref{i=F}|Todo], More, GcT, #gct{t=Et,s=Es0}=GcE, GcU, GcF) ->
    %% io:format("eref0: ~p\ ~p ~pn", [F,Et,Es0]),
    case ordsets:is_element(F, Es0) of
        true ->                                 %Already done
            mark(Todo, More, GcT, GcE, GcU, GcF);
        false ->                                %Mark it and add to todo
            Es1 = ordsets:add_element(F, Es0),
            Ses = tuple_to_list(?GET_TABLE(F, Et)),
            %% io:format("eref1: ~p ~p\n", [Et,Es1]),
            mark(Todo, [Ses|More], GcT, GcE#gct{s=Es1}, GcU, GcF)
    end;
mark([#usdref{i=U}|Todo], More, GcT, GcE, #gct{s=Us0}=GcU, GcF) ->
    case ordsets:is_element(U, Us0) of
       true ->                                 %Already done
           mark(Todo, More, GcT, GcE, GcU, GcF);
       false ->
           Us1 = ordsets:add_element(U, Us0),
           mark(Todo, More, GcT, GcE, GcU#gct{s=Us1}, GcF)
    end;
mark([#funref{i=F,env=Erefs}|ToDo], More, GcT, GcE, GcU,
     #gct{t=Ft0,s=Fs0}=GcF) ->
    %% io:format("funref0: ~p ~p ~p\n", [F,Fs0,Erefs]),
    %% Each funref has its own environments but we only need to add
    %% the function definition once.
    case ordsets:is_element(F, Fs0) of
        true ->
            mark(ToDo, [Erefs|More], GcT, GcE, GcU, GcF);
        false ->
            %% And mark the function definition.
            Fs1 = ordsets:add_element(F, Fs0),
            Fdef = ?GET_TABLE(F, Ft0),
            %% io:format("funref1: ~p ~p ~p\n", [F,Fs1,Erefs]),
            mark([Fdef|ToDo], [Erefs|More], GcT, GcE, GcU, GcF#gct{s=Fs1})
    end;
mark([#lua_func{funrefs=Funrefs}|Todo], More, GcT, GcE, GcU, GcF) ->
    %% io:format("push funrefs ~p\n", [Funrefs]),
    mark(Todo, [Funrefs|More], GcT, GcE, GcU, GcF);
%% The call stack.
mark([#call_frame{func=Funref,lvs=Lvs,env=Env}|Todo],
     More0, GcT, GcE, GcU, GcF) ->
    %% io:format("cf ~p\n", [Funref]),
    More1 = [ tuple_to_list(Lv) || Lv <- Lvs, is_tuple(Lv) ] ++ [Env|More0],
    mark([Funref|Todo], More1, GcT, GcE, GcU, GcF);
mark([#loop_frame{lvs=Lvs,stk=Stk,env=Env}|Todo], More0, GcT, GcE, GcU, GcF) ->
    More1 = [ tuple_to_list(Lv) || Lv <- Lvs, is_tuple(Lv) ] ++ [Stk,Env|More0],
    mark(Todo, More1, GcT, GcE, GcU, GcF);
%% Specifically catch these as they would match table key-value pair.
mark([#erl_func{}|Todo], More, GcT, GcE, GcU, GcF) ->
    mark(Todo, More, GcT, GcE, GcU, GcF);
mark([#erl_mfa{}|Todo], More, GcT, GcE, GcU, GcF) ->
    mark(Todo, More, GcT, GcE, GcU, GcF);
mark([#thread{}|Todo], More, GcT, GcE, GcU, GcF) ->
    mark(Todo, More, GcT, GcE, GcU, GcF);
mark([#userdata{meta=Meta}|Todo], More, GcT, GcE, GcU, GcF) ->
    mark([Meta|Todo], More, GcT, GcE, GcU, GcF);
mark([{K,V}|Todo], More, GcT, GcE, GcU, GcF) -> %Table key-value pair
    %% io:format("mt: ~p\n", [{K,V}]),
    mark([K,V|Todo], More, GcT, GcE, GcU, GcF);
mark([_|Todo], More, GcT, GcE, GcU, GcF) ->
    %% Can ignore everything else.
    mark(Todo, More, GcT, GcE, GcU, GcF);
mark([], [M|More], GcT, GcE, GcU, GcF) ->
    mark(M, More, GcT, GcE, GcU, GcF);
mark([], [], #gct{s=St}, #gct{s=Se}, #gct{s=Su}, #gct{s=Sf}) ->
    {St,Se,Su,Sf}.

%% filter_tables(Seen, Free, Tables) -> {Free,Tables}.
%% filter_environment(Seen, Free, Frames) -> {Free,Frames}.
%% filter_userdata(Seen, Free, Frames) -> {Free,Frames}.
%% filter_funcdefs(Seen, Free, Frames) -> {Free,Frames}.
%%  Filter tables/frames/userdata/funcdefs and return updated free
%%  lists and tables/frames.

filter_tables(Seen, Tf0, Tt0) ->
    %% Update the free list.
    Tf1 = ?FOLD_TABLES(fun (K, _, Free) ->
                               case ordsets:is_element(K, Seen) of
                                   true -> Free;
                                   false -> [K|Free]
                               end
                       end, Tf0, Tt0),
    Tt1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Tt0),
    {Tf1,Tt1}.

filter_environment(Seen, Ef0, Et0) ->
    %% io:format("env0: ~p ~p ~p\n", [Seen,Ef0,Et0]),
    %% Update the free list.
    Ef1 = ?FOLD_TABLES(fun (K, _, Free) ->
                               case ordsets:is_element(K, Seen) of
                                   true -> Free;
                                   false -> [K|Free]
                               end
                       end, Ef0, Et0),
    Et1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Et0),
    %% io:format("env1: ~p ~p\n", [Ef1,Et1]),
    {Ef1,Et1}.

filter_userdata(Seen, Uf0, Ut0) ->
    %% Update the free list.
    Uf1 = ?FOLD_TABLES(fun (K, _, Free) ->
                              case ordsets:is_element(K, Seen) of
                                  true -> Free;
                                  false -> [K|Free]
                              end
                      end, Uf0, Ut0),
    %% Reclaim free table slots.
    Ut1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Ut0),
    {Uf1,Ut1}.

filter_funcdefs(Seen, Ff0, Ft0) ->
    %% Update the free list.
    Ff1 = ?FOLD_TABLES(fun (K, _, Free) ->
                                case ordsets:is_element(K, Seen) of
                                    true -> Free;
                                    false -> [K|Free]
                                end
                        end, Ff0, Ft0),
    Ft1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Ft0),
    {Ff1,Ft1}.
