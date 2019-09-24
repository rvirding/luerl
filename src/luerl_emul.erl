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

%% File    : luerl_emul.erl
%% Author  : Robert Virding
%% Purpose : A very basic LUA 5.2 machine emulator.

%% First version of emulator. Compiler so far only explicitly handles
%% local/global variables.
%%
%% We explicitly mirror the parser rules which generate the AST and do
%% not try to fold similar structures into common code. While this
%% means we get more code it also becomes more explicit and clear what
%% we are doing. It may also allow for specific optimisations. And
%% example is that we DON'T fold 'var' and 'funcname' even though they
%% are almost the same.
%%
%% Issues: how should we handle '...'? Now we treat it as any (local)
%% variable.

-module(luerl_emul).

-include("luerl.hrl").
-include("luerl_comp.hrl").
-include("luerl_instrs.hrl").

%% Basic interface.
-export([init/0,gc/1]).
-export([call/2,call/3,emul/2]).
-export([load_chunk/2,load_chunk/3]).

%% Internal functions which can be useful "outside".
-export([alloc_table/1,alloc_table/2,free_table/2,
	 functioncall/3,methodcall/4,
	 get_table_keys/2,get_table_keys/3,
	 set_table_keys/3,set_table_keys/4,
	 get_table_key/3,set_table_key/4,
         alloc_userdata/2,alloc_userdata/3,get_userdata/2,set_userdata/3,
	 alloc_funcdef/2,get_funcdef/2,set_funcdef/3,
         get_metatable/2, set_metatable/3,
         get_metamethod/3,get_metamethod/4]).

%% Currently unused internal functions, to suppress warnings.
-export([set_global_name/3,set_global_key/3,
	 get_global_name/2,get_global_key/2]).

%% For testing.
-export([pop_vals/2,push_vals/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).

%% -compile(inline).				%For when we are optimising
%% -compile({inline,[boolean_value/1,first_value/1]}).

%% -define(ITRACE_DO(Expr), ok).
-define(ITRACE_DO(Expr), (get(itrace) /= undefined) andalso Expr).

%% init() -> State.
%% Initialise the basic state.

init() ->
    %% Initialise the general stuff.
    St0 = #luerl{meta=#meta{},tag=make_ref()},
    %% Initialise the tables.
    St1 = init_tables(St0),
    %% Allocate the _G table and initialise the environment
    {_G,St2} = luerl_lib_basic:install(St1),	%Global environment
    St3 = St2#luerl{g=_G},
    %% Now we can start adding libraries. Package MUST be first!
    St4 = load_lib(<<"package">>, luerl_lib_package, St3),
    %% Add the other standard libraries.
    St5 = load_libs([
		     {<<"bit32">>,luerl_lib_bit32},
		     {<<"io">>,luerl_lib_io},
		     {<<"math">>,luerl_lib_math},
		     {<<"os">>,luerl_lib_os},
		     {<<"string">>,luerl_lib_string},
		     {<<"utf8">>,luerl_lib_utf8},
		     {<<"table">>,luerl_lib_table},
		     {<<"debug">>,luerl_lib_debug}
		    ], St4),
    %% Set _G variable to point to it and add it to packages.loaded.
    St6 = set_global_key(<<"_G">>, _G, St5),
    set_table_keys([<<"package">>,<<"loaded">>,<<"_G">>], _G, St6).

init_tables(St0) ->
    %% Initialise the table handling.
    St1 = St0#luerl{ttab=?MAKE_TABLE(),tfree=[],tnext=0},
    %% Initialise the frame handling.
    St2 = St1#luerl{upvtab=?MAKE_TABLE(),upvfree=[],upvnext=0},
    %% Initialise the userdata handling.
    St3 = St2#luerl{usdtab=?MAKE_TABLE(),usdfree=[],usdnext=0},
    %% Initialise the function def handling.
    St4 = St3#luerl{funtab=?MAKE_TABLE(),funfree=[],funnext=0},
    St4.

load_libs(Libs, St) ->
    Fun = fun ({Key,Mod}, S) -> load_lib(Key, Mod, S) end,
    lists:foldl(Fun, St, Libs).

%% load_lib(Key, Module, State) -> State.

load_lib(Key, Mod, St0) ->
    {Tab,St1} = Mod:install(St0),
    %% Add key to global and to package.loaded.
    St2 = set_global_key(Key, Tab, St1),
    set_table_keys([<<"package">>,<<"loaded">>,Key], Tab, St2).

%% set_global_name(Name, Value, State) -> State.
%% set_global_key(Key, Value, State) -> State.
%% get_global_name(Name, State) -> {[Val],State}.
%% get_global_key(Key, State) -> {[Val],State}.
%%  Access elements in the global name table, _G.

set_global_name(Name, Val, St) ->
    set_global_key(atom_to_binary(Name, latin1), Val, St).

set_global_key(Key, Val, #luerl{g=G}=St) ->
    set_table_key(G, Key, Val, St).

get_global_name(Name, St) ->
    get_global_key(atom_to_binary(Name, latin1), St).

get_global_key(Key, #luerl{g=G}=St) ->
    get_table_key(G, Key, St).

%% alloc_frame(Frame, State) -> {Fref,State}.
%%  Allocate the frame in the frame table and return its fref.

alloc_frame(Fr, #luerl{upvtab=Ft0,upvfree=[N|Ns]}=St) ->
    Ft1 = ?SET_TABLE(N, Fr, Ft0),
    {#fref{i=N},St#luerl{upvtab=Ft1,upvfree=Ns}};
alloc_frame(Fr, #luerl{upvtab=Ft0,upvfree=[],upvnext=N}=St) ->
    Ft1 = ?SET_TABLE(N, Fr, Ft0),
    {#fref{i=N},St#luerl{upvtab=Ft1,upvnext=N+1}}.

%% alloc_table(State) -> {Tref,State}.
%% alloc_table(InitialTable, State) -> {Tref,State}.
%% free_table(Tref, State) -> State.
%%  The InitialTable is [{Key,Value}], there is no longer any need to
%%  have it as an orddict.

alloc_table(St) -> alloc_table([], St).

alloc_table(Itab, #luerl{ttab=Ts0,tfree=[N|Ns]}=St) ->
    T = create_table(Itab),
    %% io:fwrite("it1: ~p\n", [{N,T}]),
    Ts1 = ?SET_TABLE(N, T, Ts0),
    {#tref{i=N},St#luerl{ttab=Ts1,tfree=Ns}};
alloc_table(Itab, #luerl{ttab=Ts0,tfree=[],tnext=N}=St) ->
    T = create_table(Itab),
    %% io:fwrite("it2: ~p\n", [{N,T}]),
    Ts1 = ?SET_TABLE(N, T, Ts0),
    {#tref{i=N},St#luerl{ttab=Ts1,tnext=N+1}}.

create_table(Itab) ->
    D0 = ttdict:new(),
    A0 = array:new([{default,nil}]),		%Arrays with 'nil' as default
    Init = fun ({_,nil}, {D,A}) -> {D,A};	%Ignore nil values
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

free_table(#tref{i=N}, #luerl{ttab=Ts0,tfree=Ns}=St) ->
    %% io:fwrite("ft: ~p\n", [{N,?GET_TABLE(N, Ts0)}]),
    Ts1 = ?DEL_TABLE(N, Ts0),
    St#luerl{ttab=Ts1,tfree=[N|Ns]}.

%% get_table_keys(Keys, State) -> {Value,State}.
%% get_table_keys(Tab, Keys, State) -> {Value,State}.
%%  Search down tables which stops when no more tables.

get_table_keys(Keys, St) ->
    get_table_keys(St#luerl.g, Keys, St).

get_table_keys(Tab, [K|Ks], St0) ->
    {Val,St1} = luerl_emul:get_table_key(Tab, K, St0),
    get_table_keys(Val, Ks, St1);
get_table_keys(Val, [], St) -> {Val,St}.

%% set_table_keys(Keys, Val, State) -> State.
%% set_table_keys(Tab, Keys, Val, State) -> State.
%%  Setter down tables.

set_table_keys(Keys, Val, St) ->
    set_table_keys(St#luerl.g, Keys, Val, St).

set_table_keys(Tab, [K], Val, St) ->
    luerl_emul:set_table_key(Tab, K, Val, St);
set_table_keys(Tab0, [K|Ks], Val, St0) ->
    {Tab1,St1} = luerl_emul:get_table_key(Tab0, K, St0),
    set_table_keys(Tab1, Ks, Val, St1).

%% set_table_key(Tref, Key, Value, State) -> State.
%% get_table_key(Tref, Key, State) -> {Val,State}.
%%  Access tables, as opposed to the environment (which are also
%%  tables). Setting a value to 'nil' will clear it from the array but
%%  not from the table; however, we won't add a nil value.
%%  NOTE: WE ALWAYS RETURN A SINGLE VALUE!

set_table_key(#tref{}=Tref, Key, Val, St) when is_integer(Key), Key >= 1 ->
    set_table_int_key(Tref, Key, Key, Val, St);
set_table_key(#tref{}=Tref, Key, Val, St) when is_float(Key) ->
    case ?IS_FLOAT_INT(Key, I) of
	true when I >= 1 -> set_table_int_key(Tref, Key, I, Val, St);
	_NegFalse -> set_table_key_key(Tref, Key, Val, St)
    end;
set_table_key(Tab, nil=Key, _, St) ->
    lua_error({illegal_index,Tab,Key}, St);
set_table_key(#tref{}=Tref, Key, Val, St) ->
    set_table_key_key(Tref, Key, Val, St);
set_table_key(Tab, Key, _, St) ->
    lua_error({illegal_index,Tab,Key}, St).

set_table_key_key(#tref{i=N}=Tab, Key, Val, #luerl{ttab=Ts0}=St) ->
    #table{d=Dict0,meta=Meta}=T = ?GET_TABLE(N, Ts0),
    case ttdict:find(Key, Dict0) of
	{ok,_} ->				%Key exists
	    Dict1 = if Val =:= nil -> ttdict:erase(Key, Dict0);
		       true -> ttdict:store(Key, Val, Dict0)
		    end,
	    Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
	    St#luerl{ttab=Ts1};
	error ->				%Key does not exist
	    case get_metamethod_tab(Meta, <<"__newindex">>, Ts0) of
		nil ->
		    %% Only add non-nil value.
		    Dict1 = if Val =:= nil -> Dict0;
			       true -> ttdict:store(Key, Val, Dict0)
			    end,
		    Ts1 = ?SET_TABLE(N, T#table{d=Dict1}, Ts0),
		    St#luerl{ttab=Ts1};
		Meth when ?IS_FUNCTION(Meth) ->
		    {_Ret, St1} = functioncall(Meth, [Tab,Key,Val], St),
		    St1;
		Meth -> set_table_key(Meth, Key, Val, St)
	    end
    end.

set_table_int_key(#tref{i=N}=Tab, Key, I, Val, #luerl{ttab=Ts0}=St) ->
    #table{a=Arr0,meta=Meta}=T = ?GET_TABLE(N, Ts0),
    case array:get(I, Arr0) of
	nil ->					%Key does not exist
	    case get_metamethod_tab(Meta, <<"__newindex">>, Ts0) of
		nil ->
		    %% Only add non-nil value, slightly faster (?)
		    Arr1 = if Val =:= nil -> Arr0;
			      true -> array:set(I, Val, Arr0)
			   end,
		    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
		    St#luerl{ttab=Ts1};
		Meth when ?IS_FUNCTION(Meth) ->
		    {_Ret, St1} = functioncall(Meth, [Tab,Key,Val], St),
		    St1;
		Meth -> set_table_key(Meth, Key, Val, St)
	    end;
	_ ->					%Key exists
	    %% Can do this as 'nil' is default value of array.
	    Arr1 = array:set(I, Val, Arr0),
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
	    St#luerl{ttab=Ts1}
    end.

get_table_key(#tref{}=Tref, Key, St) when is_integer(Key), Key >= 1 ->
    get_table_int_key(Tref, Key, Key, St);
get_table_key(#tref{}=Tref, Key, St) when is_float(Key) ->
    case ?IS_FLOAT_INT(Key, I) of
	true when I >= 1 -> get_table_int_key(Tref, Key, I, St);
	_NegFalse -> get_table_key_key(Tref, Key, St)
    end;
get_table_key(#tref{}=Tref, Key, St) ->
    get_table_key_key(Tref, Key, St);
get_table_key(Tab, Key, St) ->			%Just find the metamethod
    case get_metamethod(Tab, <<"__index">>, St) of
	nil -> lua_error({illegal_index,Tab,Key}, St);
	Meth when ?IS_FUNCTION(Meth) ->
	    {Vs,St1} = functioncall(Meth, [Tab,Key], St),
	    {first_value(Vs),St1};
	Meth ->					%Recurse down the metatable
	    get_table_key(Meth, Key, St)
    end.

get_table_key_key(#tref{i=N}=T, Key, #luerl{ttab=Ts}=St) ->
    #table{d=Dict,meta=Meta} = ?GET_TABLE(N, Ts),
    case ttdict:find(Key, Dict) of
	{ok,Val} -> {Val,St};
	error ->
	    %% Key not present so try metamethod
	    get_table_metamethod(T, Meta, Key, Ts, St)
    end.

get_table_int_key(#tref{i=N}=T, Key, I, #luerl{ttab=Ts}=St) ->
    #table{a=A,meta=Meta} = ?GET_TABLE(N, Ts),	%Get the table.
    case array:get(I, A) of
	nil ->
	    %% Key not present so try metamethod
	    get_table_metamethod(T, Meta, Key, Ts, St);
	Val -> {Val,St}
    end.

get_table_metamethod(T, Meta, Key, Ts, St) ->
    case get_metamethod_tab(Meta, <<"__index">>, Ts) of
	nil -> {nil,St};
	Meth when ?IS_FUNCTION(Meth) ->
	    {Vs,St1} = functioncall(Meth, [T,Key], St),
	    {first_value(Vs),St1};
	Meth ->				%Recurse down the metatable
	    get_table_key(Meth, Key, St)
    end.

%% alloc_userdata(Data, State) -> {Usdref,State}.
%% alloc_userdata(Data, Meta, State) -> {Usdref,State}.
%% set_userdata(Usdref, UserData, State) -> State.
%% get_userdata(Usdref, State) -> {UserData,State}.

alloc_userdata(Data, St) ->
    alloc_userdata(Data, nil, St).

alloc_userdata(Data, Meta, #luerl{usdtab=Us0,usdfree=[N|Ns]}=St) ->
    Us1 = ?SET_TABLE(N, #userdata{d=Data,meta=Meta}, Us0),
    {#usdref{i=N},St#luerl{usdtab=Us1,usdfree=Ns}};
alloc_userdata(Data, Meta, #luerl{usdtab=Us0,usdfree=[],usdnext=N}=St) ->
    Us1 = ?SET_TABLE(N, #userdata{d=Data,meta=Meta}, Us0),
    {#usdref{i=N},St#luerl{usdtab=Us1,usdnext=N+1}}.

set_userdata(#usdref{i=N}, Data, #luerl{usdtab=Us0}=St) ->
    Us1 = ?UPD_TABLE(N, fun (Ud) -> Ud#userdata{d=Data} end, Us0),
    St#luerl{usdtab=Us1}.

get_userdata(#usdref{i=N}, #luerl{usdtab=Us}=St) ->
    #userdata{} = Udata = ?GET_TABLE(N, Us),
    {Udata,St}.

%% make_userdata(Data) -> make_userdata(Data, nil).
%% make_userdata(Data, Meta) -> #userdata{d=Data,meta=Meta}.

%% alloc_funcdef(Def, State) -> {FunRef,State}.
%% set_funcdef(Funref, Fdef, State) -> State.
%% get_funcdef(Funref, State) -> {Fdef,State}.

alloc_funcdef(Func, #luerl{funtab=Ft0,funfree=[N|Ns]}=St) ->
    Ft1 = ?SET_TABLE(N, Func, Ft0),
    {#funref{i=N},St#luerl{funtab=Ft1,funfree=Ns}};
alloc_funcdef(Func, #luerl{funtab=Ft0,funfree=[],funnext=N}=St) ->
    Ft1 = ?SET_TABLE(N, Func, Ft0),
    {#funref{i=N},St#luerl{funtab=Ft1,funnext=N+1}}.

set_funcdef(#funref{i=N}, Func, #luerl{funtab=Ft0}=St) ->
    Ft1 = ?SET_TABLE(N, Func, Ft0),
    St#luerl{funtab=Ft1}.

get_funcdef(#funref{i=N}, #luerl{funtab=Ft}=St) ->
    Fdef = ?GET_TABLE(N, Ft),
    {Fdef,St}.

%% set_local_var(Depth, Index, Var, Frames) -> Frames.
%% get_local_var(Depth, Index, Frames) -> Val.

set_local_var(1, I, V, [F|Fs]) ->
    [setelement(I, F, V)|Fs];
set_local_var(D, I, V, [F|Fs]) ->
    [F|set_local_var(D-1, I, V, Fs)].

get_local_var(1, I, [F|_]) -> element(I, F);
get_local_var(D, I, [_|Fs]) ->
    get_local_var(D-1, I, Fs).

%% set_env_var(Depth, Index, Val, Env, State) -> State.
%% get_env_var(Depth, Index, Env, State) -> Val.
%%  We must have the state as the environments are global in the
%%  state.

set_env_var(D, I, Val, Env, #luerl{upvtab=Ft0}=St) ->
    Ft1 = set_env_var_1(D, I, Val, Env, Ft0),
    St#luerl{upvtab=Ft1}.

set_env_var_1(1, I, V, [#fref{i=N}|_], Ft) ->
    F = setelement(I, ?GET_TABLE(N, Ft), V),
    ?SET_TABLE(N, F, Ft);
set_env_var_1(2, I, V, [_,#fref{i=N}|_], Ft) ->
    F = setelement(I, ?GET_TABLE(N, Ft), V),
    ?SET_TABLE(N, F, Ft);
set_env_var_1(D, I, V, Fps, Ft) ->
    #fref{i=N} = lists:nth(D, Fps),
    F = setelement(I, ?GET_TABLE(N, Ft), V),
    ?SET_TABLE(N, F, Ft).

get_env_var(D, I, Env, #luerl{upvtab=Ft}) ->
    get_env_var_1(D, I, Env, Ft).

get_env_var_1(1, I, [#fref{i=N}|_], Ft) ->
    element(I, ?GET_TABLE(N, Ft));
get_env_var_1(2, I, [_,#fref{i=N}|_], Ft) ->
    element(I, ?GET_TABLE(N, Ft));
get_env_var_1(D, I, Fps, Ft) ->
    #fref{i=N} = lists:nth(D, Fps),
    element(I, ?GET_TABLE(N, Ft)).

%% set_global_var(Var, Val, State) -> State.
%% get_global_var(Var, State) -> {Val,State}.
%%  _G a normal table with metatable so we must use the table
%%  functions.  However we can optimise a bit as we KNOW that _G is a
%%  table and the var is always a normal non-integer key.

set_global_var(Var, Val, #luerl{g=G}=St) ->
    set_table_key_key(G, Var, Val, St).

get_global_var(Var, #luerl{g=G}=St) ->
    get_table_key_key(G, Var, St).

%% load_chunk(FunctionDefCode, State) -> {Function,State}.
%% load_chunk(FunctionDefCode, Env, State) -> {Function,State}.
%%  Load a chunk from the compiler which a compilefunction definition
%%  instructions returning a callable function. Currently it does
%%  nothing with the state.

load_chunk(Code, St) -> load_chunk(Code, [], St).

load_chunk(#code{code=[Code]}, [], St0) ->
    {?PUSH_FDEF(Funref),_,St1} = load_chunk_i(Code, [], St0),
    {Funref,St1}.

%% load_chunk_i(Instr, FuncRefs, Status) -> {Instr,FuncRefs,State}.
%% load_chunk_is(Instrs, FuncRefs, Status) -> {Instrs,FuncRefs,State}.
%%  Load chunk instructions. We keep track of the functions refs and
%%  save the ones directly accessed in each function. This will make
%%  gc easier as we will not have to step through the function code at
%%  gc time.

load_chunk_is([I0|Is0], Funrs0, St0) ->
    {I1,Funrs1,St1} = load_chunk_i(I0, Funrs0, St0),
    {Is1,Funrs2,St2} = load_chunk_is(Is0, Funrs1, St1),
    {[I1|Is1],Funrs2,St2};
load_chunk_is([], Funrs, St) -> {[],Funrs,St}.

%% First the instructions with nested code.
load_chunk_i(?PUSH_FDEF(Anno, Lsz, Esz, Pars, B0), Funrs0, St0) ->
    {B1,Funrs,St1} = load_chunk_is(B0, [], St0),
    Fdef = #lua_func{anno=Anno,funrefs=Funrs,lsz=Lsz,esz=Esz,pars=Pars,b=B1},
    {Funref,St2} = alloc_funcdef(Fdef, St1),
    Funrs1 = ordsets:add_element(Funref, Funrs0),
    {?PUSH_FDEF(Funref),Funrs1,St2};
load_chunk_i(?BLOCK(Lsz, Esz, B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?BLOCK(Lsz, Esz, B1),Funrs1,St1};
load_chunk_i(?REPEAT(B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?REPEAT(B1),Funrs1,St1};
load_chunk_i(?WHILE(E0, B0), Funrs0, St0) ->
    {E1,Funrs1,St1} = load_chunk_is(E0, Funrs0, St0),
    {B1,Funrs2,St2} = load_chunk_is(B0, Funrs1, St1),
    {?WHILE(E1, B1),Funrs2,St2};
load_chunk_i(?AND_THEN(T0), Funrs0, St0) ->
    {T1,Funrs1,St1} = load_chunk_is(T0, Funrs0, St0),
    {?AND_THEN(T1),Funrs1,St1};
load_chunk_i(?OR_ELSE(T0), Funrs0, St0) ->
    {T1,Funrs1,St1} = load_chunk_is(T0, Funrs0, St0),
    {?OR_ELSE(T1),Funrs1,St1};
load_chunk_i(?IF_TRUE(T0), Funrs0, St0) ->
    {T1,Funrs1,St1} = load_chunk_is(T0, Funrs0, St0),
    {?IF_TRUE(T1),Funrs1,St1};
load_chunk_i(?IF(T0, F0), Funrs0, St0) ->
    {T1,Funrs1,St1} = load_chunk_is(T0, Funrs0, St0),
    {F1,Funrs2,St2} = load_chunk_is(F0, Funrs1, St1),
    {?IF(T1, F1),Funrs2,St2};
load_chunk_i(?NFOR(V, B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?NFOR(V, B1),Funrs1,St1};
load_chunk_i(?GFOR(Vs, B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?GFOR(Vs, B1),Funrs1,St1};
%% These are dynamic instructions we don't see here:
%% while_test, repeat_test

%% Then the rest which we don't have to worry about.
load_chunk_i(I, Funrs, St) -> {I,Funrs,St}.

%% call(Function, State) -> {Return,State}.
%% call(Function, Args, State) -> {Return,State}.

call(Func, St) -> call(Func, [], St).

call(#funref{}=Funref, Args, St0) ->		%Lua function
    {Ret,St1} = functioncall(Funref, Args, St0),
    %% Should do GC here.
    {Ret,St1};
call(#erl_func{}=Func, Args, St0) ->		%Erlang function
    {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1}.

itrace_print(Format, Args) ->
    ?ITRACE_DO(io:fwrite(Format, Args)).

%% exp(_, _) ->
%%     error(boom).

-record(call_frame, {func,args,lvs,env}).	%Save these for the GC

%% emul(Instrs, State).
%% emul(Instrs, LocalVariables, Stack, Env, State).
%%  The cost of checking the itrace process variable is very slight
%%  compared to everythin else.

emul(Is, St) ->
    emul(Is, {}, [], [], St).

emul([I|_]=Is, Lvs, Stk, Env, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("Lvs: ~p\n", [Lvs]),
		   io:fwrite("Upv: ~p\n", [Env]),
		   io:fwrite("Stk: ~p\n", [Stk]),
		   io:fwrite("I: ~p\n", [I]),
		   io:put_chars("--------\n")
	       end),
    emul_1(Is, Lvs, Stk, Env, St);
emul([], Lvs, Stk, Env, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("Lvs: ~p\n", [Lvs]),
		   io:fwrite("Upv: ~p\n", [Env]),
		   io:fwrite("Stk: ~p\n", [Stk]),
		   io:put_chars("--------\n")
	       end),
    emul_1([], Lvs, Stk, Env, St).

%% Expression instructions.
emul_1([?PUSH_LIT(L)|Is], Lvs, Stk, Env, St) ->
    emul(Is, Lvs, [L|Stk], Env, St);
emul_1([?PUSH_LVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    Val = get_local_var(D, I, Lvs),
    emul(Is, Lvs, [Val|Stk], Env, St);
emul_1([?PUSH_EVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Val = get_env_var(D, I, Env, St),
    emul(Is, Lvs, [Val|Stk], Env, St);
emul_1([?PUSH_GVAR(K)|Is], Lvs, Stk, Env, St0) ->
    {Val,St1} = get_global_var(K, St0),
    emul(Is, Lvs, [Val|Stk], Env, St1);

emul_1([?PUSH_LAST_LIT(L)|Is], Lvs, Stk, Env, St) ->
    emul(Is, Lvs, [[L]|Stk], Env, St);
emul_1([?PUSH_LAST_LVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    Val = get_local_var(D, I, Lvs),
    emul(Is, Lvs, [[Val]|Stk], Env, St);
emul_1([?PUSH_LAST_EVAR(D, I)|Is], Lvs, Stk, Env, St) ->
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Val = get_env_var(D, I, Env, St),
    emul(Is, Lvs, [[Val]|Stk], Env, St);
emul_1([?PUSH_LAST_GVAR(K)|Is], Lvs, Stk, Env, St0) ->
    {Val,St1} = get_global_var(K, St0),
    emul(Is, Lvs, [[Val]|Stk], Env, St1);

emul_1([?STORE_LVAR(D, I)|Is], Lvs0, [V|Stk], Env, St) ->
    Lvs1 = set_local_var(D, I, V, Lvs0),
    emul(Is, Lvs1, Stk, Env, St);
emul_1([?STORE_EVAR(D, I)|Is], Lvs, [V|Stk], Env, St0) ->
    St1 = set_env_var(D, I, V, Env, St0),
    emul(Is, Lvs, Stk, Env, St1);
emul_1([?STORE_GVAR(K)|Is], Lvs, [V|Stk], Env, St0) ->
    St1 = set_global_var(K, V, St0),
    emul(Is, Lvs, Stk, Env, St1);

emul_1([?GET_KEY|Is], Lvs, [Key,Tab|Stk], Env, St0) ->
    {Val,St1} = get_table_key(Tab, Key, St0),
    emul(Is, Lvs, [Val|Stk], Env, St1);
emul_1([?GET_LIT_KEY(K)|Is], Lvs, [Tab|Stk], Env, St0) ->
    %% [?PUSH_LIT(K),?GET_KEY]
    {Val,St1} = get_table_key(Tab, K, St0),
    emul(Is, Lvs, [Val|Stk], Env, St1);
emul_1([?SET_KEY|Is], Lvs, [Key,Tab,Val|Stk], Env, St0) ->
    St1 = set_table_key(Tab, Key, Val, St0),
    emul(Is, Lvs, Stk, Env, St1);
emul_1([?SET_LIT_KEY(Key)|Is], Lvs, [Tab,Val|Stk], Env, St0) ->
    %% [?PUSH_LIT(K),?SET_KEY]
    St1 = set_table_key(Tab, Key, Val, St0),
    emul(Is, Lvs, Stk, Env, St1);

emul_1([?SINGLE|Is], Lvs, [Val|Stk], Env, St) ->
    emul(Is, Lvs, [first_value(Val)|Stk], Env, St);
emul_1([?MULTIPLE|Is], Lvs, [Val|Stk], Env, St) ->
    emul(Is, Lvs, [multiple_value(Val)|Stk], Env, St);

emul_1([?BUILD_TAB(Fc, I)|Is], Lvs, Stk0, Env, St0) ->
    {Tab,Stk1,St1} = build_tab(Fc, I, Stk0, St0),
    emul(Is, Lvs, [Tab|Stk1], Env, St1);
emul_1([?FCALL|Is], Lvs, Stk, Env, St) ->
    do_fcall(Is, Lvs, Stk, Env, St);
emul_1([?TAIL_FCALL(Ac)|Is], Lvs, Stk, Env, St) ->
    do_tail_fcall(Is, Lvs, Stk, Env, St, Ac);
emul_1([?MCALL(K)|Is], Lvs, Stk, Env, St) ->
    do_mcall(Is, Lvs, Stk, Env, St, K);
emul_1([?OP(Op,1)|Is], Lvs, Stk, Env, St) ->
    do_op1(Is, Lvs, Stk, Env, St, Op);
emul_1([?OP(Op,2)|Is], Lvs, Stk, Env, St) ->
    do_op2(Is, Lvs, Stk, Env, St, Op);
emul_1([?PUSH_FDEF(Funref)|Is], Lvs, Stk, Env, St0) ->
    %% Update the env field of the function reference with the current
    %% environment.
    Funref1 = Funref#funref{env=Env},
    %% io:format("pf: ~p\n", [[Funref1|Env]]),
    emul(Is, Lvs, [Funref1|Stk], Env, St0);

%% Control instructions.

%% emul_1([?BLOCK(Lsz, Esz, Bis)|Is], Lvs0, Stk0, Env0, St0) ->
%%     {Lvs1,Stk1,Env1,St1} = do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz),
%%     emul(Is, Lvs1, Stk1, Env1, St1);

emul_1([?BLOCK(Lsz, Esz, Bis)|Is], Lvs, Stk, Env, St0) ->
    L = make_loc_frame(Lsz),
    {Eref,St1} = make_env_frame(Esz, St0),
    AllIs = Bis ++ [?CLOSE] ++ Is,
    emul(AllIs, [L|Lvs], Stk, [Eref|Env], St1);
emul_1([?CLOSE|Is], [_|Lvs], Stk, [_|Env], St) ->
    emul(Is, Lvs, Stk, Env, St);

emul_1([?WHILE(Eis, Wis)|Is], Lvs, Stk, Env, St) ->
    do_while(Is, Lvs, Stk, Env, St, Eis, Wis);

%% emul_1([?WHILE(Eis, Wis)=While|Is], Lvs, Stk, Env, St) ->
%%     AllIs = Eis ++ ?IF_TRUE(Wis ++ [{while_test,Eis,Wis}]) ++ Is,
%%     emul(AllIs, Lvs, Stk, Env, St);
%% emul_1([{while_test,Eis,Wis}=Loop|Is], Lvs, Stk, Env, St) ->
%%     AllIs = Eis ++ ?IF_TRUE(Wis ++ [Loop]) ++ Is,
%%     emul(AllIs, Lvs, Stk, Env, St);

emul_1([?REPEAT(Ris)|Is], Lvs, Stk, Env, St) ->
    do_repeat(Is, Lvs, Stk, Env, St, Ris);

%% emul_1([?REPEAT(Ris)|Is], Lvs, Stk, Upvs, St) ->
%%     AllIs = Ris ++ [{repeat_test,Ris}] ++ Is,
%%     emul(AllIs, Lvs, Stk, Upvs, St);
%% emul_1([{repeat_test,Ris}|Is], Lvs, [Val|Stk], Upvs, St) ->
%%     case boolean_value(Val) of
%% 	true ->
%% 	    emul(Is, Lvs, Stk, Upvs, St);
%% 	false ->
%% 	    AllIs = [?REPEAT(Ris)|Is],
%% 	    emul(AllIs, Lvs, Stk, Upvs, St)
%%     end;

emul_1([?AND_THEN(T)|Is], Lvs, [Val|Stk1]=Stk0, Env, St) ->
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    AllIs = T ++ Is,
	    emul(AllIs, Lvs, Stk1, Env, St);
	false ->
	    emul(Is, Lvs, Stk0, Env, St)	%Non true value left on stack
    end;
emul_1([?OR_ELSE(T)|Is], Lvs, [Val|Stk1]=Stk0, Env, St) ->
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    emul(Is, Lvs, Stk0, Env, St);	%Non false value left on stack
	false ->
	    AllIs = T ++ Is,
	    emul(AllIs, Lvs, Stk1, Env, St)
    end;

%% emul_1([?IF_TRUE(T)|Is], Lvs, [Val|Stk0], Env, St0) ->
%%     %% This is a statement and pops the boolean value.
%%     io:format("it: ~p ~p\n", [Lvs,Env]),
%%     case boolean_value(Val) of
%%         true ->
%% 	    {Lvs1,Stk1,Env1,St1} = emul(T, Lvs, Stk0, Env, St0),
%% 	    emul(Is, Lvs1, Stk1, Env1, St1);
%%         false ->
%% 	    emul(Is, Lvs, Stk0, Env, St0)
%%     end;

emul_1([?IF_TRUE(T)|Is], Lvs, [Val|Stk], Env, St) ->
    %% This is a statement and pops the boolean value.
    case boolean_value(Val) of
	true ->
	    AllIs = T ++ Is,
	    emul(AllIs, Lvs, Stk, Env, St);
	false ->
	    emul(Is, Lvs, Stk, Env, St)
    end;

emul_1([?IF(True, False)|Is], Lvs0, Stk0, Env0, St0) ->
    {Lvs1,Stk1,Env1,St1} = do_if(Lvs0, Stk0, Env0, St0, True, False),
    emul(Is, Lvs1, Stk1, Env1, St1);
emul_1([?NFOR(V, Fis)|Is], Lvs, Stk, Env, St) ->
    do_numfor(Is, Lvs, Stk, Env, St, V, Fis);
emul_1([?GFOR(Vs, Fis)|Is], Lvs, Stk, Env, St) ->
    do_genfor(Is, Lvs, Stk, Env, St, Vs, Fis);
emul_1([?BREAK|_], Lvs, Stk, Env, St) ->
    throw({break,St#luerl.tag,Lvs,Stk,Env,St});
emul_1([?RETURN(0)|_], _, _, _, St) ->
    throw({return,St#luerl.tag,[],St});
emul_1([?RETURN(Ac)|_], _, Stk, _, St) ->
    {Ret,_} = pop_vals(Ac, Stk),
    throw({return,St#luerl.tag,Ret,St});

%% Stack instructions
emul_1([?POP|Is], Lvs, [_|Stk], Env, St) ->	%Just pop top off stack
    emul(Is, Lvs, Stk, Env, St);
emul_1([?POP2|Is], Lvs, [_,_|Stk], Env, St) ->	%Just pop top 2 off stack
    emul(Is, Lvs, Stk, Env, St);
emul_1([?SWAP|Is], Lvs, [S1,S2|Stk], Env, St) ->
    emul(Is, Lvs, [S2,S1|Stk], Env, St);
emul_1([?DUP|Is], Lvs, [V|_]=Stk, Env, St) ->
    emul(Is, Lvs, [V|Stk], Env, St);
emul_1([?PUSH_VALS(Vc)|Is], Lvs, [Vals|Stk0], Env, St) ->
    %% Pop value list off the stack and push Vc vals from it.
    Stk1 = push_vals(Vc, Vals, Stk0),
    emul(Is, Lvs, Stk1, Env, St);
emul_1([?POP_VALS(Vc)|Is], Lvs, Stk0, Env, St) ->
    %% Pop Vc vals off the stack, put in a list and push onto the stack.
    {Vals,Stk1} = pop_vals(Vc, Stk0),
    emul(Is, Lvs, [Vals|Stk1], Env, St);
emul_1([?PUSH_ARGS(Al)|Is], Lvs, [Args|Stk0], Env, St) ->
    %% Pop argument list off the stack and push args onto the stack.
    Stk1 = push_args(Al, Args, Stk0),
    emul(Is, Lvs, Stk1, Env, St);
emul_1([?POP_ARGS(Ac)|Is], Lvs, Stk0, Env, St) ->
    %% Pop Ac args off the stack, put in a list and push onto the stack.
    {Args,Stk1} = pop_vals(Ac, Stk0),
    emul(Is, Lvs, [Args|Stk1], Env, St);
emul_1([?COMMENT(_)|Is], Lvs, Stk, Env, St) ->
    %% This just a comment which is ignored.
    emul(Is, Lvs, Stk, Env, St);
emul_1([], Lvs, Stk, Env, St) ->
    {Lvs,Stk,Env,St}.

%% pop_vals(Count, Stack) -> {ValList,Stack}.
%% pop_vals(Count, Stack, ValList) -> {ValList,Stack}.
%%  Pop Count values off the stack and push onto the value list.
%%  First value is deepest. Always generates list.

pop_vals(0, Stk) -> {[],Stk};
pop_vals(C, [Vtail|Stk]) ->			%This a list tail
    pop_vals(C-1, Stk, Vtail).

pop_vals(0, Stk, Vs) -> {Vs,Stk};
pop_vals(1, [V|Stk], Vs) -> {[V|Vs],Stk};
pop_vals(2, [V2,V1|Stk], Vs) -> {[V1,V2|Vs],Stk};
pop_vals(C, [V2,V1|Stk], Vs) ->
    pop_vals(C-2, Stk, [V1,V2|Vs]).

%% push_vals(Count, ValList, Stack) -> Stack.
%%  Push Count values from ValList onto the stack. First value is
%%  deepest. Fill with 'nil' if not enough values.

push_vals(0, _, Stk) -> Stk;
push_vals(C, [V|Vs], Stk) ->
    push_vals(C-1, Vs, [V|Stk]);
push_vals(C, [], Stk) ->
    push_vals(C-1, [], [nil|Stk]).

%% push_args(Varlist, ArgList, Stack) -> Stack.
%%  Use Varlist to push args from ArgList onto the stack. First arg is
%%  deepest. Tail of VarList determines whether there are varargs.

push_args([_V|Vs], [A|As], Stk) ->
    push_args(Vs, As, [A|Stk]);
push_args([_V|Vs], [], Stk) ->
    push_args(Vs, [], [nil|Stk]);
push_args([], _As, Stk) -> Stk;			%Drop the rest
push_args(_V, As, Stk) ->			%Varargs ... save as list
    [As|Stk].

%% do_block(BlockInstrs, LocalVars, Stack, Env, State,
%%          LocalSize, EnvSize) -> {LocalVars,Stack,Env,State}.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

%% do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz) ->
%%     L = make_loc_frame(Lsz),
%%     {Eref,St1} = make_env_frame(Esz, St0),
%%     {[_|Lvs1],Stk1,[_|Env1],St2} = emul(Bis, [L|Lvs0], Stk0, [Eref|Env0], St1),
%%     {Lvs1,Stk1,Env1,St2}.

make_env_frame(0, St) -> {not_used,St};
make_env_frame(Esz, St0) ->
    E = erlang:make_tuple(Esz, nil),
    {Eref,St1} = alloc_frame(E, St0),
    {Eref,St1}.

make_loc_frame(0) -> not_used;
make_loc_frame(Lsz) ->
    erlang:make_tuple(Lsz, nil).

%% do_op1(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.
%% do_op2(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.

do_op1(Is, Lvs, [A|Stk], Env, St, Op) ->
    case op(Op, A) of
	{ok,Res} ->
	    emul(Is, Lvs, [Res|Stk], Env, St);
	{meta,Meta} ->
	    functioncall(Is, Lvs, Stk, Env, St, #erl_func{code=Meta}, []);
	{error,E} -> lua_error(E, St)
    end.

do_op2(Is, Lvs, [A2,A1|Stk], Env, St, Op) ->
    case op(Op, A1, A2) of
	{ok,Res} ->
	    emul(Is, Lvs, [Res|Stk], Env, St);
	{meta,Meta} ->
	    functioncall(Is, Lvs, Stk, Env, St, #erl_func{code=Meta}, []);
	{error,E} -> lua_error(E, St)
    end.

%% do_fcall(Instrs, LocalVars, Stack, Upvs, State) -> ReturnFromEmul.
%%  Pop arg list and function from stack and do call.

do_fcall(Is, Lvs, [Args,Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, Args).

%% functioncall(Function, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

functioncall(Func, Args, #luerl{stk=Stk}=St0) ->
    {Ret,St1} = functioncall(Func, Args, Stk, St0),
    {Ret,St1}.

%% functioncall(Instrs, LocalVars, Stk, Env, State, Func, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

functioncall(Is, Lvs, Stk0, Env, St0, Func, Args) ->
    Fr = #call_frame{func=Func,args=Args,lvs=Lvs,env=Env},
    Stk1 = [Fr|Stk0],
    {Ret,St1} = functioncall(Func, Args, Stk1, St0),
    emul(Is, Lvs, [Ret|Stk0], Env, St1).

%% do_tail_fcall(Instrs, Acc, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_tail_fcall(_Is, _Var, Stk, _Env, _St, 0) ->
    error({boom,[],Stk});
do_tail_fcall(_Is, _Var, Stk0, _Env, _St, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Func|Stk2] = Stk1,				%Get function
    error({boom,Func,Args,Stk2}).

%% do_mcall(Instrs, LocalVars, Stack, Env, State, Method) ->

do_mcall(Is, Lvs, [Args,Obj|Stk], Env, St, M) ->
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, Args).

%% methodcall(Object, Method, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

methodcall(Obj, M, Args, St0) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, M, St0) of
	{nil,St1} ->				%No method
	    lua_error({undef_method,Obj,M}, St1);
	{Val,St1} ->
	    functioncall(Val, [Obj|Args], St1#luerl.stk, St1)
    end.

%% methodcall(Instrs, Var, Stk, Env, State, Object, Method, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

methodcall(Is, Lvs, Stk, Env, St0, Obj, M, Args) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, M, St0) of
	{nil,St1} ->				%No method
	    lua_error({undef_method,Obj,M}, St1);
	{Val,St1} ->
	    functioncall(Is, Lvs, Stk, Env, St1, Val, [Obj|Args])
    end.

%% functioncall(Function, Args, Stack, State) -> {Return,State}.
%%  Setup environment for function and do the actual call.

functioncall(#funref{env=Env}=Funref, Args, Stk, St0) ->
    {Func,St1} = get_funcdef(Funref, St0),
    functioncall(Func, Args, Stk, Env, St1);
functioncall(#erl_func{code=Func}, Args, Stk, #luerl{stk=Stk0}=St0) ->
    %% Here we must save the stack in state as function may need it.
    {Ret,St1} = Func(Args, St0#luerl{stk=Stk}),
    {Ret,St1#luerl{stk=Stk0}};			%Replace it
functioncall(Func, Args, Stk, St) ->
    case get_metamethod(Func, <<"__call">>, St) of
	nil -> lua_error({undef_function,Func}, St);
	Meta -> functioncall(Meta, [Func|Args], Stk, St)
    end.

%% functioncall(LuaFunc, Args, Stack, Upv, State) -> {Return,State}.
%%  Make the local variable and Upv frames and push them onto
%%  respective stacks and call the function.

functioncall(#lua_func{anno=_Anno,lsz=Lsz,esz=Esz,pars=_Pars,b=Fis}, Args, Stk, Env, St0) ->
    %% io:format("fc1: anno=~p\n", [_Anno]),
    %% io:format("fc1: lsz=~p esz=~p pars=~p args=~p\n", [Lsz,Esz,_Pars,Args]),
    L = make_loc_frame(Lsz),
    {Eref,St1} = make_env_frame(Esz, St0),
    %% io:format("fc1: L=~p E=~p\n",
    %% 	      [L,Eref == not_used
    %% 	       orelse (?GET_TABLE(Eref#fref.i,St1#luerl.upvtab))]),
    {Ret,St2} = call_luafunc(Fis, [L], [Args|Stk], [Eref|Env], St1),
    {Ret,St2}.

call_luafunc(Fis, Lvs, Stk, Env, St0) ->
    Tag = St0#luerl.tag,
    %% io:fwrite("fc: ~p\n", [{Lvs,Env,St0#luerl.env}]),
    try
	{_,_,_,Sta} = emul(Fis, Lvs, Stk, Env, St0),
	%%io:fwrite("fr: ~p\n", [{Tag,[]}]),
	{[],Sta}				%No return, no arguments
    catch
	throw:{return,Tag,Ret,Stb} ->
	    %%io:fwrite("fr: ~p\n", [{Tag,Ret,Stb#luerl.env}]),
	    {Ret,Stb};
	throw:{break,Tag,_,_,_,St} ->
	    lua_error({illegal_op,break}, St)
    end.

%% do_repeat(Instrs, LocalVars, Stack, Env, State, RepeatInstrs) -> <emul>

do_repeat(Is, Lvs, Stk, Env, St, Ris) ->
    Do = fun (S) ->
		 repeat_loop(Ris, Lvs, Stk, Env, S)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

repeat_loop(Ris, Lvs0, Stk0, Env0, St0) ->
    {Lvs1,[Val|Stk1],Env1,St1} =
	emul(Ris, Lvs0, Stk0, Env0, St0),
    case boolean_value(Val) of
	true -> {Lvs1,St1};
	false -> repeat_loop(Ris, Lvs1, Stk1, Env1, St1)
    end.

%% do_while(Instrs, LocalVars, Stack, Env, State, WhileEis, WhileBis) ->
%%     <emul>

do_while(Is, Lvs, Stk, Env, St, Eis, Wis) ->
    Do = fun (S) ->
		 while_loop(Eis, Lvs, Stk, Env, S, Wis)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

while_loop(Eis, Lvs0, Stk0, Env0, St0, Wis) ->
    {Lvs1,[Val|Stk1],Env1,St1} =
	emul(Eis, Lvs0, Stk0, Env0, St0),
    case boolean_value(Val) of
	true ->
	    {Lvs2,Stk2,Env2,St2} =
		emul(Wis, Lvs1, Stk1, Env1, St1),
	    while_loop(Eis, Lvs2, Stk2, Env2, St2, Wis);
	false -> {Lvs1,St1}
    end.

loop_block(Is, Lvs0, Stk, Env, St0, Do) ->
    Tag = St0#luerl.tag,
    {Lvs2,St1} = try
		     Do(St0)
		 catch
		     throw:{break,Tag,Lvs1,_,_,St} -> {Lvs1,St}
		 end,
    %% Trim local variable stack.
    Lvs3 = lists:nthtail(length(Lvs2)-length(Lvs0), Lvs2),
    emul(Is, Lvs3, Stk, Env, St1).

%% do_if(Blocks, Else, Lvs, Stk, Env, Sy) ->
%%     do_if_blocks(Blocks, Else, Lvs, Stk, Env, St).

%% do_if_blocks([{T,B}|Ts], Else, Lvs0, Stk0, Env0, St0) ->
%%     {Lvs1,[Val|Stk1],Env1,St1} = emul(T, Lvs0, Stk0, Env0, St0),
%%     case boolean_value(Val) of
%% 	true -> emul(B, Lvs1, Stk1, Env1, St1);
%% 	false -> do_if_blocks(Ts, Lvs1, Stk1, Env1, St1)
%%     end;
%% do_if_blocks([], Else, Lvs, Stk, Env, St) ->
%%     emul(Else, Lvs, Stk, Env, St).

%% do_if(LocalVars, Stack, Env, State, TrueInstrs, FalseInstrs) ->
%%     {LocalVars,Stack,Env,State}.

do_if(Lvs, [Val|Stk], Env, St, True, False) ->
    case boolean_value(Val) of
	true -> emul(True, Lvs, Stk, Env, St);
	false -> emul(False, Lvs, Stk, Env, St)
    end.

%% do_if_block([?BLOCK(Lsz, Esz, Bis)], Lvs0, Stk0, Env0, St0, Is) ->
%%     {Lvs1,Stk1,Env1,St1} = do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz),
%%     emul(Is, Lvs1, Stk1, Env1, St1);
%% do_if_block(Bis, Lvs0, Stk0, Env0, St0, Is) ->
%%     {Lvs1,Stk1,Env1,St1} = emul(Bis, Lvs0, Stk0, Env0, St0),
%%     emul(Is, Lvs1, Stk1, Env1, St1).

%% do_numfor(Instrs, LocalVars, Stack, Env, State, Varname, FromInstrs) ->
%%     <emul>

do_numfor(Is, Lvs, [Step,Limit,Init|Stk], Env, St, _, Fis) ->
    %% First check if we have numbers.
    case luerl_lib:args_to_numbers([Init,Limit,Step]) of
	[I,L,S] ->
	    Do = fun (St_) ->
			 numfor_loop(I, L, S, Fis, Lvs, Stk, Env, St_)
		 end,
	    loop_block(Is, Lvs, Stk, Env, St, Do);
	error -> badarg_error(loop, [Init,Limit,Step], St)
    end.

numfor_loop(N, Limit, Step, Fis, Lvs0, Stk0, Env0, St0) ->
    %% Leave the counter at the top of the stack for code to get.
    itrace_print("nl: ~p\n", [{N,Stk0}]),
    if Step > 0, N =< Limit ->			%Keep going
	    {Lvs1,Stk1,Env1,St1} =
		emul(Fis, Lvs0, [N|Stk0], Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       Step < 0, N >= Limit ->			%Keep going
	    {Lvs1,Stk1,Env1,St1} =
		emul(Fis, Lvs0, [N|Stk0], Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       true -> {Lvs0,St0}				%Done!
    end.

%% do_genfor(Instrs, LocalVars, Stack, Env, State, Vars, FromInstrs) -> <emul>

do_genfor(Is, Lvs, [Val|Stk], Env, St, _, Fis) ->
    case Val of					%Export F, T, V
	[F] -> T = nil, V = nil;
	[F,T] -> V = nil;
	[F,T,V|_] -> ok;
	F -> T = nil, V = nil
    end,
    Do = fun (St_) ->
		 genfor_loop(F, T, V, Fis, Lvs, Stk, Env, St_)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

genfor_loop(Func, Tab, Val, Fis, Lvs0, Stk, Env, St0) ->
    {Vals,St1} = functioncall(Func, [Tab,Val], Stk, St0),
    case boolean_value(Vals) of
	true ->
	    {Lvs1,_,_,St2} =
		emul(Fis, Lvs0, [Vals|Stk], Env, St1),
	    genfor_loop(Func, Tab, hd(Vals), Fis, Lvs1, Stk, Env, St2);
	false -> {Lvs0,St1}
    end.

%% get_metamethod(Object1, Object2, Event, State) -> Metod | nil.
%% get_metamethod(Object, Event, State) -> Method | nil.
%% Get the metamethod for object(s).

get_metamethod(O1, O2, E, St) ->
    case get_metamethod(O1, E, St) of
	nil -> get_metamethod(O2, E, St);
	M -> M
    end.

get_metamethod(O, E, St) ->
    Meta = get_metatable(O, St),			%Can be nil
    get_metamethod_tab(Meta, E, St#luerl.ttab).

get_metamethod_tab(#tref{i=M}, E, Ts) ->
    #table{d=Mdict} = ?GET_TABLE(M, Ts),
    case ttdict:find(E, Mdict) of
	{ok,Mm} -> Mm;
	error -> nil
    end;
get_metamethod_tab(_, _, _) -> nil.		%Other types have no metatables

get_metatable(#tref{i=T}, #luerl{ttab=Ts}) ->
    (?GET_TABLE(T, Ts))#table.meta;
get_metatable(#usdref{i=U}, #luerl{usdtab=Us}) ->
    (?GET_TABLE(U, Us))#userdata.meta;
get_metatable(nil, #luerl{meta=Meta}) -> Meta#meta.nil;
get_metatable(B, #luerl{meta=Meta}) when is_boolean(B) ->
    Meta#meta.boolean;
get_metatable(N, #luerl{meta=Meta}) when is_number(N) ->
    Meta#meta.number;
get_metatable(S, #luerl{meta=Meta}) when is_binary(S) ->
    Meta#meta.string;
get_metatable(_, _) -> nil.			%Other types have no metatables

set_metatable(#tref{i=N}, M, #luerl{ttab=Ts0}=St) ->
    Ts1 = ?UPD_TABLE(N, fun (Tab) -> Tab#table{meta=M} end, Ts0),
    St#luerl{ttab=Ts1};
set_metatable(#usdref{i=N}, M, #luerl{usdtab=Us0}=St) ->
    Us1 = ?UPD_TABLE(N, fun (Ud) -> Ud#userdata{meta=M} end, Us0),
    St#luerl{usdtab=Us1};
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
set_metatable(_, _, St) ->			%Do nothing for the rest
    St.

%% build_tab(FieldCount, Index, Stack, State) -> {TableRef,Stack,State}.
%%  FieldCount is how many Key/Value pairs are on the stack, Index is
%%  the index of the next value in the acc.

build_tab(Fc, I, [Last|Stk0], St0) ->
    Fs0 = build_tab_last(I, Last),
    {Fs1,Stk1} = build_tab_loop(Fc, Stk0, Fs0),
    %% io:fwrite("bt: ~p\n", [{Fc,I,Acc,Fs0,Fs1}]),
    {Tref,St1} = alloc_table(Fs1, St0),
    {Tref,Stk1,St1}.

build_tab_last(I, [V|Vs]) ->
    [{I,V}|build_tab_last(I+1, Vs)];
build_tab_last(_, []) -> [];
build_tab_last(_, Last) -> error({boom,build_tab_acc,Last}).

build_tab_loop(0, Stk, Fs) -> {Fs,Stk};
build_tab_loop(C, [V,K|Stk], Fs) ->
    build_tab_loop(C-1, Stk, [{K,V}|Fs]).

%% op(Op, Arg) -> {ok,Ret} | {meta,Func} | {error,Error}.
%% op(Op, Arg1, Arg2) -> {ok,Ret} | {meta,Func} | {error,Error}.
%%  The built-in operators. Always return a single value!

op('-', A) ->
    numeric_op('-', A, <<"__unm">>, fun (N) -> -N end);
op('not', A) -> {ok,not ?IS_TRUE(A)};
op('~', A) ->
    integer_op('~', A, <<"__bnot">>, fun (N) -> {ok,bnot(N)} end);
op('#', B) when is_binary(B) -> {ok,byte_size(B)};
op('#', #tref{}=T) ->
    {meta,fun (_, St) -> luerl_lib_table:length(T, St) end};
op(Op, A) -> {error,{badarg,Op,[A]}}.

%% Numeric operators.
op('+', A1, A2) ->
    numeric_op('+', A1, A2, <<"__add">>, fun (N1,N2) -> N1+N2 end);
op('-', A1, A2) ->
    numeric_op('-', A1, A2, <<"__sub">>, fun (N1,N2) -> N1-N2 end);
op('*', A1, A2) ->
    numeric_op('*', A1, A2, <<"__mul">>, fun (N1,N2) -> N1*N2 end);
op('/', A1, A2) ->
    numeric_op('/', A1, A2, <<"__div">>, fun (N1,N2) -> N1/N2 end);
%% The '//' and '%' operators are specially handled to avoid first
%% converting integers to floats and potentially lose precision.
op('//', A1, A2) ->
    numeric_op('//', A1, A2, <<"__idiv">>,
	       fun (N1,N2) when is_integer(N1), is_integer(N2) ->
		       Idiv = N1 div N2,
		       Irem = N1 rem N2,
		       if Irem =:= 0 -> Idiv;
			  Idiv < 0 -> Idiv - 1;
			  true -> Idiv
		       end;
		   (N1,N2) -> 0.0 + floor(N1/N2) end);
op('%', A1, A2) ->
    numeric_op('%', A1, A2, <<"__mod">>,
               fun (N1,N2) when is_integer(N1), is_integer(N2) ->
                       Irem = N1 rem N2,
                       if (Irem < 0) and (N2 >= 0) -> Irem + N2;
                          (Irem > 0) and (N2 < 0) -> Irem + N2;
                          true -> Irem
                       end;
                       %% if Irem < 0 ->
                       %%         if N2 < 0 -> Irem;
                       %%            true -> Irem + N2
                       %%         end;
                       %%    Irem > 0 ->
                       %%         if N2 < 0 -> Irem + N2;
                       %%            true -> Irem
                       %%         end;
                       %%    true -> 0             %Irem =:= 0
                       %% end;
                   (N1,N2) -> N1 - floor(N1/N2)*N2 end);
op('^', A1, A2) ->
    numeric_op('^', A1, A2, <<"__pow">>,
	       fun (N1,N2) -> math:pow(N1, N2) end);
%% Bitwise operators.

%% The '>>' is an arithmetic shift as a logical shift implies a word
%% size which we don't have.
op('&', A1, A2) ->
    integer_op('&', A1, A2, <<"__band">>, fun (N1,N2) -> N1 band N2 end);
op('|', A1, A2) ->
    integer_op('|', A1, A2, <<"__bor">>, fun (N1,N2) -> N1 bor N2 end);
op('~', A1, A2) ->
    integer_op('|', A1, A2, <<"__bxor">>, fun (N1,N2) -> N1 bxor N2 end);
op('<<', A1, A2) ->
    integer_op('<<', A1, A2, <<"__shl">>, fun (N1,N2) -> N1 bsl N2 end);
op('>>', A1, A2) ->
    integer_op('>>', A1, A2, <<"__shr">>, fun (N1,N2) -> N1 bsr N2 end);
%% Relational operators, getting close.
op('==', A1, A2) -> eq_op('==', A1, A2);
op('~=', A1, A2) -> neq_op('~=', A1, A2);
op('<=', A1, A2) -> le_op('<=', A1, A2);
op('>=', A1, A2) -> le_op('>=', A2, A1);
op('<', A1, A2) -> lt_op('<', A1, A2);
op('>', A1, A2) -> lt_op('>', A2, A1);
%% String operator.
op('..', A1, A2) -> concat_op(A1, A2);
%% Bad args here.
op(Op, A1, A2) -> {error,{badarg,Op,[A1,A2]}}.

-ifndef(HAS_FLOOR).
%% floor(Number) -> integer().
%%  Floor does not exist before 20 so we need to do it ourselves.

floor(N) when is_integer(N) -> N;
floor(N) when is_float(N) -> round(N - 0.5).
-endif.

%% numeric_op(Op, Arg, Event, Raw) -> {ok,Res} | {meta,Meta}.
%% numeric_op(Op, Arg, Arg, Event, Raw) -> {ok,Res} | {meta,Meta}.
%% eq_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% neq_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% lt_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% le_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%% concat_op(Op, Arg, Arg) -> {ok,Res} | {meta,Meta}.
%%  Together with their metas straight out of the reference
%%  manual. Note that numeric_op string args are always floats.

numeric_op(Op, A, E, Raw) ->
    case arg_to_number(A) of
	error ->				%Neither number nor string
	    {meta,fun (_, St) -> numeric_meta(Op, A, E, St) end}; 
	N -> {ok,Raw(N)}
    end.

numeric_op(Op, A1, A2, E, Raw) ->
    case arg_to_number(A1) of
	error -> {meta,fun (_, St) -> numeric_meta(Op, A1, A2, E, St) end};
	N1 ->
	    case arg_to_number(A2) of
		error ->
		    {meta,fun (_, St) -> numeric_meta(Op, A1, A2, E, St) end};
		N2 -> {ok,Raw(N1, N2)}
	    end
    end.

arg_to_number(Arg) ->
    case luerl_lib:arg_to_number(Arg) of
	error -> error;
	N when is_binary(Arg) -> float(N);	%String arg always float
	N -> N
    end.

integer_op(Op, A, E, Raw) ->
    case luerl_lib:arg_to_exact_integer(A) of
	error -> {meta,fun (_, St) -> numeric_meta(Op, A, E, St) end};
	N -> {ok,Raw(N)}
    end.

integer_op(Op, A1, A2, E, Raw) ->
    case luerl_lib:arg_to_exact_integer(A1) of
	error -> {meta,fun (_, St) -> numeric_meta(Op, A1, A2, E, St) end};
	N1 ->
	    case luerl_lib:arg_to_exact_integer(A2) of
		error ->
		    {meta,fun (_, St) -> numeric_meta(Op, A1, A2, E, St) end};
		N2 -> {ok,Raw(N1, N2)}
	    end
    end.

numeric_meta(Op, A, E, St0) ->
    case get_metamethod(A, E, St0) of
	nil -> badarg_error(Op, [A], St0);	%No meta method
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A], St0),
	    {first_value(Ret),St1}
    end.

numeric_meta(Op, A1, A2, E, St0) ->
    case get_metamethod(A1, A2, E, St0) of
	nil -> badarg_error(Op, [A1,A2], St0);	%No meta methods
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {first_value(Ret),St1}
    end.

eq_op(_Op, A1, A2) when A1 == A2 -> {ok,true};
eq_op(_Op, A1, A2) ->
    {meta,fun (_, St) -> eq_meta(A1, A2, St) end}.

neq_op(_Op, A1, A2) when A1 == A2 -> {ok,false};
neq_op(_Op, A1, A2) ->
    {meta,fun (_, St0) ->
		  {Ret,St1} = eq_meta(A1, A2, St0),
		  {not Ret,St1}
	  end}.

eq_meta(A1, A2, St0) ->
    %% Must have "same" metamethod here. How do we test?
    case get_metamethod(A1, <<"__eq">>, St0) of
	nil -> {false,St0};			%Tweren't no method
	Meta ->
	    case get_metamethod(A2, <<"__eq">>, St0) of
		Meta ->				%Must be the same method
		    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
		    {boolean_value(Ret),St1};
		_ -> {false,St0}
	    end
    end.

lt_op(_Op, A1, A2) when is_number(A1), is_number(A2) -> {ok,A1 < A2};
lt_op(_Op, A1, A2) when is_binary(A1), is_binary(A2) -> {ok,A1 < A2};
lt_op(Op, A1, A2) ->
    {meta,fun (_, St) -> lt_meta(Op, A1, A2, St) end}.

lt_meta(Op, A1, A2, St0) ->
    case get_metamethod(A1, A2, <<"__lt">>, St0) of
	nil -> badarg_error(Op, [A1,A2], St0);
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {boolean_value(Ret),St1}
    end.

le_op(_Op, A1, A2) when is_number(A1), is_number(A2) -> {ok,A1 =< A2};
le_op(_Op, A1, A2) when is_binary(A1), is_binary(A2) -> {ok,A1 =< A2};
le_op(Op, A1, A2) ->
    {meta,fun (_, St) -> le_meta(Op, A1, A2, St) end}.

le_meta(Op, A1, A2, St0) ->
    %% Must check for first __le then __lt metamethods.
    case get_metamethod(A1, A2, <<"__le">>, St0) of
	nil ->
	    %% Try for not (Op2 < Op1) instead.
	    case get_metamethod(A1, A2, <<"__lt">>, St0) of
		nil -> badarg_error(Op, [A1,A2], St0);
		Meta ->
		    {Ret,St1} = functioncall(Meta, [A2,A1], St0),
		    {not boolean_value(Ret),St1}
	    end;
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {boolean_value(Ret),St1}
    end.

concat_op(A1, A2) ->
    case luerl_lib:arg_to_string(A1) of
	error ->
	    {meta,fun (_, St) -> concat_meta(A1, A2, St) end};
	S1 ->
	    case luerl_lib:arg_to_string(A2) of
		error ->
		    {meta,fun (_, St) -> concat_meta(A1, A2, St) end};
		S2 ->
		    {ok,<<S1/binary,S2/binary>>}
	    end
    end.

concat_meta(A1, A2, St0) ->
    case get_metamethod(A1, A2, <<"__concat">>, St0) of
	nil -> badarg_error('..', [A1,A2], St0);
	Meta ->
	    {Ret,St1} = functioncall(Meta, [A1,A2], St0),
	    {first_value(Ret),St1}
    end.

%% boolean_value(Rets) -> boolean().
%%  Return the "boolean" value of a value/function return list.

boolean_value([nil|_]) -> false;
boolean_value([false|_]) -> false;
boolean_value([_|_]) -> true;
boolean_value([]) -> false;
boolean_value(nil) -> false;
boolean_value(false) -> false;
boolean_value(_) -> true.

%% first_value(Rets) -> Value.
%% multiple_value(Value) -> [Value].

first_value([V|_]) -> V;
first_value([]) -> nil.

multiple_value(V) -> [V].

%% gc(State) -> State.
%%  The garbage collector. Its main job is to reclaim unused tables
%%  and frames. It is a mark/sweep collector which passes over all
%%  objects and marks tables and frames which it has seen. All unseen
%%  tables and frames are then freed and their indexes added to the
%%  free lists.

-record(gct, {t,s}).				%Gc table info table, seen

gc(#luerl{ttab=Tt0,tfree=Tf0,
	  upvtab=Ft0,upvfree=Ff0,
	  usdtab=Ut0,usdfree=Uf0,
	  funtab=Funt0,funfree=Funf0,
          g=G,stk=Stk,meta=Meta}=St) ->
    %% The root set consisting of global table and stack.
    Root = [Meta#meta.nil,Meta#meta.boolean,Meta#meta.number,Meta#meta.string,
	    G|Stk],
    %% Mark all seen tables and frames, i.e. return them.
    GcT = #gct{t=Tt0,s=[]},
    GcF = #gct{t=Ft0,s=[]},
    GcU = #gct{t=Ut0,s=[]},
    GcFun = #gct{t=Funt0,s=[]},
    {SeenT,SeenF,SeenU,SeenFun} = mark(Root, [], GcT, GcF, GcU, GcFun),
    %% io:format("gc: ~p\n", [{SeenT,SeenF,SeenU}]),
    %% Free unseen tables and add freed to free list.
    {Tf1,Tt1} = filter_tables(SeenT, Tf0, Tt0),
    {Ff1,Ft1} = filter_frames(SeenF, Ff0, Ft0),
    {Uf1,Ut1} = filter_userdata(SeenU, Uf0, Ut0),
    {Funf1,Funt1} = filter_funcdefs(SeenFun, Funf0, Funt0),
    St#luerl{ttab=Tt1,tfree=Tf1,
	     upvtab=Ft1,upvfree=Ff1,
	     usdtab=Ut1,usdfree=Uf1,
	     funtab=Funt1,funfree=Funf1}.

%% mark(ToDo, MoreTodo, GcTabs, GcFrames, GcUserdata, GcFuncdefs) ->
%%     {SeenTabs,SeenFrames,SeenUserdata,SeenFuncdefs}.
%% Scan over all live objects and mark seen tables by adding them to
%% the seen list.

mark([{in_table,_}=_T|Todo], More, GcT, GcF, GcU, GcFun) ->
    %%io:format("gc: ~p\n", [_T]),
    mark(Todo, More, GcT, GcF, GcU, GcFun);
mark([#tref{i=T}|Todo], More, #gct{t=Tt,s=Ts0}=GcT, GcF, GcU, GcFun) ->
    case ordsets:is_element(T, Ts0) of
	true ->					%Already done
	    mark(Todo, More, GcT, GcF, GcU, GcFun);
	false ->				%Mark it and add to todo
	    Ts1 = ordsets:add_element(T, Ts0),
	    #table{a=Arr,d=Dict,meta=Meta} = ?GET_TABLE(T, Tt),
	    %% Have to be careful when adding Tab and Meta as Tab is
	    %% [{Key,Val}], Arr is array and Meta is
	    %% nil|#tref{i=M}. We want lists.
	    Aes = array:sparse_to_list(Arr),
	    Des = ttdict:to_list(Dict),
	    mark([Meta|Todo], [[{in_table,T}],Des,Aes,[{in_table,-T}]|More],
		 GcT#gct{s=Ts1}, GcF, GcU, GcFun)
    end;
mark([#fref{i=F}|Todo], More, GcT, #gct{s=Sf0,t=Ft}=GcF, GcU, GcFun) ->
    case ordsets:is_element(F, Sf0) of
	true ->					%Already done
	    mark(Todo, More, GcT, GcF, GcU, GcFun);
	false ->				%Mark it and add to todo
	    Sf1 = ordsets:add_element(F, Sf0),
	    Ses = tuple_to_list(?GET_TABLE(F, Ft)),
	    mark(Todo, [Ses|More], GcT, GcF#gct{s=Sf1}, GcU, GcFun)
    end;
mark([#usdref{i=U}|Todo], More, GcT, GcF, #gct{s=Su0}=GcU, GcFun) ->
    case ordsets:is_element(U, Su0) of
       true ->                                 %Already done
           mark(Todo, More, GcT, GcF, GcU, GcFun);
       false ->
           Su1 = ordsets:add_element(U, Su0),
           mark(Todo, More, GcT, GcF, GcU#gct{s=Su1}, GcFun)
    end;
mark([#lua_func{upv=Env}|Todo], More, GcT, GcF, GcU, GcFun) ->
    mark(Todo, [Env|More], GcT, GcF, GcU, GcFun);
mark([#funref{i=F}|ToDo], More, GcT, GcF, GcU,
     #gct{t=Funt0,s=Funs0}=GcFun) ->
    case ordsets:is_element(F, Funs0) of
	true ->
	    mark(ToDo, More, GcT, GcF, GcU, GcFun);
	false ->
	    Funs1 = ordsets:add_element(F, Funs0),
	    Fdef = ?GET_TABLE(F, Funt0),
	    Funrefs = Fdef#lua_func.funrefs,
	    mark(ToDo, [Funrefs|More], GcT, GcF, GcU, GcFun#gct{s=Funs1})
    end;
%% Specifically catch these as they would match table key-value pair.
mark([#erl_func{}|Todo], More, GcT, GcF, GcU, GcFun) ->
    mark(Todo, More, GcT, GcF, GcU, GcFun);
mark([#thread{}|Todo], More, GcT, GcF, GcU, GcFun) ->
    mark(Todo, More, GcT, GcF, GcU, GcFun);
mark([#userdata{meta=Meta}|Todo], More, GcT, GcF, GcU, GcFun) ->
    mark([Meta|Todo], More, GcT, GcF, GcU, GcFun);
mark([#call_frame{lvs=Lvs,env=Env}|Todo], More0, GcT, GcF, GcU, GcFun) ->
    More1 = [ tuple_to_list(Lv) || Lv <- Lvs ] ++ [Env|More0],
    mark(Todo, More1, GcT, GcF, GcU, GcFun);
mark([{K,V}|Todo], More, GcT, GcF, GcU, GcFun) -> %Table key-value pair
    %%io:format("mt: ~p\n", [{K,V}]),
    mark([K,V|Todo], More, GcT, GcF, GcU, GcFun);
mark([_|Todo], More, GcT, GcF, GcU, GcFun) ->
    %% Can ignore everything else.
    mark(Todo, More, GcT, GcF, GcU, GcFun);
mark([], [M|More], GcT, GcF, GcU, GcFun) ->
    mark(M, More, GcT, GcF, GcU, GcFun);
mark([], [], #gct{s=St}, #gct{s=Sf}, #gct{s=Su}, #gct{s=Sfun}) ->
    {St,Sf,Su,Sfun}.

%% filter_tables(Seen, Free, Tables) -> {Free,Tables}.
%% filter_frames(Seen, Free, Frames) -> {Free,Frames}.
%% filter_frames(Seen, Free, Frames) -> {Free,Frames}.
%% filter_frames(Seen, Free, Frames) -> {Free,Frames}.
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

filter_frames(Seen, Ff0, Ft0) ->
    %% Update the free list.
    Ff1 = ?FOLD_TABLES(fun (K, _, Free) ->
			       case ordsets:is_element(K, Seen) of
				   true -> Free;
				   false -> [K|Free]
			       end
		       end, Ff0, Ft0),
    Ft1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Ft0),
    {Ff1,Ft1}.

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

filter_funcdefs(Seen, Funf0, Funt0) ->
    %% Update the free list.
    Funf1 = ?FOLD_TABLES(fun (K, _, Free) ->
				case ordsets:is_element(K, Seen) of
				    true -> Free;
				    false -> [K|Free]
				end
			end, Funf0, Funt0),
    Funt1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Funt0),
    {Funf1,Funt1}.
