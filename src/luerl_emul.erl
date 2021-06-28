%% Copyright (c) 2013-2020 Robert Virding
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
%% Purpose : A basic LUA 5.3 machine emulator.

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

-export([functioncall/3,methodcall/4,
	 set_global_key/3,get_global_key/2,
	 get_table_keys/2,get_table_keys/3,
	 set_table_keys/3,set_table_keys/4,
	 get_table_key/3,set_table_key/4
	]).

%% Temporary shadow calls.
-export([alloc_table/2,set_userdata/3,get_metamethod/3]).

%% For testing.
-export([pop_vals/2,push_vals/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).

%% -compile(inline).				%For when we are optimising
%% -compile({inline,[boolean_value/1,first_value/1]}).

%% -define(ITRACE_DO(Expr), ok).
-define(ITRACE_DO(Expr),
	begin (get(luerl_itrace) /= undefined) andalso Expr end).

%% Temporary shadow calls.
gc(St) -> luerl_heap:gc(St).
alloc_table(Itab, St) -> luerl_heap:alloc_table(Itab, St).
set_userdata(Ref, Data, St) ->
    luerl_heap:set_userdata(Ref, Data, St).
get_metamethod(Obj, Event, St) ->
    luerl_heap:get_metamethod(Obj, Event, St).

%% init() -> State.
%% Initialise the basic state.

init() ->
    St1 = luerl_heap:init(),
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

load_libs(Libs, St) ->
    Fun = fun ({Key,Mod}, S) -> load_lib(Key, Mod, S) end,
    lists:foldl(Fun, St, Libs).

%% load_lib(Key, Module, State) -> State.

load_lib(Key, Mod, St0) ->
    {Tab,St1} = Mod:install(St0),
    %% Add key to global and to package.loaded.
    St2 = set_global_key(Key, Tab, St1),
    set_table_keys([<<"package">>,<<"loaded">>,Key], Tab, St2).

%% set_global_key(Key, Value, State) -> State.
%% get_global_key(Key, State) -> {[Val],State}.
%%  Access elements in the global name table, _G.

set_global_key(Key, Val, #luerl{g=G}=St) ->
    set_table_key(G, Key, Val, St).

get_global_key(Key, #luerl{g=G}=St) ->
    get_table_key(G, Key, St).

%% get_table_keys(Keys, State) -> {Value,State}.
%% get_table_keys(Tab, Keys, State) -> {Value,State}.
%%  Search down tables which stops when no more tables.

get_table_keys(Keys, St) ->
    get_table_keys(St#luerl.g, Keys, St).

get_table_keys(Tab, [K|Ks], St0) ->
    {Val,St1} = get_table_key(Tab, K, St0),
    get_table_keys(Val, Ks, St1);
get_table_keys(Val, [], St) -> {Val,St}.

%% set_table_keys(Keys, Val, State) -> State.
%% set_table_keys(Tab, Keys, Val, State) -> State.
%%  Setter down tables.

set_table_keys(Keys, Val, St) ->
    set_table_keys(St#luerl.g, Keys, Val, St).

set_table_keys(Tab, [K], Val, St) ->
    set_table_key(Tab, K, Val, St);
set_table_keys(Tab0, [K|Ks], Val, St0) ->
    {Tab1,St1} = get_table_key(Tab0, K, St0),
    set_table_keys(Tab1, Ks, Val, St1).

%% set_table_key(Tref, Key, Value, State) -> State.
%% get_table_key(Tref, Key, State) -> {Val,State}.
%%  Access tables, as opposed to the environment (which are also
%%  tables). Setting a value to 'nil' will clear it from the array but
%%  not from the table; however, we won't add a nil value.
%%  NOTE: WE ALWAYS RETURN A SINGLE VALUE!

set_table_key(Tref, Key, Val, St0) ->
    case luerl_heap:set_table_key(Tref, Key, Val, St0) of
	{value,_Val,St1} -> St1;
	{meta,Meth,Args,St1} ->
	    {_Ret,St2} = functioncall(Meth, Args, St1),
	    St2;
	{error,Error,St1} ->
	    lua_error(Error, St1)
    end.

get_table_key(Tref, Key, St0) ->
    case luerl_heap:get_table_key(Tref, Key, St0) of
	{value,Val,St1} -> {Val,St1};
	{meta,Meth,Args,St1} ->
	    {Ret,St2} = functioncall(Meth, Args, St1),
	    {first_value(Ret),St2};
	{error,Error,St1} ->
	    lua_error(Error, St1)
    end.

%% set_local_var(Depth, Index, Var, Frames) -> Frames.
%% get_local_var(Depth, Index, Frames) -> Val.

set_local_var(1, I, V, [F|Fs]) ->
    [setelement(I, F, V)|Fs];
set_local_var(D, I, V, [F|Fs]) ->
    [F|set_local_var(D-1, I, V, Fs)].

get_local_var(1, I, [F|_]) -> element(I, F);
get_local_var(D, I, [_|Fs]) ->
    get_local_var(D-1, I, Fs).

%% set_env_var(Depth, Index, Val, EnvStack, State) -> State.
%% get_env_var(Depth, Index, EnvStack, State) -> Val.
%%  We must have the state as the environments are global in the
%%  state.

set_env_var(D, I, Val, Estk, St) ->
    St1 = set_env_var_1(D, I, Val, Estk, St),
    %% io:format("******** SEV DONE ~w ~w ********\n", [D,I]),
    St1.

set_env_var_1(1, I, Val, [Eref|_], St) ->
    luerl_heap:set_env_var(Eref, I, Val, St);
set_env_var_1(2, I, Val, [_,Eref|_], St) ->
    luerl_heap:set_env_var(Eref, I, Val, St);
set_env_var_1(D, I, Val, Env, St) ->
    luerl_heap:set_env_var(lists:nth(D, Env), I, Val, St).

get_env_var(D, I, Env, St) ->
    Val = get_env_var_1(D, I,  Env, St),
    %% io:format("******** GEV DONE ~w ~w ********\n", [D, I]),
    Val.

get_env_var_1(1, I, [Eref|_], St) ->
    luerl_heap:get_env_var(Eref, I, St);
get_env_var_1(2, I, [_,Eref|_], St) ->
    luerl_heap:get_env_var(Eref, I, St);
get_env_var_1(D, I, Env, St) ->
    luerl_heap:get_env_var(lists:nth(D, Env), I, St).

%% load_chunk(FunctionDefCode, State) -> {Function,State}.
%% load_chunk(FunctionDefCode, Env, State) -> {Function,State}.
%%  Load a chunk from the compiler which is a compiled function
%%  definition whose instructions define everything. Return a callable
%%  function reference which defines everything and a updated Luerl
%%  state.

load_chunk(Code, St) -> load_chunk(Code, [], St).

load_chunk([Code], [], St0) ->
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
%% We include the dymanmic instructions here even though the compiler
%% does not generate them. This should make us more future proof.

load_chunk_i(?PUSH_FDEF(Anno, Lsz, Esz, Pars, B0), Funrs0, St0) ->
    {B1,Funrs,St1} = load_chunk_is(B0, [], St0),
    Fdef = #lua_func{anno=Anno,funrefs=Funrs,lsz=Lsz,esz=Esz,pars=Pars,b=B1},
    {Funref,St2} = luerl_heap:alloc_funcdef(Fdef, St1),
    Funrs1 = ordsets:add_element(Funref, Funrs0),
    {?PUSH_FDEF(Funref),Funrs1,St2};
load_chunk_i(?BLOCK(Lsz, Esz, B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?BLOCK(Lsz, Esz, B1),Funrs1,St1};
load_chunk_i(?REPEAT(B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?REPEAT(B1),Funrs1,St1};
load_chunk_i(?REPEAT_LOOP(B0), Funrs0, St0) ->         %This is dynamic
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?REPEAT_LOOP(B1),Funrs1,St1};
load_chunk_i(?WHILE(E0, B0), Funrs0, St0) ->
    {E1,Funrs1,St1} = load_chunk_is(E0, Funrs0, St0),
    {B1,Funrs2,St2} = load_chunk_is(B0, Funrs1, St1),
    {?WHILE(E1, B1),Funrs2,St2};
load_chunk_i(?WHILE_LOOP(E0, B0), Funrs0, St0) ->
    {E1,Funrs1,St1} = load_chunk_is(E0, Funrs0, St0),
    {B1,Funrs2,St2} = load_chunk_is(B0, Funrs1, St1),
    {?WHILE_LOOP(E1, B1),Funrs2,St2};
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
load_chunk_i(?NFOR_LOOP(N, L, S, B0), Funrs0, St0) ->  %This is dynamic
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?NFOR_LOOP(N, L, S, B1),Funrs1,St1};
load_chunk_i(?GFOR(Vs, B0), Funrs0, St0) ->
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?GFOR(Vs, B1),Funrs1,St1};
load_chunk_i(?GFOR_CALL(F, D, V, B0), Funrs0, St0) ->  %This is dynamic
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?GFOR_CALL(F, D, V, B1),Funrs1,St1};
load_chunk_i(?GFOR_LOOP(F, D, B0), Funrs0, St0) ->     %This is dynamic
    {B1,Funrs1,St1} = load_chunk_is(B0, Funrs0, St0),
    {?GFOR_LOOP(F, D, B1),Funrs1,St1};

%% Then the rest which we don't have to worry about.
load_chunk_i(I, Funrs, St) -> {I,Funrs,St}.

%% call(Function, State) -> {Return,State}.
%% call(Function, Args, State) -> {Return,State}.
%% functioncall(Function, Args, State) -> {Return,State}.
%% methodcall(Object, Method, Args, State) -> {Return,State}.
%%  These ares called from the outside and expect everything necessary
%%  to be in the state.

call(Func, St) -> call(Func, [], St).

call(#funref{}=Funref, Args, St0) ->		%Lua function
    {Ret,St1} = functioncall(Funref, Args, St0),
    %% Should do GC here.
    {Ret,St1};
call(#erl_func{}=Func, Args, St0) ->		%Erlang function
    {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1}.

functioncall(Func, Args, #luerl{stk=Stk}=St0) ->
    Fr = #call_frame{func=Func,args=Args,lvs=[],env=[],is=[],cont=[]},
    Cs0 = [Fr],
    {_Lvs,[Ret|_],_Env,Cs1,St1} = functioncall(Func, Args, Stk, Cs0, St0),
    {Ret,St1#luerl{stk=Stk,cs=Cs1}}.		%Reset the stacks

methodcall(Obj, Meth, Args, St0) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, Meth, St0) of
	{nil,St1} ->				%No method
	    lua_error({undefined_method,Obj,Meth}, St1);
	{Func,St1} ->
	    functioncall(Func, [Obj|Args], St1)
    end.

%% emul(Instrs, State).
%% emul(Instrs, Continuation, LocalVariables, Stack, Env, CallStack, State).
%%  The cost of checking the itrace process variable is very slight
%%  compared to everythin else.

emul(Is, St) ->
    emul(Is, [], {}, [], [], [], St).

%% The faster (yeah sure) version.
%% emul(Is, Cont, Lvs, Stk, Env, Cs, St) ->
%%     emul_1(Is, Cont, Lvs, Stk, Env, Cs, St).

%% The tracing versions.
emul([I|_]=Is, Cont, Lvs, Stk, Env, Cs, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("Is:  ~p\n", [Is]),
		   io:fwrite("Cnt: ~p\n", [Cont]),
		   io:fwrite("Lvs: ~p\n", [Lvs]),
		   io:fwrite("Env: ~p\n", [Env]),
		   io:fwrite("Stk: ~p\n", [Stk]),
		   io:fwrite("Cs:  ~p\n", [Cs]),
		   io:fwrite("I: ~p\n", [I]),
		   io:put_chars("--------\n")
	       end),
    emul_1(Is, Cont, Lvs, Stk, Env, Cs, St);
emul([], Cont, Lvs, Stk, Env, Cs, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("Is:  ~p\n", [[]]),
		   io:fwrite("Cnt: ~p\n", [Cont]),
		   io:fwrite("Lvs: ~p\n", [Lvs]),
		   io:fwrite("Env: ~p\n", [Env]),
		   io:fwrite("Stk: ~p\n", [Stk]),
		   io:fwrite("Cs:  ~p\n", [Cs]),
		   io:put_chars("--------\n")
	       end),
    emul_1([], Cont, Lvs, Stk, Env, Cs, St).

%% itrace_print(Format, Args) ->
%%     ?ITRACE_DO(io:fwrite(Format, Args)).

%% Expression instructions.
emul_1([?PUSH_LIT(L)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    emul(Is, Cont, Lvs, [L|Stk], Env, Cs, St);
emul_1([?PUSH_LVAR(D, I)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    Val = get_local_var(D, I, Lvs),
    emul(Is, Cont, Lvs, [Val|Stk], Env, Cs, St);
emul_1([?PUSH_EVAR(D, I)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    Val = get_env_var(D, I, Env, St),
    emul(Is, Cont, Lvs, [Val|Stk], Env, Cs, St);
emul_1([?PUSH_GVAR(Key)|Is], Cont, Lvs, Stk, Env, Cs, St0) ->
    %% We must handle the metamethod and error here.
    case luerl_heap:get_global_key(Key, St0) of
	{value,Val,St1} -> emul(Is, Cont, Lvs, [Val|Stk], Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL,?SINGLE|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end;

emul_1([?PUSH_LAST_LIT(L)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    emul(Is, Cont, Lvs, [[L]|Stk], Env, Cs, St);
emul_1([?PUSH_LAST_LVAR(D, I)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    Val = get_local_var(D, I, Lvs),
    emul(Is, Cont, Lvs, [[Val]|Stk], Env, Cs, St);
emul_1([?PUSH_LAST_EVAR(D, I)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    Val = get_env_var(D, I, Env, St),
    emul(Is, Cont, Lvs, [[Val]|Stk], Env, Cs, St);
emul_1([?PUSH_LAST_GVAR(Key)|Is], Cont, Lvs, Stk, Env, Cs, St0) ->
    %% We must handle the metamethod and error here.
    case luerl_heap:get_global_key(Key, St0) of
	{value,Val,St1} -> emul(Is, Cont, Lvs, [[Val]|Stk], Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end;

emul_1([?STORE_LVAR(D, I)|Is], Cont, Lvs0, [Val|Stk], Env, Cs, St) ->
    Lvs1 = set_local_var(D, I, Val, Lvs0),
    emul(Is, Cont, Lvs1, Stk, Env, Cs, St);
emul_1([?STORE_EVAR(D, I)|Is], Cont, Lvs, [Val|Stk], Env, Cs, St0) ->
    St1 = set_env_var(D, I, Val, Env, St0),
    emul(Is, Cont, Lvs, Stk, Env, Cs, St1);
emul_1([?STORE_GVAR(Key)|Is], Cont, Lvs, [Val|Stk], Env, Cs, St0) ->
    %% We must handle the metamethod and error here.
    case luerl_heap:set_global_key(Key, Val, St0) of
	{value,_,St1} -> emul(Is, Cont, Lvs, Stk, Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL,?POP|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end;

emul_1([?GET_KEY|Is], Cont, Lvs, [Key,Tab|Stk], Env, Cs, St) ->
    do_get_key(Is, Cont, Lvs, Stk, Env, Cs, St, Tab, Key);
emul_1([?GET_LIT_KEY(Key)|Is], Cont, Lvs, [Tab|Stk], Env, Cs, St) ->
    %% [?PUSH_LIT(Key),?GET_KEY]
    do_get_key(Is, Cont, Lvs, Stk, Env, Cs, St, Tab, Key);
emul_1([?SET_KEY|Is], Cont, Lvs, [Key,Tab,Val|Stk], Env, Cs, St) ->
    do_set_key(Is, Cont, Lvs, Stk, Env, Cs, St, Tab, Key, Val);
emul_1([?SET_LIT_KEY(Key)|Is], Cont, Lvs, [Tab,Val|Stk], Env, Cs, St) ->
    %% [?PUSH_LIT(Key),?SET_KEY]
    do_set_key(Is, Cont, Lvs, Stk, Env, Cs, St, Tab, Key, Val);

emul_1([?SINGLE|Is], Cont, Lvs, [Val|Stk], Env, Cs, St) ->
    emul(Is, Cont, Lvs, [first_value(Val)|Stk], Env, Cs, St);
emul_1([?MULTIPLE|Is], Cont, Lvs, [Val|Stk], Env, Cs, St) ->
    emul(Is, Cont, Lvs, [multiple_value(Val)|Stk], Env, Cs, St);

emul_1([?BUILD_TAB(Fc, I)|Is], Cont, Lvs, Stk0, Env, Cs, St0) ->
    {Tab,Stk1,St1} = build_tab(Fc, I, Stk0, St0),
    emul(Is, Cont, Lvs, [Tab|Stk1], Env, Cs, St1);
emul_1([?FCALL|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_fcall(Is, Cont, Lvs, Stk, Env, Cs, St);
emul_1([?TAIL_FCALL|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_tail_fcall(Is, Cont, Lvs, Stk, Env, Cs, St);
emul_1([?MCALL(M)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_mcall(Is, Cont, Lvs, Stk, Env, Cs, St, M);
emul_1([?TAIL_MCALL(M)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_tail_mcall(Is, Cont, Lvs, Stk, Env, Cs, St, M);
emul_1([?OP(Op,1)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_op1(Is, Cont, Lvs, Stk, Env, Cs, St, Op);
emul_1([?OP(Op,2)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_op2(Is, Cont, Lvs, Stk, Env, Cs, St, Op);
emul_1([?PUSH_FDEF(Funref)|Is], Cont, Lvs, Stk, Env, Cs, St0) ->
    %% Update the env field of the function reference with the current
    %% environment.
    Funref1 = Funref#funref{env=Env},
    emul(Is, Cont, Lvs, [Funref1|Stk], Env, Cs, St0);

%% Control instructions.

emul_1([?BLOCK(Lsz, Esz, Bis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_block(Is, Cont, Lvs, Stk, Env, Cs, St, Lsz, Esz, Bis);
emul_1([?BLOCK_OPEN(Lsz, Esz)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_block_open(Is, Cont, Lvs, Stk, Env, Cs, St, Lsz, Esz);
emul_1([?BLOCK_CLOSE|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_block_close(Is, Cont, Lvs, Stk, Env, Cs, St);

emul_1([?WHILE(Eis, Wis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_while(Is, Cont, Lvs, Stk, Env, Cs, St, Eis, Wis);
emul_1([?WHILE_LOOP(Eis, Wis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_while_loop(Is, Cont, Lvs, Stk, Env, Cs, St, Eis, Wis);

emul_1([?REPEAT(Ris)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_repeat(Is, Cont, Lvs, Stk, Env, Cs, St, Ris);
emul_1([?REPEAT_LOOP(Ris)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_repeat_loop(Is, Cont, Lvs, Stk, Env, Cs, St, Ris);

emul_1([?AND_THEN(Then)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_and_then(Is, Cont, Lvs, Stk, Env, Cs, St, Then);
emul_1([?OR_ELSE(Else)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_or_else(Is, Cont, Lvs, Stk, Env, Cs, St, Else);
emul_1([?IF_TRUE(True)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_if_true(Is, Cont, Lvs, Stk, Env, Cs, St, True);
emul_1([?IF(True, False)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_if(Is, Cont, Lvs, Stk, Env, Cs, St, True, False);

emul_1([?NFOR(V, Fis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_numfor(Is, Cont, Lvs, Stk, Env, Cs, St, V, Fis);
emul_1([?NFOR_LOOP(N,L,S,Fis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_numfor_loop(Is, Cont, Lvs, Stk, Env, Cs, St, N, L, S, Fis);

emul_1([?GFOR(Vs, Fis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_genfor(Is, Cont, Lvs, Stk, Env, Cs, St, Vs, Fis);
emul_1([?GFOR_CALL(Func, Data, Val, Fis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_genfor_call(Is, Cont, Lvs, Stk, Env, Cs, St, Func, Data, Val, Fis);
emul_1([?GFOR_LOOP(Func, Data, Fis)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_genfor_loop(Is, Cont, Lvs, Stk, Env, Cs, St, Func, Data, Fis);

emul_1([?BREAK|_], _Cont, Lvs, _Stk, _Env, Cs, St) ->
    do_break(Lvs, Cs, St);
emul_1([?RETURN(Ac)|_], _Cont, _Lvs, Stk, _Env, Cs, St) ->
    do_return(Ac, Stk, Cs, St);

%% Stack instructions
emul_1([?POP|Is], Cont, Lvs, [_|Stk], Env, Cs, St) ->
    emul(Is, Cont, Lvs, Stk, Env, Cs, St);
emul_1([?POP2|Is], Cont, Lvs, [_,_|Stk], Env, Cs, St) ->
    emul(Is, Cont, Lvs, Stk, Env, Cs, St);
emul_1([?SWAP|Is], Cont, Lvs, [S1,S2|Stk], Env, Cs, St) ->
    emul(Is, Cont, Lvs, [S2,S1|Stk], Env, Cs, St);
emul_1([?DUP|Is], Cont, Lvs, [V|_]=Stk, Env, Cs, St) ->
    emul(Is, Cont, Lvs, [V|Stk], Env, Cs, St);
emul_1([?PUSH_VALS(Vc)|Is], Cont, Lvs, [Vals|Stk0], Env, Cs, St) ->
    %% Pop value list off the stack and push Vc vals from it.
    Stk1 = push_vals(Vc, Vals, Stk0),
    emul(Is, Cont, Lvs, Stk1, Env, Cs, St);
emul_1([?POP_VALS(Vc)|Is], Cont, Lvs, Stk0, Env, Cs, St) ->
    %% Pop Vc vals off the stack, put in a list and push onto the stack.
    {Vals,Stk1} = pop_vals(Vc, Stk0),
    emul(Is, Cont, Lvs, [Vals|Stk1], Env, Cs, St);
emul_1([?PUSH_ARGS(Al)|Is], Cont, Lvs, [Args|Stk0], Env, Cs, St) ->
    %% Pop argument list off the stack and push args onto the stack.
    Stk1 = push_args(Al, Args, Stk0),
    emul(Is, Cont, Lvs, Stk1, Env, Cs, St);
emul_1([?POP_ARGS(Ac)|Is], Cont, Lvs, Stk0, Env, Cs, St) ->
    %% Pop Ac args off the stack, put in a list and push onto the stack.
    {Args,Stk1} = pop_vals(Ac, Stk0),
    emul(Is, Cont, Lvs, [Args|Stk1], Env, Cs, St);
emul_1([?COMMENT(_)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    %% This just a comment which is ignored.
    emul(Is, Cont, Lvs, Stk, Env, Cs, St);
emul_1([?CURRENT_LINE(Line,File)|Is], Cont, Lvs, Stk, Env, Cs, St) ->
    do_current_line(Is, Cont, Lvs, Stk, Env, Cs, St, Line, File);
emul_1([], [Is|Cont], Lvs, Stk, Env, Cs, St) ->
    emul(Is, Cont, Lvs, Stk, Env, Cs, St);
emul_1([], [], Lvs, Stk, Env, Cs, St) ->
    {Lvs,Stk,Env,Cs,St}.

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

%% do_set_key(Instrs, LocalVars, Stack, Env, State, Table, Key, Val) ->
%%  ReturnFromEmul.
%% do_get_key(Instrs, LocalVars, Stack, Env, State, Table, Key) ->
%%  ReturnFromEmul.

do_set_key(Is, Cont, Lvs, Stk, Env, Cs, St0, Tab, Key, Val) ->
    %% We must handle the metamethod and error here.
    case luerl_heap:set_table_key(Tab, Key, Val, St0) of
	{value,_,St1} -> emul(Is, Cont, Lvs, Stk, Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL,?POP|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end.

do_get_key(Is, Cont, Lvs, Stk, Env, Cs, St0, Tab, Key) ->
    %% We must handle the metamethod and error here.
    case luerl_heap:get_table_key(Tab, Key, St0) of
	{value,Val,St1} -> emul(Is, Cont, Lvs, [Val|Stk], Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL,?SINGLE|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end.

%% do_op1(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.
%% do_op2(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.

do_op1(Is, Cont, Lvs, [A|Stk], Env, Cs, St0, Op) ->
    case op(Op, A, St0) of
	{value,Res,St1} -> emul(Is, Cont, Lvs, [Res|Stk], Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL,?SINGLE|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end.

do_op2(Is, Cont, Lvs, [A2,A1|Stk], Env, Cs, St0, Op) ->
    case op(Op, A1, A2, St0) of
	{value,Res,St1} -> emul(Is, Cont, Lvs, [Res|Stk], Env, Cs, St1);
	{meta,Meth,Args,St1} ->
	    emul([?FCALL,?SINGLE|Is], Cont, Lvs, [Args,Meth|Stk], Env, Cs, St1);
	{error,Error,St1} ->
	    lua_error(Error, St1#luerl{stk=Stk,cs=Cs})
    end.

%% do_break(LocalVars, CallStack, State) -> <emul>.
do_break(Lvs0, Cs0, St) ->
    {Bf,Cs1} = find_loop_frame(Cs0, St),
    #loop_frame{is=Is,cont=Cont,lvs=Lvs1,stk=Stk,env=Env} = Bf,
    %% Trim the new local variable stack down to original length.
    Lvs2 = lists:nthtail(length(Lvs0)-length(Lvs1), Lvs0),
    emul(Is, Cont, Lvs2, Stk, Env, Cs1, St).

%% do_return(ArgCount, Stack, Callstack, State) -> <emul>.

do_return(Ac, Stk0, Cs0, St0) ->
    {Cf,Cs1} = find_call_frame(Cs0, St0),       %Find the first call frame
    {Ret,Stk1} = pop_vals(Ac, Stk0),
    %% When tracing bring the state up to date and call the tracer.
    Tfunc = St0#luerl.trace_func,
    St1  = if is_function(Tfunc) ->
                   Tfunc(?RETURN(Ret), St0#luerl{stk=Stk1,cs=Cs1});
              true -> St0
           end,
    #call_frame{is=Is,cont=Cont,lvs=Lvs,env=Env} = Cf,
    emul(Is, Cont, Lvs, [Ret|Stk1], Env, Cs1, St1#luerl{cs=Cs1}).

find_call_frame([#call_frame{}=Cf|Cs], _St) -> {Cf,Cs};
find_call_frame([_|Cs], St) -> find_call_frame(Cs, St).

find_loop_frame([#current_line{}|Cs], St) ->	%Skip current line info
    find_loop_frame(Cs, St);
find_loop_frame([#loop_frame{}=Bf|Cs], _St) -> {Bf,Cs};
find_loop_frame(Cs, St) ->
    lua_error({illegal_op,break}, St#luerl{cs=Cs}).

%% do_current_line(Instrs, Continuation, LocalVars, Stack, Env, Stack, State,
%%                 Line, File).

do_current_line(Is, Cont, Lvs, Stk, Env, Cs0, St0, Line, File) ->
    Cs1 = push_current_line(Cs0, Line, File),   %Push onto callstack
    %% When tracing bring the state up to date and call the tracer.
    Tfunc = St0#luerl.trace_func,
    St1 = if is_function(Tfunc) ->
                  Tfunc(?CURRENT_LINE(Line, File), St0#luerl{stk=Stk,cs=Cs1});
             true -> St0
          end,
    emul(Is, Cont, Lvs, Stk, Env, Cs1, St1).

%% push_current_line(CallStack, CurrLine, FileName) -> CallStack.
%%  Push the current line info on the stack replacing an existing one
%%  on the top.

push_current_line([#current_line{}|Cs], Line, File) ->
    [#current_line{line=Line,file=File}|Cs];
push_current_line(Cs, Line, File) ->
    [#current_line{line=Line,file=File}|Cs].

%% do_fcall(Instrs, LocalVars, Stack, Env, State) -> ReturnFromEmul.
%%  Pop arg list and function from stack and do call.

do_fcall(Is, Cont, Lvs, [Args,Func|Stk], Env, Cs, St) ->
    functioncall(Is, Cont, Lvs, Stk, Env, Cs, St, Func, Args).

%% functioncall(Instrs, Cont, LocalVars, Stk, Env, CallStack, State, Func, Args) ->
%%     <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

functioncall(Is, Cont, Lvs, Stk, Env, Cs0, St, Func, Args) ->
    Fr = #call_frame{func=Func,args=Args,lvs=Lvs,env=Env,is=Is,cont=Cont},
    Cs1 = [Fr|Cs0],
    functioncall(Func, Args, Stk, Cs1, St).

%% do_tail_fcall(Instrs, Cont, LocalVars, Stack, Env, State) ->
%%     ReturnFromEmul.

do_tail_fcall(_Is, _Cont, _Lvs, [Args,Func|_Stk], _Env, Cs, St) ->
    error({tail_fcall,Func,Args,Cs,St}).

%% do_mcall(Instrs, Cont, LocalVars, Stack, Env, State, Method) ->

do_mcall(Is, Cont, Lvs, [Args,Obj|Stk], Env, Cs, St, M) ->
    methodcall(Is, Cont, Lvs, Stk, Env, Cs, St, Obj, M, Args).

%% methodcall(Instrs, Cont, Var, Stk, Env, State, Object, Method, Args) ->
%%     <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

methodcall(Is, Cont, Lvs, Stk, Env, Cs, St0, Obj, Meth, Args) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, Meth, St0) of
	{nil,St1} ->				%No method
	    lua_error({undefined_method,Obj,Meth}, St1#luerl{stk=Stk,cs=Cs});
	{Func,St1} ->
	    functioncall(Is, Cont, Lvs, Stk, Env, Cs, St1, Func, [Obj|Args])
    end.

%% do_tail_mcall(Instrs, Cont, LocalVars, Stack, Env, State, Method) ->
%%     <emul>.

do_tail_mcall(_Is, _Cont, _Lvs, [Args,Obj|_Stk], _Env, Cs, St, Meth) ->
    error({tail_mcall,Obj,Meth,Args,Cs,St}).

%% functioncall(Function, Args, Stack, CallStack, State) -> {Return,State}.
%%  Setup environment for function and do the actual call.

functioncall(#funref{env=Env}=Funref, Args, Stk, Cs, St0) ->
    %% When tracing bring the state up to date and call the tracer.
    Tfunc = St0#luerl.trace_func,
    St1 = if is_function(Tfunc) ->
                  Tfunc({fcall,Funref,Args}, St0);
             true -> St0
          end,
    %% Here we must save the stack in state as function may need it.
    {Func,St2} = luerl_heap:get_funcdef(Funref, St1#luerl{stk=Stk}),
    call_luafunc(Func, Args, Stk, Env, Cs, St2);
functioncall(#erl_func{code=Func}, Args, Stk, Cs, St) ->
    call_erlfunc(Func, Args, Stk, Cs, St);
functioncall(Func, Args, Stk, Cs, St) ->
    case luerl_heap:get_metamethod(Func, <<"__call">>, St) of
	nil -> lua_error({undefined_function,Func}, St#luerl{stk=Stk,cs=Cs});
	Meta ->
	    functioncall(Meta, [Func|Args], Stk, Cs, St)
    end.

%% call_luafunc(LuaFunc, Args, Stack, Env, State) -> {Return,State}.
%%  Make the local variable and Env frames and push them onto
%%  respective stacks and call the function.

call_luafunc(#lua_func{lsz=Lsz,esz=Esz,pars=_Pars,b=Fis},
	     Args, Stk0, Env0, Cs, St0) ->
    L = make_loc_frame(Lsz),
    {Eref,St1} = make_env_frame(Esz, St0),
    Lvs = [L],
    Stk1 = [Args|Stk0],
    Env1 = [Eref|Env0],
    %% Tag = St0#luerl.tag,
    %% io:fwrite("fc: ~p\n", [{Lvs,Env,St0#luerl.env}]),
    emul(Fis, [], Lvs, Stk1, Env1, Cs, St1).

%% call_erlfunc(ErlFunc, Args, Stack, Env, State) -> {Return,State}.
%%  Here we must save the stacks in state as function may need it.
%%  Note we leave the call frame to the erlang function on the call
%%  stack. It is popped when we return.

call_erlfunc(Func, Args, Stk, Cs0, #luerl{stk=Stk0}=St0) ->
    {Ret,St1} = Func(Args, St0#luerl{stk=Stk,cs=Cs0}),
    [#call_frame{is=Is,cont=Cont,lvs=Lvs,env=Env}|Cs1] = Cs0,
    emul(Is, Cont, Lvs, [Ret|Stk], Env, Cs1, St1#luerl{stk=Stk0,cs=Cs1}).

%% do_block(Instrs, LocalVars, Stack, Env, State,
%%          LocalSize, EnvSize, BlockInstrs) -> <emul>.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

do_block(Is, Cont, Lvs, Stk, Env, Cs, St0, Lsz, Esz, Bis) ->
    L = make_loc_frame(Lsz),
    {Eref,St1} = make_env_frame(Esz, St0),
    emul(Bis, [Is|Cont], [L|Lvs], Stk, [Eref|Env], Cs, St1).

%% do_block_open(Instrs, LocalVars, Stack, Env, State,
%%               LocalSize, EnvSize) -> <emul>.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

do_block_open(Is, Cont, Lvs, Stk, Env, Cs, St0, Lsz, Esz) ->
    L = make_loc_frame(Lsz),
    {Eref,St1} = make_env_frame(Esz, St0),
    emul(Is, Cont, [L|Lvs], Stk, [Eref|Env], Cs, St1).

%% do_block_close(Instrs, LocalVars, Stack, Env, State,
%%                LocalSize, EnvSize) -> <emul>.
%%  Pop the block local variables and environment variables.

do_block_close(Is, Cont, [_|Lvs], Stk, [_|Env], Cs, St) ->
    emul(Is, Cont, Lvs, Stk, Env, Cs, St).

make_env_frame(0, St) -> {not_used,St};
make_env_frame(Esz, St) ->
    luerl_heap:alloc_environment(Esz, St).	%{Eref,St}.

make_loc_frame(0) -> not_used;
make_loc_frame(Lsz) ->
    erlang:make_tuple(Lsz, nil).

%% do_while(Instrs, Cont, LocalVars, Stack, Env, State, WhileEis, WhileBis) ->
%%     <emul>

do_while(Is, Cont, Lvs, Stk, Env, Cs0, St, Eis, Wis) ->
    %% Add the break frame to the call stack.
    Fr = #loop_frame{lvs=Lvs,stk=Stk,env=Env,is=Is,cont=Cont},
    Cs1 = [Fr|Cs0],
    emul(Eis, [[?WHILE_LOOP(Eis, Wis)|Is]|Cont],
	 Lvs, Stk, Env, Cs1, St).

do_while_loop(Is, Cont, Lvs, [Val|Stk], Env, Cs, St, Eis, Wis) ->
    case boolean_value(Val) of
	true ->
	    emul(Wis, [Eis,[?WHILE_LOOP(Eis, Wis)|Is]|Cont],
		 Lvs, Stk, Env, Cs, St);
	false ->
	    emul([?BREAK|Is], Cont, Lvs, Stk, Env, Cs, St)
    end.

%% do_repeat(Instrs, Cont, LocalVars, Stack, Env, State, RepeatInstrs) ->
%%     <emul>
%%  We know that at the end of the repear instructions the test value
%%  is calculated.

do_repeat(Is, Cont, Lvs, Stk, Env, Cs0, St, Ris) ->
    %% Add the break frame to the call stack.
    Fr = #loop_frame{lvs=Lvs,stk=Stk,env=Env,is=Is,cont=Cont},
    Cs1 = [Fr|Cs0],
    emul(Ris, [[?REPEAT_LOOP(Ris)|Is]|Cont], Lvs, Stk, Env, Cs1, St).

do_repeat_loop(Is, Cont, Lvs, [Val|Stk], Env, Cs, St, Ris) ->
    case boolean_value(Val) of
	true ->
	    emul([?BREAK|Is], Cont, Lvs, Stk, Env, Cs, St);
	false ->
	    emul(Ris, [[?REPEAT_LOOP(Ris)|Is]|Cont], Lvs, Stk, Env, Cs, St)
    end.

%% do_and_then(Instrs, Continuation,LocalVars, Stack, Env, State, ThenInstrs) ->
%%     <emul>.
%% do_or_else(Instrs, Continuation,LocalVars, Stack, Env, State, ElseInstrs) ->
%%     <emul>.

do_and_then(Is, Cont, Lvs, [Val|Stk1]=Stk0, Env, Cs, St, Then) ->
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    emul(Then, [Is|Cont], Lvs, Stk1, Env, Cs, St);
	false ->
	    %% Non true value left on stack.
	    emul(Is, Cont, Lvs, Stk0, Env, Cs, St)
    end.

do_or_else(Is, Cont, Lvs, [Val|Stk1]=Stk0, Env, Cs, St, Else) ->
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    %% Non false value left on stack.
	    emul(Is, Cont, Lvs, Stk0, Env, Cs, St);
	false ->
	    emul(Else, [Is|Cont], Lvs, Stk1, Env, Cs, St)
    end.

%% do_if(Instrs, Continuation, LocalVars, Stack, Env, State, TrueInstrs) ->
%%     <emul>.
%%  Test value on stack to choose whether to do True instructions.

do_if_true(Is, Cont, Lvs, [Val|Stk], Env, Cs, St, True) ->
    case boolean_value(Val) of
        true ->
	    emul(True, [Is|Cont], Lvs, Stk, Env, Cs, St);
        false ->
	    emul(Is, Cont, Lvs, Stk, Env, Cs, St)
    end.

%% do_if(Instrs, LocalVars, Stack, Env, State, TrueInstrs, FalseInstrs) ->
%%     <emul>.
%%  Test value on stack to choose either True or False instructions.

do_if(Is, Cont, Lvs0, [Val|Stk0], Env0, Cs, St0, True, False) ->
    case boolean_value(Val) of
	true -> emul(True, [Is|Cont], Lvs0, Stk0, Env0, Cs, St0);
	false -> emul(False, [Is|Cont], Lvs0, Stk0, Env0, Cs, St0)
    end.

%% do_if(Blocks, Else, Lvs, Stk, Env, St) ->
%%     do_if_blocks(Blocks, Else, Lvs, Stk, Env, St).

%% do_if_blocks([{T,B}|Ts], Else, Lvs0, Stk0, Env0, St0) ->
%%     {Lvs1,[Val|Stk1],Env1,St1} = emul(T, Lvs0, Stk0, Env0, St0),
%%     case boolean_value(Val) of
%% 	true -> emul(B, Lvs1, Stk1, Env1, St1);
%% 	false -> do_if_blocks(Ts, Lvs1, Stk1, Env1, St1)
%%     end;
%% do_if_blocks([], Else, Lvs, Stk, Env, St) ->
%%     emul(Else, Lvs, Stk, Env, St).

%% do_if_block([?BLOCK(Lsz, Esz, Bis)], Lvs0, Stk0, Env0, St0, Is) ->
%%     {Lvs1,Stk1,Env1,St1} = do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz),
%%     emul(Is, Lvs1, Stk1, Env1, St1);
%% do_if_block(Bis, Lvs0, Stk0, Env0, St0, Is) ->
%%     {Lvs1,Stk1,Env1,St1} = emul(Bis, Lvs0, Stk0, Env0, St0),
%%     emul(Is, Lvs1, Stk1, Env1, St1).

%% do_numfor(Instrs, LocalVars, Stack, Env, State, Varname, FromInstrs) ->
%%     <emul>

do_numfor(Is, Cont, Lvs, [Step,Limit,Init|Stk], Env, Cs0, St, _, Fis) ->
    %% First check if we have numbers.
    case luerl_lib:args_to_numbers([Init,Limit,Step]) of
	[I,L,S] ->
	    %% Add the break frame to the call stack.
	    Fr = #loop_frame{lvs=Lvs,stk=Stk,env=Env,is=Is,cont=Cont},
	    Cs1 = [Fr|Cs0],
	    do_numfor_loop(Is, Cont, Lvs, Stk, Env, Cs1, St, I, L, S, Fis);
	error ->
	    badarg_error(loop, [Init,Limit,Step], St#luerl{cs=Cs0})
    end.

do_numfor_loop(Is, Cont, Lvs, Stk, Env, Cs, St, N, Limit, Step, Fis) ->
    %% itrace_print("nl: ~p\n", [{N,Stk}]),
    %% Leave the current counter at the top of the stack for code to get.
    if Step > 0, N =< Limit ->			%Keep going
	    emul(Fis, [[?NFOR_LOOP(N+Step, Limit, Step, Fis)|Is]|Cont],
		 Lvs, [N|Stk], Env, Cs, St);
       Step < 0, N >= Limit ->			%Keep going
	    emul(Fis, [[?NFOR_LOOP(N+Step, Limit, Step, Fis)|Is]|Cont],
		 Lvs, [N|Stk], Env, Cs, St);
       true ->					%Done!
	    emul([?BREAK|Is], Cont, Lvs, Stk, Env, Cs, St)
    end.

%% do_genfor(Instrs, LocalVars, Stack, Env, 
%%           CallStack, State, Vars, FromInstrs) -> <emul>
%%  The top of the stack will contain the return values from the explist.

do_genfor(Is, Cont, Lvs, [Val|Stk], Env, Cs0, St, _, Fis) ->
    case Val of					%Sneaky, export Func, Data, Var
	[Func] -> Data = nil, Var = nil;
	[Func,Data] -> Var = nil;
	[Func,Data,Var|_] -> ok;
	Func -> Data = nil, Var = nil
    end,
    %% Add the break frame to the call stack.
    Fr = #loop_frame{lvs=Lvs,stk=Stk,env=Env,is=Is,cont=Cont},
    Cs1 = [Fr|Cs0],
    do_genfor_call(Is, Cont, Lvs, Stk, Env, Cs1, St, Func, Data, Var, Fis).

do_genfor_call(Is, Cont, Lvs, Stk, Env, Cs, St, Func, Data, Val, Fis) ->
    emul([?FCALL,?GFOR_LOOP(Func, Data, Fis)|Is], Cont,
	 Lvs, [[Data,Val],Func|Stk], Env, Cs, St).

do_genfor_loop(Is, Cont, Lvs, [Vals|Stk], Env, Cs, St, Func, Data, Fis) ->
    case boolean_value(Vals) of
	true ->
	    emul(Fis, [[?GFOR_CALL(Func,Data,hd(Vals),Fis)|Is]|Cont],
		 Lvs, [Vals|Stk], Env, Cs, St);
	false ->
	    emul([?BREAK|Is], Cont, Lvs, Stk, Env, Cs, St)
    end.

%% build_tab(FieldCount, Index, Stack, State) -> {TableRef,Stack,State}.
%%  FieldCount is how many Key/Value pairs are on the stack, Index is
%%  the index of the next value in the acc.

build_tab(Fc, I, [Last|Stk0], St0) ->
    Fs0 = build_tab_last(I, Last),
    {Fs1,Stk1} = build_tab_loop(Fc, Stk0, Fs0),
    %% io:fwrite("bt: ~p\n", [{Fc,I,Acc,Fs0,Fs1}]),
    {Tref,St1} = luerl_heap:alloc_table(Fs1, St0),
    {Tref,Stk1,St1}.

build_tab_last(I, [V|Vs]) ->
    [{I,V}|build_tab_last(I+1, Vs)];
build_tab_last(_, []) -> [];
build_tab_last(_, Last) -> error({boom,build_tab_acc,Last}).

build_tab_loop(0, Stk, Fs) -> {Fs,Stk};
build_tab_loop(C, [V,K|Stk], Fs) ->
    build_tab_loop(C-1, Stk, [{K,V}|Fs]).

%% op(Op, Arg, State) -> OpReturn.
%% op(Op, Arg1, Arg2, State) -> OpReturn.
%%
%%  OpReturn = {value,Ret,State} |
%%             {meta,Method,Args,State} |
%%             {error,Error,State}.
%%
%%  The built-in operators. Always return a single value!

op('-', A, St) ->
    numeric_op('-', A, St, <<"__unm">>, fun (N) -> -N end);
op('not', A, St) -> {value,not ?IS_TRUE(A),St};
op('~', A, St) ->
    integer_op('~', A, St, <<"__bnot">>, fun (N) -> bnot(N) end);
op('#', A, St) ->
    length_op('#', A, St);
op(Op, A, St) ->
    {error,{badarg,Op,[A]},St}.

%% Numeric operators.
op('+', A1, A2, St) ->
    numeric_op('+', A1, A2, St, <<"__add">>, fun (N1,N2) -> N1+N2 end);
op('-', A1, A2, St) ->
    numeric_op('-', A1, A2, St, <<"__sub">>, fun (N1,N2) -> N1-N2 end);
op('*', A1, A2, St) ->
    numeric_op('*', A1, A2, St, <<"__mul">>, fun (N1,N2) -> N1*N2 end);
op('/', A1, A2, St) ->
    numeric_op('/', A1, A2, St, <<"__div">>, fun (N1,N2) -> N1/N2 end);
%% The '//' and '%' operators are specially handled to avoid first
%% converting integers to floats and potentially lose precision.
op('//', A1, A2, St) ->
    numeric_op('//', A1, A2, St, <<"__idiv">>,
	       fun (N1,N2) when is_integer(N1), is_integer(N2) ->
		       Idiv = N1 div N2,
		       Irem = N1 rem N2,
		       if Irem =:= 0 -> Idiv;
			  Idiv < 0 -> Idiv - 1;
			  true -> Idiv
		       end;
		   (N1,N2) -> 0.0 + floor(N1/N2) end);
op('%', A1, A2, St) ->
    numeric_op('%', A1, A2, St, <<"__mod">>,
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
op('^', A1, A2, St) ->
    numeric_op('^', A1, A2, St, <<"__pow">>,
	       fun (N1,N2) -> math:pow(N1, N2) end);
%% Bitwise operators.
%% The '>>' is an arithmetic shift as a logical shift implies a word
%% size which we don't have.
op('&', A1, A2, St) ->
    integer_op('&', A1, A2, St, <<"__band">>, fun (N1,N2) -> N1 band N2 end);
op('|', A1, A2, St) ->
    integer_op('|', A1, A2, St, <<"__bor">>, fun (N1,N2) -> N1 bor N2 end);
op('~', A1, A2, St) ->
    integer_op('~', A1, A2, St, <<"__bxor">>, fun (N1,N2) -> N1 bxor N2 end);
op('<<', A1, A2, St) ->
    integer_op('<<', A1, A2, St, <<"__shl">>, fun (N1,N2) -> N1 bsl N2 end);
op('>>', A1, A2, St) ->
    integer_op('>>', A1, A2, St, <<"__shr">>, fun (N1,N2) -> N1 bsr N2 end);
%% Relational operators, getting close.
op('==', A1, A2, St) -> eq_op('==', A1, A2, St);
op('~=', A1, A2, St) -> neq_op('~=', A1, A2, St);
op('<=', A1, A2, St) -> le_op('<=', A1, A2, St);
op('>=', A1, A2, St) -> le_op('>=', A2, A1, St);
op('<', A1, A2, St) -> lt_op('<', A1, A2, St);
op('>', A1, A2, St) -> lt_op('>', A2, A1, St);
%% String operator.
op('..', A1, A2, St) -> concat_op(A1, A2, St);
%% Bad args here.
op(Op, A1, A2, St) ->
    {error,{badarg,Op,[A1,A2]}, St}.

-ifndef(HAS_FLOOR).
%% floor(Number) -> integer().
%%  Floor does not exist before 20 so we need to do it ourselves.

floor(N) when is_integer(N) -> N;
floor(N) when is_float(N) -> round(N - 0.5).
-endif.

%% length_op(Op, Arg, State) -> OpReturn.
%% numeric_op(Op, Arg, State, Event, Raw) -> OpReturn.
%% numeric_op(Op, Arg, Arg, State, Event, Raw) -> OpReturn.
%% integer_op(Op, Arg, State, Event, Raw) -> OpReturn.
%% integer_op(Op, Arg, Arg, State, Event, Raw) -> OpReturn.
%% eq_op(Op, Arg, Arg, State) -> OpReturn.
%% neq_op(Op, Arg, Arg, State) -> OpReturn.
%% lt_op(Op, Arg, Arg, State) -> OpReturn.
%% le_op(Op, Arg, Arg, State) -> OpReturn.
%% concat_op(Arg, Arg, State) -> OpReturn.
%%
%%  OpReturn = {value,Ret,State} |
%%             {meta,Method,Args,State} |
%%             {error,Error,State}.
%%
%%  Together with their metas straight out of the reference
%%  manual. Note that:
%%  - numeric_op string args are always floats
%%  - eq/neq metamethods here must return boolean values and the tests
%%    themselves are type dependent

length_op(_Op, A, St) when is_binary(A) -> {value,byte_size(A),St};
length_op(_Op, A, St) ->
    case luerl_heap:get_metamethod(A, <<"__len">>, St) of
	nil ->
	    if ?IS_TREF(A) ->
		    {value,luerl_lib_table:raw_length(A, St),St};
	       true ->
		    {error,{badarg,'#',[A]}, St}
	    end;
	Meth -> {meta,Meth,[A],St}
    end.

numeric_op(Op, A, St, E, Raw) ->
    case luerl_lib:arg_to_number(A) of
	error -> op_meta(Op, A, E, St);
	N -> {value,Raw(N),St}
    end.

numeric_op(Op, A1, A2, St, E, Raw) ->
    case luerl_lib:args_to_numbers(A1, A2) of
	[N1,N2] ->
	    {value,Raw(N1, N2),St};
	error ->
	    op_meta(Op, A1, A2, E, St)
    end.

integer_op(Op, A, St, E, Raw) ->
    case luerl_lib:arg_to_integer(A) of
	error -> op_meta(Op, A, E, St);
	N -> {value,Raw(N),St}
    end.

integer_op(Op, A1, A2, St, E, Raw) ->
    case luerl_lib:args_to_integers(A1, A2) of
	[N1,N2] -> {value,Raw(N1, N2),St};
	error ->
	    op_meta(Op, A1, A2, E, St)
    end.

eq_op(_Op, A1, A2, St) when A1 == A2 -> {value,true,St};
eq_op(_Op, A1, A2, St)
  when ?IS_TREF(A1), ?IS_TREF(A2) ; ?IS_USDREF(A1), ?IS_USDREF(A2) ->
    case get_eqmetamethod(A1, A2, St) of
	nil -> {value,false,St};
	Meth ->
	    Func = fun (Args, St0) ->
			   {Ret,St1} = functioncall(Meth, Args, St0),
			   {[boolean_value(Ret)],St1}
		   end,
	    {meta,#erl_func{code=Func},[A1,A2],St}
    end;
eq_op(_, _, _, St) -> {value,false,St}.

neq_op(_Op, A1, A2, St) when A1 == A2 -> {value,false,St};
neq_op(_Op, A1, A2, St)
  when ?IS_TREF(A1), ?IS_TREF(A2) ; ?IS_USDREF(A1), ?IS_USDREF(A2) ->
    case get_eqmetamethod(A1, A2, St) of
	nil -> {value,true,St};
	Meth ->
	    Func = fun (Args, St0) ->
			   {Ret,St1} = functioncall(Meth, Args, St0),
			   {[not boolean_value(Ret)],St1}
		   end,
	    {meta,#erl_func{code=Func},[A1,A2],St}
    end;
neq_op(_, _, _, St) -> {value,true,St}.

get_eqmetamethod(A1, A2, St) ->
    %% Must have "same" metamethod here. How do we test?
    case luerl_heap:get_metamethod(A1, <<"__eq">>, St) of
	nil -> nil;
	Meth ->
	    case luerl_heap:get_metamethod(A2, <<"__eq">>, St) of
		Meth -> Meth;			%Must be the same method
		_ -> nil
	    end
    end.

lt_op(_Op, A1, A2, St) when is_number(A1), is_number(A2) -> {value,A1 < A2,St};
lt_op(_Op, A1, A2, St) when is_binary(A1), is_binary(A2) -> {value,A1 < A2,St};
lt_op(Op, A1, A2, St) ->
    op_meta(Op, A1, A2, <<"__lt">>, St).

le_op(_Op, A1, A2, St) when is_number(A1), is_number(A2) -> {value,A1 =< A2,St};
le_op(_Op, A1, A2, St) when is_binary(A1), is_binary(A2) -> {value,A1 =< A2,St};
le_op(Op, A1, A2, St) ->
    %% Must check for first __le then __lt metamethods.
    case luerl_heap:get_metamethod(A1, A2, <<"__le">>, St) of
	nil ->
	    %% Try for not (Op2 < Op1) instead.
	    case luerl_heap:get_metamethod(A1, A2, <<"__lt">>, St) of
		nil ->
		    {error,{badarg,Op,[A1,A2]}, St};
		Meth ->
		    {meta,Meth,[A2,A1],St}
	    end;
	Meth ->
	    {meta,Meth,[A1,A2],St}
    end.

concat_op(A1, A2, St) ->
    case luerl_lib:conv_list([A1,A2], [lua_string,lua_string]) of
	[S1,S2] -> {value,<<S1/binary,S2/binary>>,St};
	error ->
	    op_meta('..', A1, A2, <<"__concat">>, St)
    end.

op_meta(Op, A, E, St) ->
    case luerl_heap:get_metamethod(A, E, St) of
	nil -> {error,{badarg,Op,[A]}, St};
	Meth -> {meta,Meth,[A],St}
    end.

op_meta(Op, A1, A2, E, St) ->
    case luerl_heap:get_metamethod(A1, A2, E, St) of
	nil -> {error,{badarg,Op,[A1,A2]},St};
	Meth -> {meta,Meth,[A1,A2],St}
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

multiple_value(V) when not is_list(V) -> [V].
