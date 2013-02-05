%% Copyright (c) 2013 Robert Virding
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
-export([emul/2,call/2,call/3,chunk/2,chunk/3]).

%% Internal functions which can be useful "outside".
-export([alloc_table/1,alloc_table/2,free_table/2,
	 functioncall/3,get_table_key/3,
	 getmetamethod/3,getmetamethod/4]).

%% Currently unused internal functions, to suppress warnings.
-export([set_global_name/3,set_global_key/3,
	 get_global_name/2,get_global_key/2]).

%% For testing.
-export([pop_vals/3,push_vals/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).

%% -compile(inline).				%For when we are optimising
%% -compile({inline,[is_true_value/1,first_value/1]}).

-define(ITRACE_DO(B), (get(itrace) /= undefined) andalso B).

%% init() -> State.
%% Initialise the basic state.

init() ->
    %% Initialise the general stuff.
    St0 = #luerl{meta=#meta{},locf=false,tag=make_ref()},
    %% Initialise the table handling.
    St1 = St0#luerl{ttab=?MAKE_TABLE(),tfree=[],tnext=0},
    %% Initialise the frame handling.
    St2 = St1#luerl{ftab=array:new(),ffree=[],fnext=0},
    %% Allocate the _G table and initialise the environment
    {_G,St3} = luerl_basic:install(St2),	%Global environment
    St4 = St3#luerl{g=_G},
    %% Set _G variable to point to it.
    St5 = set_global_name('_G', _G, St4),
    %% Add the other standard libraries.
    St6 = alloc_libs([{<<"math">>,luerl_math},
		      {<<"io">>,luerl_io},
		      {<<"os">>,luerl_os},
		      {<<"string">>,luerl_string},
		      {<<"table">>,luerl_table}], St5),
    St6.

alloc_libs(Libs, St) ->
    Fun = fun ({Key,Mod}, St0) ->
		  {T,St1} = Mod:install(St0),
		  set_global_key(Key, T, St1)
	  end,
    lists:foldl(Fun, St, Libs).

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

alloc_frame(Fr, #luerl{ftab=Ft0,ffree=[N|Ns]}=St) ->
    Ft1 = array:set(N, Fr, Ft0),
    {#fref{i=N},St#luerl{ftab=Ft1,ffree=Ns}};
alloc_frame(Fr, #luerl{ftab=Ft0,ffree=[],fnext=N}=St) ->
    Ft1 = array:set(N, Fr, Ft0),
    {#fref{i=N},St#luerl{ftab=Ft1,fnext=N+1}}.

%% alloc_table(State) -> {Tref,State}.
%% alloc_table(InitialTable, State) -> {Tref,State}.
%% free_table(Tref, State) -> State.
%%  The InitialTable is [{Key,Value}], there is no longer any need to
%%  have it as an orddict.

alloc_table(St) -> alloc_table([], St).

alloc_table(Itab, #luerl{ttab=Ts0,tfree=[N|Ns]}=St) ->
    T = init_table(Itab),
    %% io:fwrite("it1: ~p\n", [{N,T}]),
    Ts1 = ?SET_TABLE(N, T, Ts0),
    {#tref{i=N},St#luerl{ttab=Ts1,tfree=Ns}};
alloc_table(Itab, #luerl{ttab=Ts0,tfree=[],tnext=N}=St) ->
    T = init_table(Itab),
    %% io:fwrite("it2: ~p\n", [{N,T}]),
    Ts1 = ?SET_TABLE(N, T, Ts0),
    {#tref{i=N},St#luerl{ttab=Ts1,tnext=N+1}}.

init_table(Itab) ->
    T0 = ttdict:new(),
    A0 = array:new([{default,nil}]),		%Arrays with 'nil' as default
    Init = fun ({_,nil}, {T,A}) -> {T,A};	%Ignore nil values
	       ({K,V}, {T,A}) when is_number(K) ->
		   case ?IS_INTEGER(K, I) of
		       true when I >= 1 -> {T,array:set(I, V, A)};
		       _NegFalse -> {ttdict:store(K, V, T),A}
		   end;
	       ({K,V}, {T,A}) -> {ttdict:store(K, V, T),A}
	   end,
    {T1,A1} = lists:foldl(Init, {T0,A0}, Itab),
    #table{a=A1,t=T1,m=nil}.

free_table(#tref{i=N}, #luerl{ttab=Ts0,tfree=Ns}=St) ->
    %% io:fwrite("ft: ~p\n", [{N,?GET_TABLE(N, Ts0)}]),
    Ts1 = ?DEL_TABLE(N, Ts0),
    St#luerl{ttab=Ts1,tfree=[N|Ns]}.

%% set_table_key(Tref, Key, Value, State) -> State.
%% get_table_key(Tref, Key, State) -> {Val,State}.
%%  Access tables, as opposed to the environment (which are also
%%  tables). Setting a value to 'nil' will clear it from the array but
%%  not from the table; however, we won't add a nil value.
%%  NOTE: WE ALWAYS RETURN A SINGLE VALUE!

set_table_key(#tref{}=Tref, Key, Val, St) when is_number(Key) ->
    case ?IS_INTEGER(Key, I) of
	true when I >= 1 -> set_table_int_key(Tref, Key, I, Val, St);
	_NegFalse -> set_table_key_key(Tref, Key, Val, St)
    end;
set_table_key(#tref{}=Tref, Key, Val, St) ->
    set_table_key_key(Tref, Key, Val, St);
set_table_key(Tab, Key, _, St) ->
    lua_error({illegal_index,Tab,Key}, St).

set_table_key_key(#tref{i=N}, Key, Val, #luerl{ttab=Ts0}=St) ->
    #table{t=Tab0,m=Meta}=T = ?GET_TABLE(N, Ts0),	%Get the table
    case ttdict:find(Key, Tab0) of
	{ok,_} ->			    %Key exists
	    %% Don't delete key for nil here!
	    Tab1 = ttdict:store(Key, Val, Tab0),
	    Ts1 = ?SET_TABLE(N, T#table{t=Tab1}, Ts0),
	    St#luerl{ttab=Ts1};
	error ->				%Key does not exist
	    case getmetamethod_tab(Meta, <<"__newindex">>, Ts0) of
		nil ->
		    %% Only add non-nil value.
		    Tab1 = if Val =:= nil -> Tab0;
			      true -> ttdict:store(Key, Val, Tab0)
			   end,
		    Ts1 = ?SET_TABLE(N, T#table{t=Tab1}, Ts0),
		    St#luerl{ttab=Ts1};
		Meth when element(1, Meth) =:= function ->
		    functioncall(Meth, [Key,Val], St);
		Meth -> set_table_key(Meth, Key, Val, St)
	    end
    end.

set_table_int_key(#tref{i=N}, Key, I, Val, #luerl{ttab=Ts0}=St) ->
    #table{a=Arr0,m=Meta}=T = ?GET_TABLE(N, Ts0),	%Get the table
    case array:get(I, Arr0) of
	nil ->					%Key does not exist
	    case getmetamethod_tab(Meta, <<"__newindex">>, Ts0) of
		nil ->
		    %% Only add non-nil value, slightly faster (?)
		    Arr1 = if Val =:= nil -> Arr0;
			      true -> array:set(I, Val, Arr0)
			   end,
		    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
		    St#luerl{ttab=Ts1};
		Meth when element(1, Meth) =:= function ->
		    functioncall(Meth, [Key,Val], St);
		Meth -> set_table_key(Meth, Key, Val, St)
	    end;
	_ ->					%Key exists
	    %% Can do this as 'nil' is default value of array.
	    Arr1 = array:set(I, Val, Arr0),
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
	    St#luerl{ttab=Ts1}
    end.

get_table_key(#tref{}=Tref, Key, St) when is_number(Key) ->
    case ?IS_INTEGER(Key, I) of
	true when I >= 1 -> get_table_int_key(Tref, Key, I, St);
	_NegFalse -> get_table_key_key(Tref, Key, St)
    end;
get_table_key(#tref{}=Tref, Key, St) ->
    get_table_key_key(Tref, Key, St);
get_table_key(Tab, Key, St) ->			%Just find the metamethod
    case getmetamethod(Tab, <<"__index">>, St) of
	nil -> lua_error({illegal_index,Tab,Key}, St);
	Meth when element(1, Meth) =:= function ->
	    {Vs,St1} = functioncall(Meth, [Tab,Key], St),
	    {first_value(Vs),St1};
	Meth ->					%Recurse down the metatable
	    get_table_key(Meth, Key, St)
    end.

get_table_key_key(#tref{i=N}=T, Key, #luerl{ttab=Ts}=St) ->
    #table{t=Tab,m=Meta} = ?GET_TABLE(N, Ts),	%Get the table.
    case ttdict:find(Key, Tab) of
	{ok,Val} -> {Val,St};
	error ->
	    %% Key not present so try metamethod
	    get_table_metamethod(T, Meta, Key, Ts, St)
    end.

get_table_int_key(#tref{i=N}=T, Key, I, #luerl{ttab=Ts}=St) ->
    #table{a=A,m=Meta} = ?GET_TABLE(N, Ts),	%Get the table.
    case array:get(I, A) of
	nil ->
	    %% Key not present so try metamethod
	    get_table_metamethod(T, Meta, Key, Ts, St);
	Val -> {Val,St}
    end.

get_table_metamethod(T, Meta, Key, Ts, St) ->
    case getmetamethod_tab(Meta, <<"__index">>, Ts) of
	nil -> {nil,St};
	Meth when element(1, Meth) =:= function ->
	    {Vs,St1} = functioncall(Meth, [T,Key], St),
	    {first_value(Vs),St1};
	Meth ->				%Recurse down the metatable
	    get_table_key(Meth, Key, St)
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

%% set_env_var(Depth, Index, Val, Env, State) -> State.
%% get_env_var(Depth, Index, Env, State) -> Val.
%%  We must have the state as the environments are global in the
%%  state.

set_env_var(D, I, Val, Env, #luerl{ftab=Ft0}=St) ->
    Ft1 = set_env_var_1(D, I, Val, Env, Ft0),
    St#luerl{ftab=Ft1}.

set_env_var_1(1, I, V, [#fref{i=N}|_], Ft) ->
    F = setelement(I, array:get(N, Ft), V),
    array:set(N, F, Ft);
set_env_var_1(2, I, V, [_,#fref{i=N}|_], Ft) ->
    F = setelement(I, array:get(N, Ft), V),
    array:set(N, F, Ft);
set_env_var_1(D, I, V, Fps, Ft) ->
    #fref{i=N} = lists:nth(D, Fps),
    F = setelement(I, array:get(N, Ft), V),
    array:set(N, F, Ft).

get_env_var(D, I, Env, #luerl{ftab=Ft}) ->
    get_env_var_1(D, I, Env, Ft).

get_env_var_1(1, I, [#fref{i=N}|_], Ft) ->
    element(I, array:get(N, Ft));
get_env_var_1(2, I, [_,#fref{i=N}|_], Ft) ->
    element(I, array:get(N, Ft));
get_env_var_1(D, I, Fps, Ft) ->
    #fref{i=N} = lists:nth(D, Fps),
    element(I, array:get(N, Ft)).

%% set_global_var(Var, Val, State) -> State.
%% get_global_var(Var, State) -> {Val,State}.
%%  _G a normal table with metatable so we must use the table
%%  functions.  However we can optimise a bit as we KNOW that _G is a
%%  table and the var is always a normal non-integer key.

set_global_var(Var, Val, #luerl{g=G}=St) ->
    set_table_key_key(G, Var, Val, St).

get_global_var(Var, #luerl{g=G}=St) ->
    get_table_key_key(G, Var, St).

%% chunk(Chunk, State) -> {Return,State}.
%% chunk(Chunk, Args, State) -> {Return,State}.

chunk(Chunk, St) -> chunk(Chunk, [], St).
chunk(Chunk, Args, St) -> call(Chunk, Args, St).

%% call(Chunk, State) -> {Return,State}.
%% call(Chunk, Args, State) -> {Return,State}.

call(Chunk, St) -> call(Chunk, [], St).

call(#code{code=C0}, Args, St0) ->
    {R,_,_,_,St1} = emul(C0, St0),		%R is the accumulator
    itrace_print("e: ~p\n", [R]),
    functioncall(R, Args, St1);
call(#function{}=Func, Args, St0) ->		%Already defined
    {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1};
call({function,_}=Func, Args, St0) ->		%Internal erlang function
    {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1}.

itrace_do(Fun) ->
    ?ITRACE_DO(Fun()).

itrace_print(Format, Args) ->
    ?ITRACE_DO(io:fwrite(Format, Args)).

%% exp(_, _) ->
%%     error(boom).

-record(call_frame, {acc,lvs,env}).		%Save these for the GC

%% emul(Instrs, State).
%% emul(Instrs, Accumulator, LocalVariables, Stack, Env, State).

emul(Is, St) ->
    emul(Is, nil, {}, [], [], St).

emul([I|_]=Is, Acc, Lvs, Stk, Env, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("ei: ~p\n", [{I,Acc,Lvs,Env}]),
		   io:fwrite("--> ", []),
		   stack_print(Stk)
	       end),
    emul_1(Is, Acc, Lvs, Stk, Env, St);
emul([], Acc, Lvs, Stk, Env, St) ->
    itrace_print("el: ~p\n", [{Acc,Lvs,Env}]),
    emul_1([], Acc, Lvs, Stk, Env, St).

stack_print([#call_frame{}|_]) -> io:fwrite(" ...\n");
stack_print([E|St]) ->
    io:fwrite(" ~p", [E]),
    stack_print(St);
stack_print([]) -> io:nl().

%% Expression instructions.
emul_1([?LOAD_LIT(L)|Is], _, Var, Stk, Env, St) ->
    emul(Is, L, Var, Stk, Env, St);
emul_1([?LOAD_LVAR(D, I)|Is], _, Lvs, Stk, Env, St) ->
    Acc = get_local_var(D, I, Lvs),
    emul(Is, Acc, Lvs, Stk, Env, St);
emul_1([?LOAD_EVAR(D, I)|Is], _, Lvs, Stk, Env, St) ->
    Acc = get_env_var(D, I, Env, St),
    emul(Is, Acc, Lvs, Stk, Env, St);
emul_1([?LOAD_GVAR(K)|Is], _, Var, Stk, Env, St0) ->
    {Acc,St1} = get_global_var(K, St0),
    emul(Is, Acc, Var, Stk, Env, St1);

emul_1([?STORE_LVAR(D, I)|Is], Acc, Lvs0, Stk, Env, St) ->
    Lvs1 = set_local_var(D, I, Acc, Lvs0),
    emul(Is, Acc, Lvs1, Stk, Env, St);
emul_1([?STORE_EVAR(D, I)|Is], Acc, Lvs, Stk, Env, St0) ->
    St1 = set_env_var(D, I, Acc, Env, St0),
    emul(Is, Acc, Lvs, Stk, Env, St1);
emul_1([?STORE_GVAR(K)|Is], Acc, Lvs, Stk, Env, St0) ->
    St1 = set_global_var(K, Acc, St0),
    emul(Is, Acc, Lvs, Stk, Env, St1);

emul_1([?GET_KEY|Is], Acc, Var, [Tab|Stk], Env, St0) ->
    {Val,St1} = get_table_key(Tab, Acc, St0),
    emul(Is, Val, Var, Stk, Env, St1);
emul_1([?GET_LIT_KEY(S)|Is], Acc, Var, Stk, Env, St0) ->
    %% [?PUSH,?LOAD_LIT(S),?KEY]
    {Val,St1} = get_table_key(Acc, S, St0),
    emul(Is, Val, Var, Stk, Env, St1);
emul_1([?SET_KEY|Is], Acc, Var, [Tab,Val|Stk], Env, St0) ->
    St1 = set_table_key(Tab, Acc, Val, St0),
    emul_1(Is, Acc, Var, Stk, Env, St1);
emul_1([?SET_LIT_KEY(K)|Is], Acc, Var, [Val|Stk], Env, St0) ->
    %% [?PUSH,?LOAD_LIT(K),?SET_KEY]
    St1 = set_table_key(Acc, K, Val, St0),
    emul_1(Is, Acc, Var, Stk, Env, St1);

emul_1([?SINGLE|Is], Acc, Var, Stk, Env, St) ->
    emul(Is, first_value(Acc), Var, Stk, Env, St);
emul_1([?BUILD_TAB(Fc, I)|Is], Acc, Var, Stk0, Env, St0) ->
    {Tab,Stk1,St1} = build_tab(Fc, I, Acc, Stk0, St0),
    emul(Is, Tab, Var, Stk1, Env, St1);
emul_1([?CALL(Ac)|Is], Acc, Var, Stk, Env, St) ->
    do_call(Is, Acc, Var, Stk, Env, St, Ac);
emul_1([?TAIL_CALL(Ac)|Is], Acc, Var, Stk, Env, St) ->
    do_tail_call(Is, Acc, Var, Stk, Env, St, Ac);
emul_1([?OP(Op,Ac)|Is], Acc, Var, Stk, Env, St) ->
    do_op(Is, Acc, Var, Stk, Env, St, Op, Ac);
emul_1([?FDEF(Lsz, Esz, Pars, Fis)|Is], _, Var, Stk, Env, St) ->
    Func = do_fdef(Lsz, Esz, Pars, Fis, Env, St),
    emul(Is, Func, Var, Stk, Env, St);
%% Control instructions.
emul_1([?BLOCK(Lsz, Esz, Bis)|Is], Acc, Var, Stk, Env, St) ->
    do_block(Is, Acc, Var, Stk, Env, St, Lsz, Esz, Bis);
emul_1([?WHILE(Eis, Wis)|Is], Acc, Var, Stk, Env, St) ->
    do_while(Is, Acc, Var, Stk, Env, St, Eis, Wis);
emul_1([?REPEAT(Ris)|Is], Acc, Var, Stk, Env, St) ->
    do_repeat(Is, Acc, Var, Stk, Env, St, Ris);
emul_1([?IF_TRUE(T)|Is], Acc, Var, Stk, Env, St0) ->
    case is_true_value(Acc) of
	true ->
	    {Acc1,Var1,Stk1,Env1,St1} =
		emul(T, Acc, Var, Stk, Env, St0),
	    emul(Is, Acc1, Var1, Stk1, Env1, St1);
	false ->
	    emul(Is, Acc, Var, Stk, Env, St0)
    end;
emul_1([?IF_FALSE(T)|Is], Acc, Var, Stk, Env, St0) ->
    case is_true_value(Acc) of
	true ->
	    emul(Is, Acc, Var, Stk, Env, St0);
	false ->
	    {Acc1,Var1,Stk1,Env1,St1} =
		emul(T, Acc, Var, Stk, Env, St0),
	    emul(Is, Acc1, Var1, Stk1, Env1, St1)
    end;
emul_1([?IF(True, False)|Is], Acc, Var, Stk, Env, St) ->
    do_if(Is, Acc, Var, Stk, Env, St, True, False);
emul_1([?NFOR(V, Fis)|Is], Acc, Var, Stk, Env, St) ->
    do_numfor(Is, Acc, Var, Stk, Env, St, V, Fis);
emul_1([?GFOR(Vs, Fis)|Is], Acc, Var, Stk, Env, St) ->
    do_genfor(Is, Acc, Var, Stk, Env, St, Vs, Fis);
emul_1([?BREAK|_], _, Lvs, Stk, Env, St) ->
    throw({break,St#luerl.tag,Lvs,Stk,Env,St});
emul_1([?RETURN(0)|_], _, _, _, _, St) ->
    throw({return,St#luerl.tag,[],St});
emul_1([?RETURN(Ac)|_], Acc, _, Stk, _, St) ->
    {Ret,_} = pop_vals(Ac-1, Stk, Acc),
    throw({return,St#luerl.tag,Ret,St});
%% Stack instructions, mainly to/from accumulator.
emul_1([?PUSH|Is], Acc, Var, Stk, Env, St) ->
    emul(Is, Acc, Var, [Acc|Stk], Env, St);
emul_1([?POP|Is], _, Var, [Acc|Stk], Env, St) ->
    emul(Is, Acc, Var, Stk, Env, St);
emul_1([?DROP|Is], Acc, [_|Stk], Var, Env, St) ->
    emul(Is, Acc, Var, Stk, Env, St);
emul_1([?SWAP|Is], Acc, Var, [S|Stk], Env, St) -> %Swap acc and top of stack
    emul(Is, S, Var, [Acc|Stk], Env, St);
emul_1([?PUSH_VALS(Vc)|Is], Acc, Var, Stk0, Env, St) ->
    {Rest,Stk1} = push_vals(Vc, Stk0, Acc),
    emul(Is, Rest, Var, Stk1, Env, St);
emul_1([?POP_VALS(Vc)|Is], Acc, Var, Stk0, Env, St) ->
    {Vals,Stk1} = pop_vals(Vc, Stk0, Acc),
    emul(Is, Vals, Var, Stk1, Env, St);
%% Combined load/push instructions.
%% These also put the value into acc the same as ?LOAD,?PUSH.
emul_1([?PUSH_LIT(L)|Is], _, Var, Stk, Env, St) ->
    emul(Is, L, Var, [L|Stk], Env, St);
emul_1([?PUSH_LVAR(D, I)|Is], _, Var, Stk, Env, St) ->
    Acc = get_local_var(D, I, Var),
    emul(Is, Acc, Var, [Acc|Stk], Env, St);
emul_1([?PUSH_EVAR(D, I)|Is], _, Lvs, Stk, Env, St) ->
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Acc = get_env_var(D, I, Env, St),
    emul(Is, Acc, Lvs, [Acc|Stk], Env, St);
emul_1([?PUSH_GVAR(K)|Is], _, Var, Stk, Env, St0) ->
    {Acc,St1} = get_global_var(K, St0),
    emul(Is, Acc, Var, [Acc|Stk], Env, St1);
emul_1([], Acc, Var, Stk, Env, St) ->
    {Acc,Var,Stk,Env,St}.

%% pop_vals(Count, Stack, ValTail) -> {ValList,Stack}.
%%  Pop Count values off the stack and push onto the argument list.
%%  First argument is deepest. Always generates list.

pop_vals(C, Stk, Vt) when is_list(Vt) ->	%List tail
    pop_vals_1(C, Stk, Vt);
pop_vals(C, Stk, V) ->				%Non-list tail
    pop_vals_1(C, Stk, [V]).

pop_vals_1(0, Stk, Vs) -> {Vs,Stk};
pop_vals_1(Ac, [A|Stk], Vs) ->
    pop_vals_1(Ac-1, Stk, [A|Vs]).

%% push_vals(Count, Stack, ValList) -> {LastVal,Stack}.
%%  Push Count values from value list onto the stack. Fill with 'nil'
%%  if not enough values. Return last value (for acc) and drop extra
%%  values.

push_vals(0, Stk, []) -> {nil,Stk};
push_vals(0, Stk, [V|_]) -> {V,Stk};
push_vals(0, Stk, V) -> {V,Stk};
push_vals(C, Stk, [V|Vs]) ->
    push_vals(C-1, [V|Stk], Vs);
push_vals(C, Stk, []) ->
    push_vals(C-1, [nil|Stk], nil);
push_vals(C, Stk, Val) ->			%Non-list value
    push_vals(C-1, [Val|Stk], nil).

pop_args(0, Stk, _) -> {[],Stk};
pop_args(Ac, Stk, Tail) -> pop_vals(Ac, Stk, Tail).

%% do_block(Instrs, Acc, Vars, Stack, Env, State,
%%          LocalSize, EnvSize, BlockInstrs) -> ReturnFromEmul.
%%  Local vars may have been updated so must continue with returned
%%  version. There should be no changes in the stack and env.

do_block(Is, Acc0, Lvs0, Stk, Env, St0, 0, 0, Bis) ->
    %% No variables at all.
    {Acc1,Lvs1,_,_,St1} = emul(Bis, Acc0, Lvs0, Stk, Env, St0),
    emul(Is, Acc1, Lvs1, Stk, Env, St1);
do_block(Is, Acc0, Lvs0, Stk, Env, St0, 0, Esz, Bis) ->
    %% No local variables, only env variables.
    E = erlang:make_tuple(Esz, nil),
    {Fref,St1} = alloc_frame(E, St0),
    {Acc1,Lvs1,_,_,St2} = emul(Bis, Acc0, Lvs0, Stk, [Fref|Env], St1),
    emul(Is, Acc1, Lvs1, Stk, Env, St2);
do_block(Is, Acc0, Lvs0, Stk, Env, St0, Lsz, 0, Bis) ->
    %% No env variables, only local variables.
    L = erlang:make_tuple(Lsz, nil),
    {Acc1,[_|Lvs1],_,_,St1} = emul(Bis, Acc0, [L|Lvs0], Stk, Env, St0),
    emul(Is, Acc1, Lvs1, Stk, Env, St1);
do_block(Is, Acc0, Lvs0, Stk, Env, St0, Lsz, Esz, Bis) ->
    %% Both local and env variables.
    L = erlang:make_tuple(Lsz, nil),
    E = erlang:make_tuple(Esz, nil),
    {Fref,St1} = alloc_frame(E, St0),
    {Acc1,[_|Lvs1],_,_,St2} = emul(Bis, Acc0, [L|Lvs0], Stk, [Fref|Env], St1),
    emul(Is, Acc1, Lvs1, Stk, Env, St2).

do_op(Is, Acc, Lvs, Stk0, Env, #luerl{stk=OldStk}=St0, Op, Ac) ->
    {Args,Stk1} = pop_vals(Ac-1, Stk0, Acc),
    %% io:fwrite("op: ~p\n", [{Op,Args}]),
    %% Fr = #call_frame{acc=Acc,lvs=Lvs,stk=Stk1,env=Env},
    {Res,St1}= do_op(Op, Args, St0#luerl{stk=Stk1}),
    emul(Is, Res, Lvs, Stk1, Env, St1#luerl{stk=OldStk}).

do_op(Op, [A], St) -> op(Op, A, St);
do_op(Op, [A1,A2], St) -> op(Op, A1, A2, St).

%% do_fdef(LocalSize, EnvSize, Pars, Instrs, Env, State) -> Function.

do_fdef(Lsz, Esz, Pars, Is, Env, _) ->
    #function{lsz=Lsz,esz=Esz,pars=Pars,env=Env,b=Is}.

%% do_call(Instrs, Acc, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_call(Is, Acc, Lvs, Stk, Env, St, 0) ->
    %% The function is in the acc.
    functioncall(Is, Acc, Lvs, Stk, Env, St, Acc, []);
do_call(Is, Acc, Lvs, Stk0, Env, St, Ac) ->
    {Args,Stk1} = pop_vals(Ac-1, Stk0, Acc),	%Pop arguments, last is in acc
    [Func|Stk2] = Stk1,				%Get function
    functioncall(Is, Acc, Lvs, Stk2, Env, St, Func, Args).

%% functioncall(Function, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

functioncall(Func, Args, #luerl{stk=Stk}=St0) ->
    {Ret,St1} = functioncall(Func, Args, Stk, St0),
    {Ret,St1}.

%% functioncall(Instrs, Acc, Var, Stk, Env, State, Func, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

functioncall(Is, Acc, Lvs, Stk0, Env, St0, Func, Args) ->
    Fr = #call_frame{acc=Acc,lvs=Lvs,env=Env},
    Stk1 = [Fr|Stk0],
    {Ret,St1} = functioncall(Func, Args, Stk1, St0),
    emul(Is, Ret, Lvs, Stk0, Env, St1).

functioncall(#function{lsz=0,esz=0,env=Env,b=Fis}, _, Stk, St0) ->
    %% No variables at all.
    functioncall(Fis, [], Stk, Env, St0);
functioncall(#function{lsz=0,esz=Esz,pars=Pars,env=Env,b=Fis},
	     Args, Stk, St0) ->
    %% No local variables, only env variables.
    E0 = erlang:make_tuple(Esz, nil),
    E1 = assign_env_pars(Pars, Args, E0),
    {Fref,St1} = alloc_frame(E1, St0),
    {Ret,St2} = functioncall(Fis, [], Stk, [Fref|Env], St1),
    {Ret,St2};
functioncall(#function{lsz=Lsz,esz=0,pars=Pars,env=Env,b=Fis},
	     Args, Stk, St0) ->
    %% No env variables, only local variables.
    L0 = erlang:make_tuple(Lsz, nil),
    L1 = assign_local_pars(Pars, Args, L0),
    {Ret,St1} = functioncall(Fis, [L1], Stk, Env, St0),
    {Ret,St1};
functioncall(#function{lsz=Lsz,esz=Esz,pars=Pars,env=Env,b=Fis},
	     Args, Stk, St0) ->
    L0 = erlang:make_tuple(Lsz, nil),
    E0 = erlang:make_tuple(Esz, nil),
    {L1,E1} = assign_pars(Pars, Args, L0, E0),
    {Fref,St1} = alloc_frame(E1, St0),
    {Ret,St2} = functioncall(Fis, [L1], Stk, [Fref|Env], St1),
    {Ret,St2};
functioncall({function,Func}, Args, Stk, #luerl{stk=Stk0}=St0) ->
    %% Here we must save the stack in state as function may need it.
    {Ret,St1} = Func(Args, St0#luerl{stk=Stk}),
    {Ret,St1#luerl{stk=Stk0}};			%Replace it
functioncall(Func, Args, Stk, St) ->
    case getmetamethod(Func, <<"__call">>, St) of
	nil -> badarg_error(Func, Args, St);
	Meta -> functioncall(Meta, Args, Stk, St)
    end.


functioncall(Fis, Lvs, Stk, Env, St0) ->
    Tag = St0#luerl.tag,
    %% Must use different St names else they become 'unsafe'.
    %% io:fwrite("fc: ~p\n", [{Lvs,Env,St0#luerl.env}]),
    try
	{_,_,_,_,Sta} = emul(Fis, nil, Lvs, Stk, Env, St0),
	%%io:fwrite("fr: ~p\n", [{Tag,[]}]),
	{[],Sta}				%No return, no arguments
    catch
	throw:{return,Tag,Ret,Stb} ->
	    %%io:fwrite("fr: ~p\n", [{Tag,Ret,Stb#luerl.env}]),
	    {Ret,Stb};
	throw:{break,Tag,_,_,_,St} ->
	    lua_error({illegal_op,break}, St)
    end.

assign_local_pars([V|Vs], [A|As], Var) ->
    assign_local_pars(Vs, As, setelement(V, Var, A));
assign_local_pars([_|Vs], [], Var) ->
    assign_local_pars(Vs, [], Var);		%Var default is nil
assign_local_pars([], _, Var) -> Var;		%No vararg, drop remain args
assign_local_pars(V, As, Var) ->		%This is a vararg!
    setelement(V, Var, As).

assign_env_pars([V|Vs], [A|As], Var) ->
    assign_env_pars(Vs, As, setelement(-V, Var, A));
assign_env_pars([_|Vs], [], Var) ->
    assign_env_pars(Vs, [], Var);		%Var default is nil
assign_env_pars([], _, Var) -> Var;		%No vararg, drop remain args
assign_env_pars(V, As, Var) ->			%This is a vararg!
    setelement(-V, Var, As).

assign_pars([V|Vs], [A|As], L, E) when V > 0 ->
    assign_pars(Vs, As, setelement(V, L, A), E);
assign_pars([V|Vs], [A|As], L, E) ->		%V < 0
    assign_pars(Vs, As, L, setelement(-V, E, A));
assign_pars([_|Vs], [], L, E) ->
    assign_pars(Vs, [], L, E);		  %Var default is nil
assign_pars([], _, L, E) -> {L,E};		%No vararg, drop remain args
assign_pars(V, As, L, E) when V > 0 ->		%This is a vararg!
    {setelement(V, L, As),E};
assign_pars(V, As, L, E) ->			%This is a vararg!
    {L,setelement(-V, E, As)}.

%% do_tail_call(Instrs, Acc, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_tail_call(_Is, Acc, _Var, Stk, _Env, _St, 0) ->
    error({boom,Acc,[],Stk});
do_tail_call(_Is, Acc, _Var, Stk0, _Env, _St, Ac) ->
    {Args,Stk1} = pop_vals(Ac-1, Stk0, Acc),	%Pop arguments, last is in acc
    [Func|Stk2] = Stk1,				%Get function
    error({boom,Func,Args,Stk2}).

%% do_repeat(Instrs, Acc, LocalVars, Stack, Env, State, RepeatInstrs) -> <emul>

do_repeat(Is, Acc, Lvs, Stk, Env, St, Ris) ->
    Do = fun (S) ->
		 repeat_loop(Ris, Acc, Lvs, Stk, Env, S)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

repeat_loop(Ris, Acc0, Lvs0, Stk0, Env0, St0) ->
    {Acc1,Lvs1,Stk1,Env1,St1} =
	emul(Ris, Acc0, Lvs0, Stk0, Env0, St0),
    case is_true_value(Acc1) of
	true -> {Lvs1,St1};
	false -> repeat_loop(Ris, Acc1, Lvs1, Stk1, Env1, St1)
    end.

%% do_while(Instrs, Acc, LocalVars, Stack, Env, State, WhileEis, WhileBis) ->
%%     <emul>

do_while(Is, Acc, Lvs, Stk, Env, St, Eis, Wis) ->
    Do = fun (S) ->
		 while_loop(Eis, Acc, Lvs, Stk, Env, S, Wis)
	 end,
    loop_block(Is, Lvs, Stk, Env, St, Do).

while_loop(Eis, Acc0, Lvs0, Stk0, Env0, St0, Wis) ->
    {Acc1,Lvs1,Stk1,Env1,St1} =
	emul(Eis, Acc0, Lvs0, Stk0, Env0, St0),
    case is_true_value(Acc1) of
	true ->
	    {Acc2,Lvs2,Stk2,Env2,St2} =
		emul(Wis, Acc1, Lvs1, Stk1, Env1, St1),
	    while_loop(Eis, Acc2, Lvs2, Stk2, Env2, St2, Wis);
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
    emul(Is, nil, Lvs3, Stk, Env, St1).

do_if(Is, Acc, Lvs, Stk, Env, St, True, False) ->
    case is_true_value(Acc) of
	true -> do_if_block(True, Acc, Lvs, Stk, Env, St, Is);
	false -> do_if_block(False, Acc, Lvs, Stk, Env, St, Is)
    end.

do_if_block([?BLOCK(Bis, Loc, Sz)], Acc, Lvs, Stk, Env, St, Is) ->
    do_block(Is, Acc, Lvs, Stk, Env, St, Bis, Loc, Sz);
do_if_block(Bis, Acc0, Lvs, Stk, Env, St0, Is) ->
    {Acc1,_,_,_,St1} =
	emul(Bis, Acc0, Lvs, Stk, Env, St0),
    emul(Is, Acc1, Lvs, Stk, Env, St1).

%% do_numfor(Instrs, Acc, Var, Stack, Env, State, Varname, FromInstrs) -> <emul>

do_numfor(Is, Step, Lvs, [Limit,Init|Stk], Env, St, _, Fis) ->
    %% First check if we have numbers.
    case luerl_lib:tonumbers([Init,Limit,Step]) of
	[I,L,S] ->
	    Do = fun (St) ->
			 numfor_loop(I, L, S, Fis, Lvs, Stk, Env, St)
		 end,
	    loop_block(Is, Lvs, Stk, Env, St, Do);
	nil -> badarg_error(loop, [Init,Limit,Step], St)
    end.

numfor_loop(N, Limit, Step, Fis, Lvs0, Stk0, Env0, St0) ->
    %% Leave the counter in the Acc for code to get.
    if Step > 0.0, N =< Limit ->		%Keep going
	    {_,Lvs1,Stk1,Env1,St1} =
		emul(Fis, N, Lvs0, Stk0, Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       Step < 0.0, N >= Limit ->		%Keep going
	    {_,Lvs1,Stk1,Env1,St1} =
		emul(Fis, N, Lvs0, Stk0, Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       true -> {Lvs0,St0}				%Done!
    end.

numfor_loop_up(N, Limit, Step, Fis, Lvs0, Stk0, Env0, St0) ->
    %% Leave the counter in the Acc for code to get.
    if N =< Limit ->				%Keep going
	    {_,Lvs1,Stk1,Env1,St1} = emul(Fis, N, Lvs0, Stk0, Env0, St0),
	    numfor_loop_up(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       true -> {Lvs0,St0}			%Done!
    end.

numfor_loop_down(N, Limit, Step, Fis, Lvs0, Stk0, Env0, St0) ->
    %% Leave the counter in the Acc for code to get.
    if N >= Limit ->				%Keep going
	    {_,Lvs1,Stk1,Env1,St1} = emul(Fis, N, Lvs0, Stk0, Env0, St0),
	    numfor_loop_down(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       true -> {Lvs0,St0}			%Done!
    end.

%% do_genfor(Instrs, Acc, Var, Stack, Env, State, Vars, FromInstrs) -> <emul>

do_genfor(Is, Acc, Var, Stk, Env, St, _, Fis) ->
    case Acc of					%Export F, T, V
	[F] -> T = nil, V = nil;
	[F,T] -> V = nil;
	[F,T,V|_] -> ok;
	F -> T = nil, V = nil
    end,
    Do = fun (St) ->
		 genfor_loop(F, T, V, Fis, Var, Stk, Env, St)
	 end,
    loop_block(Is, Var, Stk, Env, St, Do).

genfor_loop(Func, Tab, Val, Fis, Lvs0, Stk, Env, St0) ->
    {Vals,St1} = functioncall(Func, [Tab,Val], Stk, St0),
    case is_true_value(Vals) of
	true ->
	    {_,Lvs1,_,_,St2} =
		emul(Fis, Vals, Lvs0, Stk, Env, St1),
	    genfor_loop(Func, Tab, hd(Vals), Fis, Lvs1, Stk, Env, St2);
	false -> {Lvs0,St1}
    end.

%% getmetamethod(Object1, Object2, Event, State) -> Metod | nil.
%% getmetamethod(Object, Event, State) -> Method | nil.
%% Get the metamethod for object(s).

getmetamethod(O1, O2, E, St) ->
    case getmetamethod(O1, E, St) of
	nil -> getmetamethod(O2, E, St);
	M -> M
    end.

getmetamethod(#tref{i=N}, E, #luerl{ttab=Ts}) ->
    #table{m=Meta} = ?GET_TABLE(N, Ts),
    getmetamethod_tab(Meta, E, Ts);
getmetamethod(#userdata{}, E, #luerl{ttab=Ts,meta=Meta}) ->
    getmetamethod_tab(Meta#meta.userdata, E, Ts);
getmetamethod(S, E, #luerl{ttab=Ts,meta=Meta}) when is_binary(S) ->
    getmetamethod_tab(Meta#meta.string, E, Ts);
getmetamethod(N, E, #luerl{ttab=Ts,meta=Meta}) when is_number(N) ->
    getmetamethod_tab(Meta#meta.number, E, Ts);
getmetamethod(_, _, _) -> nil.			%Other types have no metatables

getmetamethod_tab(#tref{i=M}, E, Ts) ->
    #table{t=Mtab} = ?GET_TABLE(M, Ts),
    case ttdict:find(E, Mtab) of
	{ok,Mm} -> Mm;
	error -> nil
    end;
getmetamethod_tab(_, _, _) -> nil.		%Other types have no metatables

%% build_table(FieldCount, Index, Acc, Stack, State) -> {TableRef,Stack,State}.
%%  FieldCount is how many Key/Value pairs are on the stack, Index is
%%  the index of the next value in the acc.

build_tab(Fc, I, Acc, Stk0, St0) ->
    Fs0 = build_tab_acc(I, Acc),
    {Fs1,Stk1} = build_tab_loop(Fc, Stk0, Fs0),
    %% io:fwrite("bt: ~p\n", [{Fc,I,Acc,Fs0,Fs1}]),
    {Tref,St1} = alloc_table(Fs1, St0),
    {Tref,Stk1,St1}.

build_tab_acc(I, [V|Vs]) ->
    [{I,V}|build_tab_acc(I+1.0, Vs)];
build_tab_acc(_, []) -> [];
build_tab_acc(_, nil) -> [];			%Drop final nil
build_tab_acc(I, V) -> [{I,V}].			%Single value

build_tab_loop(0, Stk, Fs) -> {Fs,Stk};
build_tab_loop(C, [V,K|Stk], Fs) ->
    build_tab_loop(C-1, Stk, [{K,V}|Fs]).

%% op(Op, Arg, State) -> {[Ret],State}.
%% op(Op, Arg1, Arg2, State) -> {[Ret],State}.
%% The built-in operators.

op('-', A, St) ->
    numeric_op('-', A, <<"__unm">>, fun (N) -> -N end, St);
op('not', A, St) -> {[not ?IS_TRUE(A)],St};
%% op('not', false, St) -> {[true],St};
%% op('not', nil, St) -> {[true],St};
%% op('not', _, St) -> {[false],St};		%Everything else is false
op('#', B, St) when is_binary(B) -> {[float(byte_size(B))],St};
op('#', #tref{}=T, St) ->
    luerl_table:length(T, St);
op(Op, A, St) -> badarg_error(Op, [A], St).

%% Numeric operators.
op('+', A1, A2, St) ->
    numeric_op('+', A1, A2, <<"__add">>, fun (N1,N2) -> N1+N2 end, St);
op('-', A1, A2, St) ->
    numeric_op('-', A1, A2, <<"__sub">>, fun (N1,N2) -> N1-N2 end, St);
op('*', A1, A2, St) ->
    numeric_op('*', A1, A2, <<"__mul">>, fun (N1,N2) -> N1*N2 end, St);
op('/', A1, A2, St) ->
    numeric_op('/', A1, A2, <<"__div">>, fun (N1,N2) -> N1/N2 end, St);
op('%', A1, A2, St) ->
    numeric_op('%', A1, A2, <<"__mod">>,
	       fun (N1,N2) -> N1 - round(N1/N2 - 0.5)*N2 end, St);
op('^', A1, A2, St) ->
    numeric_op('^', A1, A2, <<"__pow">>,
	       fun (N1,N2) -> math:pow(N1, N2) end, St);
%% Relational operators, getting close.
op('==', A1, A2, St) -> eq_op('==', A1, A2, St);
op('~=', A1, A2, St) -> neq_op('~=', A1, A2, St);
op('<=', A1, A2, St) -> le_op('<=', A1, A2, St);
op('>=', A1, A2, St) -> le_op('>=', A2, A1, St);
op('<', A1, A2, St) -> lt_op('<', A1, A2, St);
op('>', A1, A2, St) -> lt_op('>', A2, A1, St);
%% String operator.
op('..', A1, A2, St) ->
    B1 = luerl_lib:tostring(A1),
    B2 = luerl_lib:tostring(A2),
    if B1 =/= nil, B2 =/= nil -> {[<< B1/binary,B2/binary >>],St};
       true ->
	    Meta = getmetamethod(A1, A2, <<"__concat">>, St),
	    functioncall(Meta, [A1,A2], St)
    end;
%% Bad args here.
op(Op, A1, A2, St) -> badarg_error(Op, [A1,A2], St).

%% numeric_op(Op, Arg, Event, Raw, State) -> {[Ret],State}.
%% numeric_op(Op, Arg, Arg, Event, Raw, State) -> {[Ret],State}.
%% eq_op(Op, Arg, Arg, State) -> {[Ret],State}.
%% neq_op(Op, Arg, Arg, State) -> {[Ret],State}.
%% lt_op(Op, Arg, Arg, State) -> {[Ret],State}.
%% le_op(Op, Arg, Arg, State) -> {[Ret],State}.
%%  Straigt out of the reference manual.

numeric_op(_Op, O, E, Raw, St0) ->
    N = luerl_lib:tonumber(O),
    if is_number(N) -> {[Raw(N)],St0};
       true ->
    	    Meta = getmetamethod(O, E, St0),
    	    {Ret,St1} = functioncall(Meta, [O], St0),
    	    {[is_true_value(Ret)],St1}
    end.

numeric_op(_Op, O1, O2, E, Raw, St0) ->
    N1 = luerl_lib:tonumber(O1),
    N2 = luerl_lib:tonumber(O2),
    if is_number(N1), is_number(N2) -> {[Raw(N1, N2)],St0};
       true ->
    	    io:fwrite("no: ~p\n", [{_Op,O1,O2,N1,N2}]),
    	    Meta = getmetamethod(O1, O2, E, St0),
    	    {Ret,St1} = functioncall(Meta, [O1,O2], St0),
    	    {[is_true_value(Ret)],St1}
    end.

eq_op(_Op, O1, O2, St) when O1 =:= O2 -> {[true],St};
eq_op(_Op, O1, O2, St0) ->
    {Ret,St1} = eq_meta(O1, O2, St0),
    {[is_true_value(Ret)],St1}.

neq_op(_Op, O1, O2, St) when O1 =:= O2 -> {[false],St};
neq_op(_Op, O1, O2, St0) ->
    {Ret,St1} = eq_meta(O1, O2, St0),
    {[not is_true_value(Ret)],St1}.

eq_meta(O1, O2, St0) ->
    case getmetamethod(O1, <<"__eq">>, St0) of
	nil -> {[false],St0};			%Tweren't no method
	Meta ->
	    case getmetamethod(O2, <<"__eq">>, St0) of
		Meta ->				%Must be the same method
		    functioncall(Meta, [O1,O2], St0);
		_ -> {[false],St0}
	    end
    end.

lt_op(_Op, O1, O2, St) when is_number(O1), is_number(O2) -> {[O1 < O2],St};
lt_op(_Op, O1, O2, St) when is_binary(O1), is_binary(O2) -> {[O1 < O2],St};
lt_op(_Op, O1, O2, St0) ->
    Meta = getmetamethod(O1, O2, <<"__lt">>, St0),
    {Ret,St1} = functioncall(Meta, [O1,O2], St0),
    {[is_true_value(Ret)],St1}.

le_op(_Op, O1, O2, St) when is_number(O1), is_number(O2) -> {[O1 =< O2],St};
le_op(_Op, O1, O2, St) when is_binary(O1), is_binary(O2) -> {[O1 =< O2],St};
le_op(_Op, O1, O2, St0) ->
    case getmetamethod(O1, O2, <<"__le">>, St0) of
	Meta when Meta =/= nil ->
	    {Ret,St1} = functioncall(Meta, [O1,O2], St0),
	    {[is_true_value(Ret)],St1};
	nil ->
	    %% Try for not (Op2 < Op1) instead.
	    Meta = getmetamethod(O1, O2, <<"__lt">>, St0),
	    {Ret,St1} = functioncall(Meta, [O2,O1], St0),
	    {[not is_true_value(Ret)],St1}
    end.

%% is_true_value(Rets) -> boolean().

is_true_value([nil|_]) -> false;
is_true_value([false|_]) -> false;
is_true_value([_|_]) -> true;
is_true_value([]) -> false;
is_true_value(nil) -> false;
is_true_value(false) -> false;
is_true_value(_) -> true.

%% first_value(Rets) -> Value.

first_value([V|_]) -> V;
first_value([]) -> nil.

%% gc(State) -> State.
%%  The garbage collector. Its main job is to reclaim unused tables
%%  and frames. It is a mark/sweep collector which passes over all
%%  objects and marks tables and frames which it has seen. All unseen
%%  tables and frames are then freed and their indexes added to the
%%  free lists.

gc(#luerl{ttab=Tt0,tfree=Tf0,g=G,env=Env,ftab=Ft0,ffree=Ff0,meta=Meta}=St) ->
    %% The root set consisting of global table and environment.
    Root = [Meta#meta.number,Meta#meta.string,Meta#meta.userdata,G|Env],
    %% Mark all seen tables and frames, i.e. return them.
    {SeenT,SeenF} = mark(Root, [], [], [], Tt0, Ft0),
    io:format("gc: ~p\n", [{SeenT,SeenF}]),
    %% Free unseen tables and add freed to free list.
    {Tf1,Tt1} = filter_tables(SeenT, Tf0, Tt0),
    {Ff1,Ft1} = filter_frames(SeenF, Ff0, Ft0),
    St#luerl{ttab=Tt1,tfree=Tf1,ftab=Ft1,ffree=Ff1}.

%% mark(ToDo, MoreTodo, SeenTabs, SeenFrames, Tabs, Frames) ->
%%     {SeenTabs,SeenFrames}.
%% Scan over all live objects and mark seen tables by adding them to
%% the seen list.

mark([{in_table,_}=T|Todo], More, St, Sf, Tt, Ft) ->
    %%io:format("gc: ~p\n", [T]),
    mark(Todo, More, St, Sf, Tt, Ft);
mark([#tref{i=T}|Todo], More, St0, Sf, Tt, Ft) ->
    case ordsets:is_element(T, St0) of
	true ->					%Already done
	    mark(Todo, More, St0, Sf, Tt, Ft);
	false ->				%Mark it and add to todo
	    St1 = ordsets:add_element(T, St0),
	    #table{a=Arr,t=Tab,m=Meta} = ?GET_TABLE(T, Tt),
	    %% Have to be careful where add Tab and Meta as Tab is
	    %% [{Key,Val}], Arr is array and Meta is
	    %% nil|#tref{i=M}. We want lists.
	    Aes = array:sparse_to_list(Arr),
	    Tes = ttdict:to_list(Tab),
	    mark([Meta|Todo], [[{in_table,T}],Tes,Aes,[{in_table,-T}]|More],
		 St1, Sf, Tt, Ft)
    end;
mark([#fref{i=F}|Todo], More, St, Sf0, Tt, Ft) ->
    case ordsets:is_element(F, Sf0) of
	true ->					%Already done
	    mark(Todo, More, St, Sf0, Tt, Ft);
	false ->				%Mark it and add to todo
	    Sf1 = ordsets:add_element(F, Sf0),
	    Ses = tuple_to_list(array:get(F, Ft)),
	    mark(Todo, [Ses|More], St, Sf1, Tt, Ft)
    end;
mark([#function{env=Env}|Todo], More, St, Sf, Tt, Ft) ->
    mark(Todo, [Env|More], St, Sf, Tt, Ft);
%% Catch these as they would match table key-value pair.
mark([{function,_}|Todo], More, St, Sf, Tt, Ft) ->
    mark(Todo, More, St, Sf, Tt, Ft);
mark([#thread{}|Todo], More, St, Sf, Tt, Ft) ->
    mark(Todo, More, St, Sf, Tt, Ft);
mark([#userdata{m=Meta}|Todo], More, St, Sf, Tt, Ft) ->
    mark([Meta|Todo], More, St, Sf, Tt, Ft);
mark([{K,V}|Todo], More, St, Sf, Tt, Ft) ->	%Table key-value pair
    %%io:format("mt: ~p\n", [{K,V}]),
    mark([K,V|Todo], More, St, Sf, Tt, Ft);
mark([_|Todo], More, St, Sf, Tt, Ft) ->		%Can ignore everything else
    mark(Todo, More, St, Sf, Tt, Ft);
mark([], [M|More], St, Sf, Tt, Ft) ->
    mark(M, More, St, Sf, Tt, Ft);
mark([], [], St, Sf, _, _) -> {St,Sf}.

%% filter_tables(Seen, Free, Tables) -> {Free,Tables}.
%% filter_frames(Seen, Free, Frames) -> {Free,Frames}.
%%  Filter tables/frames and return updated free lists and
%%  tables/frames.

filter_tables(Seen, Tf0, Tt0) ->
    Tf1 = ?FOLD_TABLES(fun (K, _, Free) ->
			       case ordsets:is_element(K, Seen) of
				   true -> Free;
				   false -> [K|Free]
			       end
		       end, Tf0, Tt0),
    Tt1 = ?FILTER_TABLES(fun (K, _) -> ordsets:is_element(K, Seen) end, Tt0),
    {Tf1,Tt1}.

filter_frames(Seen, Ff0, Ft0) ->
    %% Unfortunately there is no array:sparse_mapfoldl.
    Ff1 = array:sparse_foldl(fun (F, _, Free) ->
				     case ordsets:is_element(F, Seen) of
					 true -> Free;
					 false -> [F|Free]
				     end
			     end, Ff0, Ft0),
    Ft1 = array:sparse_map(fun (F, Fd) ->
				   case ordsets:is_element(F, Seen) of
				       true -> Fd;
				       false -> undefined
				   end
			   end, Ft0),
    {Ff1,Ft1}.
