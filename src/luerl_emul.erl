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
-export([load_chunk/2,load_chunk/3,load_function/2,load_function/3]).

%% Internal functions which can be useful "outside".
-export([alloc_table/1,alloc_table/2,free_table/2,
	 functioncall/3,methodcall/4,
	 get_table_keys/2,get_table_keys/3,
	 set_table_keys/3,set_table_keys/4,
	 get_table_key/3,set_table_key/4,
         alloc_userdata/2,alloc_userdata/3,get_userdata/2,set_userdata/3,
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
    %% Initialise the table handling.
    St1 = St0#luerl{ttab=?MAKE_TABLE(),tfree=[],tnext=0},
    %% Initialise the frame handling.
    St2 = St1#luerl{ftab=array:new(),ffree=[],fnext=0},
    %% Initialise the userdata handling.
    St3 = St2#luerl{utab=?MAKE_TABLE(),ufree=[],unext=0},
    %% Allocate the _G table and initialise the environment
    {_G,St4} = luerl_lib_basic:install(St3),	%Global environment
    St5 = St4#luerl{g=_G},
    %% Now we can start adding libraries. Package MUST be first!
    St6 = load_lib(<<"package">>, luerl_lib_package, St5),
    %% Add the other standard libraries.
    St7 = load_libs([
		     {<<"bit32">>,luerl_lib_bit32},
		     {<<"io">>,luerl_lib_io},
		     {<<"math">>,luerl_lib_math},
		     {<<"os">>,luerl_lib_os},
		     {<<"string">>,luerl_lib_string},
		     {<<"utf8">>,luerl_lib_utf8},
		     {<<"table">>,luerl_lib_table},
		     {<<"debug">>,luerl_lib_debug}
		    ], St6),
    %% Set _G variable to point to it and add it to packages.loaded.
    St8 = set_global_key(<<"_G">>, _G, St7),
    set_table_keys([<<"package">>,<<"loaded">>,<<"_G">>], _G, St8).

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
    #table{a=A1,d=D1,m=nil}.

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
%%  NOTE: WE ALWAYS RETURN A SINGLE_INFO_EMPTY VALUE!

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
    #table{d=Dict0,m=Meta}=T = ?GET_TABLE(N, Ts0),	%Get the table
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
    #table{a=Arr0,m=Meta}=T = ?GET_TABLE(N, Ts0),	%Get the table
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
    #table{d=Dict,m=Meta} = ?GET_TABLE(N, Ts),	%Get the table.
    case ttdict:find(Key, Dict) of
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
    case get_metamethod_tab(Meta, <<"__index">>, Ts) of
	nil -> {nil,St};
	Meth when ?IS_FUNCTION(Meth) ->
	    {Vs,St1} = functioncall(Meth, [T,Key], St),
	    {first_value(Vs),St1};
	Meth ->				%Recurse down the metatable
	    get_table_key(Meth, Key, St)
    end.
%% alloc_userdata(Data, State) -> {Uref,State}.
%% alloc_userdata(Data, Meta, State) -> {Uref,State}.
%% set_userdata(Uref, UserData, State) -> State.
%% get_userdata(Uref, State) -> {UserData,State}.

alloc_userdata(Data, St) ->
    alloc_userdata(Data, nil, St).

alloc_userdata(Data, Meta, #luerl{utab=Us0,ufree=[N|Ns]}=St) ->
    Us1 = ?SET_TABLE(N, #userdata{d=Data,m=Meta}, Us0),
    {#uref{i=N},St#luerl{utab=Us1,ufree=Ns}};
alloc_userdata(Data, Meta, #luerl{utab=Us0,ufree=[],unext=N}=St) ->
    Us1 = ?SET_TABLE(N, #userdata{d=Data,m=Meta}, Us0),
    {#uref{i=N},St#luerl{utab=Us1,unext=N+1}}.

set_userdata(#uref{i=N}, Data, #luerl{utab=Us0}=St) ->
    Us1 = ?UPD_TABLE(N, fun (Ud) -> Ud#userdata{d=Data} end, Us0),
    St#luerl{utab=Us1}.

get_userdata(#uref{i=N}, #luerl{utab=Us}=St) ->
    #userdata{} = Udata = ?GET_TABLE(N, Us),
    {Udata,St}.

%% make_userdata(Data) -> make_userdata(Data, nil).
%% make_userdata(Data, Meta) -> #userdata{d=Data,m=Meta}.

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

%% load_chunk(FunctionDefCode, State) -> {Function,State}.
%% load_chunk(FunctionDefCode, Env, State) -> {Function,State}.
%%  Load a chunk from the compiler.

load_chunk(Code, St) -> load_chunk(Code, [], St).

load_chunk(#code{code=Code}, Env, St) ->
    load_function(Code, Env, St).

%% load_function(FunctionDefCode, State) -> {Function,State}.
%% load_function(FunctionDefCode, Env, State) -> {Function,State}.
%%  Load a compilefunction definition instructions returning a callable
%%  function. Currently it does nothing with the state.

load_function(F, St) -> load_function(F, [], St).

load_function([?PUSH_FDEF_INFO_NOTUSED(Lsz, Esz, Pars, Is)], Env, St) ->
    do_fdef(Lsz, Esz, Pars, Is, Env, St).

%% call(Function, State) -> {Return,State}.
%% call(Function, Args, State) -> {Return,State}.
call(Func, St) ->
  call(Func, [], St).

call(#lua_func{}=Func, Args, St0) ->		%Already defined
    {Ret,St1} = functioncall(Func, Args, St0),
  %% Should do GC here.
    {Ret,St1};
call(#erl_func{}=Func, Args, St0) ->		%Internal erlang function
  {Ret,St1} = functioncall(Func, Args, St0),
    %% Should do GC here.
    {Ret,St1}.

itrace_print(Format, Args) ->
    ?ITRACE_DO(io:fwrite(Format, Args)).

%% exp(_, _) ->
%%     error(boom).

-record(call_frame, {anno=[], local_vars,env}).		%Save these for the GC

%% emul(Instrs, State).
%% emul(Instrs, LocalVariables, Stack, Env, State).

emul(Instructions, St) ->
    emul(Instructions, {}, [], [], St).

emul([I|_]= Instructions, Lvs, Stk, Env, St) ->
  ?ITRACE_DO(begin
		   io:fwrite("I: ~p\n", [I]),
		   io:fwrite("~p\n", [{Lvs,Env}]),
		   io:fwrite("St: "),
		   stack_print(Stk)
	       end),
    emul_1(Instructions, Lvs, Stk, Env, St);
emul([], Lvs, Stk, Env, St) ->
    ?ITRACE_DO(begin
		   io:fwrite("I: []\n"),
		   io:fwrite("~p\n", [{Lvs,Env}]),
		   io:fwrite("St: "),
		   stack_print(Stk)
	       end),
    emul_1([], Lvs, Stk, Env, St).

stack_print([#call_frame{}=E|St]) ->
    io:fwrite(" ~p\n", [E]),
    stack_print(St);
stack_print([E|St]) ->
    io:fwrite(" ~p", [E]),
    stack_print(St);
stack_print([]) -> io:nl().


coverage(#info_structure{ source_file=File,
            linenum=LineNum,
            token_position_in_line=StatementPositionInLine,
            original_token_description=OriginalTokenDescription,
            internal_statement=InternalStatement } = Info,
         State % To Save with executed Instructions to get variables during debugging
    ) ->

  % save last executed line and filename for error messages
  put(info_last_executed_token, #{last_executed_filename => File, last_executed_linenum => LineNum}),

  % the debug info reduce the speed of Luerl, so turn it on only if you want to debug
  SaveDebugInfoToFile = true,

  case SaveDebugInfoToFile of
    false -> State;
    true ->
      CoverageStatementFileCounter = case get(coverage_statement_file_counter) of
                                       undefined -> 1;
                                       StatementFileCounter -> StatementFileCounter+1
                                     end,
      put(coverage_statement_file_counter, CoverageStatementFileCounter),
      StateFile = lists:flatten(io_lib:format("/tmp/statefile_~p", [CoverageStatementFileCounter])),


      % Save statements for every executed command to get local/global variables if you want to debug
      %%%%% TURN ON/OFF when you need it: %%
      % luerl:log_to_file(StateFile, "~p", [State])
      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      %% LOG TO FILE: DEBUGGING, you can follow the process of LUA PROGRAM WITH THIS LOG
      ErlangTimestamp = luerl:obj_to_string(erlang:timestamp()),
      % File has to be the last because if Lua code comes from string, there is no filename but only a long string


      CoverageNow = luerl:obj_to_string(
      "coverage: line: ~p  statement pos: ~p (~p  ~p) start_time: ~p, StateFile: ~p  File: ~p",
        [LineNum, StatementPositionInLine, OriginalTokenDescription, InternalStatement, ErlangTimestamp, StateFile, File]
      ),
      [ElemFirst, LuaMap | ElemOthers ] = tuple_to_list(State),
      CoverageInfoPrev = maps:get(coverage_info, LuaMap, []),
      CoverageInfoUpdated = [CoverageNow | CoverageInfoPrev],
      LuaMapUpdated=LuaMap#{coverage_info=>CoverageInfoUpdated},
      erlang:list_to_tuple([ElemFirst, LuaMapUpdated | ElemOthers])
  end;
coverage(NotInfoStructure, State) -> % -no-file-but-string, no-file-but-forms chunks has not Info structs
  State. % I don't want to log them now
  %luerl:log_to_file("coverage not info structure: ~p", [NotInfoStructure]).

%% Expression instructions.
emul_1([?PUSH_LIT_INFO(L)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, [L|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_LVAR_INFO(D, I)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    Val = get_local_var(D, I, Lvs),
    emul(Is, Lvs, [Val|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_EVAR_INFO(D, I)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Val = get_env_var(D, I, Env, StWithCoverInfo),
    emul(Is, Lvs, [Val|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_GVAR_INFO(K)|Is], Lvs, Stk, Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Val,St1} = get_global_var(K, StWithCoverInfo),
    emul(Is, Lvs, [Val|Stk], Env, St1);

emul_1([?PUSH_LAST_LIT_INFO(L)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, [[L]|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_LAST_LVAR_INFO(D, I)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    Val = get_local_var(D, I, Lvs),
    emul(Is, Lvs, [[Val]|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_LAST_EVAR_INFO(D, I)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    %% io:fwrite("pe: ~p\n", [{D,I,St#luerl.env}]),
    Val = get_env_var(D, I, Env, StWithCoverInfo),
    emul(Is, Lvs, [[Val]|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_LAST_GVAR_INFO(K)|Is], Lvs, Stk, Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Val,St1} = get_global_var(K, StWithCoverInfo),
    emul(Is, Lvs, [[Val]|Stk], Env, St1);
emul_1([?STORE_LVAR_INFO(D, I)|Is], Lvs0, [V|Stk], Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    Lvs1 = set_local_var(D, I, V, Lvs0),
    emul(Is, Lvs1, Stk, Env, StWithCoverInfo);
emul_1([?STORE_EVAR_INFO(D, I)|Is], Lvs, [V|Stk], Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    St1 = set_env_var(D, I, V, Env, StWithCoverInfo),
    emul(Is, Lvs, Stk, Env, St1);
emul_1([?STORE_GVAR_INFO(K)|Is], Lvs, [V|Stk], Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    St1 = set_global_var(K, V, StWithCoverInfo),
    emul(Is, Lvs, Stk, Env, St1);

emul_1([?GET_KEY_INFO|Is], Lvs, [Key,Tab|Stk], Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Val,St1} = get_table_key(Tab, Key, StWithCoverInfo),
    emul(Is, Lvs, [Val|Stk], Env, St1);
emul_1([?GET_LIT_KEY_INFO(K)|Is], Lvs, [Tab|Stk], Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    %% [?PUSH_LIT_INFO_EMPTY(K),?GET_KEY_INFO_EMPTY]
    {Val,St1} = get_table_key(Tab, K, StWithCoverInfo),
    emul(Is, Lvs, [Val|Stk], Env, St1);
emul_1([?SET_KEY_INFO|Is], Lvs, [Key,Tab,Val|Stk], Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    St1 = set_table_key(Tab, Key, Val, StWithCoverInfo),
    emul_1(Is, Lvs, Stk, Env, St1);
emul_1([?SET_LIT_KEY_INFO(Key)|Is], Lvs, [Tab,Val|Stk], Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    %% [?PUSH_LIT_INFO_EMPTY(K),?SET_KEY_INFO_EMPTY]
    St1 = set_table_key(Tab, Key, Val, StWithCoverInfo),
    emul_1(Is, Lvs, Stk, Env, St1);

emul_1([?SINGLE_INFO|Is], Lvs, [Val|Stk], Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, [first_value(Val)|Stk], Env, StWithCoverInfo);
emul_1([?MULTIPLE_INFO|Is], Lvs, [Val|Stk], Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, [multiple_value(Val)|Stk], Env, StWithCoverInfo);

emul_1([?BUILD_TAB_INFO(Fc, I)|Is], Lvs, Stk0, Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Tab,Stk1,St1} = build_tab(Fc, I, Stk0, StWithCoverInfo),
    emul(Is, Lvs, [Tab|Stk1], Env, St1);

emul_1([?FCALL_INFO(0)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_fcall_0(Is, Lvs, Stk, Env, StWithCoverInfo);
emul_1([?FCALL_INFO(1)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_fcall_1(Is, Lvs, Stk, Env, StWithCoverInfo);
emul_1([?FCALL_INFO(2)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_fcall_2(Is, Lvs, Stk, Env, StWithCoverInfo);
emul_1([?FCALL_INFO(Ac)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_fcall(Is, Lvs, Stk, Env, StWithCoverInfo, Ac);

emul_1([?TAIL_FCALL_INFO(Ac)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_tail_fcall(Is, Lvs, Stk, Env, StWithCoverInfo, Ac);

emul_1([?MCALL_INFO(K, 0)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_mcall_0(Is, Lvs, Stk, Env, StWithCoverInfo, K);
emul_1([?MCALL_INFO(K, 1)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_mcall_1(Is, Lvs, Stk, Env, StWithCoverInfo, K);
emul_1([?MCALL_INFO(K, 2)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_mcall_2(Is, Lvs, Stk, Env, StWithCoverInfo, K);
emul_1([?MCALL_INFO(K, Ac)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_mcall(Is, Lvs, Stk, Env, StWithCoverInfo, K, Ac);

emul_1([?OP_INFO(Op,1)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_op1(Is, Lvs, Stk, Env, StWithCoverInfo, Op);
emul_1([?OP_INFO(Op,2)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_op2(Is, Lvs, Stk, Env, StWithCoverInfo, Op);

emul_1([?PUSH_FDEF_INFO(Lsz, Esz, Pars, Fis)|Is], Lvs, Stk, Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Func,St1} = do_fdef(Lsz, Esz, Pars, Fis, Env, StWithCoverInfo),
    emul(Is, Lvs, [Func|Stk], Env, St1);

emul_1([?BLOCK_INFO(Lsz, Esz, Bis)|Is], Lvs0, Stk0, Env0, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Lvs1,Stk1,Env1,St1} = do_block(Bis, Lvs0, Stk0, Env0, StWithCoverInfo, Lsz, Esz),
    emul(Is, Lvs1, Stk1, Env1, St1);

emul_1([?WHILE_INFO(Eis, Wis)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_while(Is, Lvs, Stk, Env, StWithCoverInfo, Eis, Wis);
emul_1([?REPEAT_INFO(Ris)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_repeat(Is, Lvs, Stk, Env, StWithCoverInfo, Ris);
emul_1([?AND_THEN_INFO(T)|Is], Lvs, [Val|Stk1]=Stk0, Env, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    {Lvs1,Stk2,Env1,St1} = emul(T, Lvs, Stk1, Env, StWithCoverInfo),
	    emul(Is, Lvs1, Stk2, Env1, St1);
	false ->
	    emul(Is, Lvs, Stk0, Env, StWithCoverInfo)
    end;
emul_1([?OR_ELSE_INFO(T)|Is], Lvs, [Val|Stk1]=Stk0, Env, St0) ->
  StWithCoverInfo=coverage(Info, St0),
    %% This is an expression and must always leave a value on stack.
    case boolean_value(Val) of
	true ->
	    emul(Is, Lvs, Stk0, Env, StWithCoverInfo);
	false ->
    {Lvs1,Stk2,Env1,St1} = emul(T, Lvs, Stk1, Env, StWithCoverInfo),
	    emul(Is, Lvs1, Stk2, Env1, St1)
    end;
emul_1([?IF_TRUE_INFO(T)|Is], Lvs, [Val|Stk0], Env, St0) ->
  StWithCoverInfo=coverage(Info, St0),
    %% This is a statement and pops the boolean value.
    case boolean_value(Val) of
	true ->
	    {Lvs1,Stk1,Env1,St1} = emul(T, Lvs, Stk0, Env, StWithCoverInfo),
	    emul(Is, Lvs1, Stk1, Env1, St1);
	false ->
	    emul(Is, Lvs, Stk0, Env, StWithCoverInfo)
    end;
emul_1([?IF_FALSE_INFO(T)|Is], Lvs, [Val|Stk0], Env, St0) ->
  StWithCoverInfo=coverage(Info, St0),
    %% This is a statement and pops the boolean value.
    case boolean_value(Val) of
	true ->
	    emul(Is, Lvs, Stk0, Env, StWithCoverInfo);
	false ->
	    {Lvs1,Stk1,Env1,St1} = emul(T, Lvs, Stk0, Env, StWithCoverInfo),
	    emul(Is, Lvs1, Stk1, Env1, St1)
    end;
emul_1([?IF_INFO(True, False)|Is], Lvs0, Stk0, Env0, St0) ->
    StWithCoverInfo=coverage(Info, St0),
    {Lvs1,Stk1,Env1,St1} = do_if(Lvs0, Stk0, Env0, StWithCoverInfo, True, False),
    emul(Is, Lvs1, Stk1, Env1, St1);
emul_1([?NFOR_INFO(V, Fis)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_numfor(Is, Lvs, Stk, Env, StWithCoverInfo, V, Fis);
emul_1([?GFOR_INFO(Vs, Fis)|Is], Lvs, Stk, Env, St) ->
    StWithCoverInfo=coverage(Info, St),
    do_genfor(Is, Lvs, Stk, Env, StWithCoverInfo, Vs, Fis);
emul_1([?BREAK_INFO|_], Lvs, Stk, Env, St) ->
  StWithCoverInfo=coverage(Info, St),
    throw({break,St#luerl.tag,Lvs,Stk,Env,StWithCoverInfo});
emul_1([?RETURN_INFO(0)|_], _, _, _, St) ->
  StWithCoverInfo=coverage(Info, St),
    throw({return,St#luerl.tag,[],StWithCoverInfo});
emul_1([?RETURN_INFO(Ac)|_], _, Stk, _, St) ->
  StWithCoverInfo=coverage(Info, St),
    {Ret,_} = pop_vals(Ac, Stk),
    throw({return,St#luerl.tag,Ret,StWithCoverInfo});
%% Stack instructions
emul_1([?POP_INFO|Is], Lvs, [_|Stk], Env, St) ->	%Just pop top off stack
  StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, Stk, Env, StWithCoverInfo);
emul_1([?POP2_INFO|Is], Lvs, [_,_|Stk], Env, St) ->	%Just pop top 2 off stack
  StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, Stk, Env, StWithCoverInfo);
emul_1([?SWAP_INFO|Is], Lvs, [S1,S2|Stk], Env, St) ->
  StWithCoverInfo=coverage(Info, St),
    emul(Is, Lvs, [S2,S1|Stk], Env, StWithCoverInfo);
emul_1([?DUP_INFO|Is], Lvs, [V|_]=Stk, Env, St) ->
  StWithCoverInfo=coverage(Info, St),
    emul_1(Is, Lvs, [V|Stk], Env, StWithCoverInfo);
emul_1([?PUSH_VALS_INFO(Vc)|Is], Lvs, [Vals|Stk0], Env, St) ->
  StWithCoverInfo=coverage(Info, St),
    %% Pop list off the stack and push Vc vals from it.
    Stk1 = push_vals(Vc, Vals, Stk0),
    emul(Is, Lvs, Stk1, Env, StWithCoverInfo);
emul_1([?POP_VALS_INFO(Vc)|Is], Lvs, Stk0, Env, St) ->
  StWithCoverInfo=coverage(Info, St),
    %% Pop Vc vals off the stack, put in a list and push.
    {Vals,Stk1} = pop_vals(Vc, Stk0),
    emul(Is, Lvs, [Vals|Stk1], Env, StWithCoverInfo);




%%%%  THIS SOLUTION IS A HOTFIX, because I don't understand exactly   %%%%%
%%%%  the source of the problem now                                   %%%%%

%%%% THE BLOCK_INFO_EMPTY STATEMENTS come without Info block. OR_ELSE_INFO_EMPTY  for_example

%%%%  Generic emul_1 without Info structure fixer                     %%%%%
%%%%  in this case I don't know from where but Instructions come      %%%%%
%%%%                                                                  %%%%%
%%%%          WITHOUT Info block.                                     %%%%%
%%%%
%%%%  if I can catch an instruction without Info, I fix it            %%%%%

%%%% example: [{push_lvar,1,4},{push_lit,0},{op,'<',2}]
%%%% these instructions are incorrect because they don't have Info
%%%% structure as first element


%%%%  IMPORTANT: it has to be before the last emul_1 line !!!!!!!
% emul_1([{}=InstructionInfoMaybeMissing|Is], Lvs0, Stk0, Env0, St0) ->
%
% ElemFirstOfInstruction = element(1, InstructionInfoMaybeMissing),
%
%   case ElemFirstOfInstruction of
%     #{} -> we_have_to_die_because_first_elem_was_a_map_and_there_wasnt_correct__emul_1__pattern;
%     _InstructionWithoutInfoMap ->
%       InfoNew = info_if_missing(),
%
%       % FIXME: it's too slow in PRODUCTION environment, use only in dev
%       luerl:log_to_file("MISSING INFO structure in tuple instructions: ~p", [InstructionInfoMaybeMissing]),
%
%       InstructionInList = erlang:tuple_to_list(InstructionInfoMaybeMissing),
%       InstructionWithInfo = erlang:list_to_tuple([InfoNew|InstructionInList]),
%       emul_1([InstructionWithInfo|Is], Lvs0, Stk0, Env0, St0)
%   end;

%emul_1([Instruction|Is], Lvs0, Stk0, Env0, St0) when is_atom(Instruction) ->
%  luerl:log_to_file("MISSING INFO structure in atom instructions: ~p", [Instruction]),
%  InfoNew = info_if_missing(),
%  InstructionWithInfo = {InfoNew, Instruction},
%  emul_1([InstructionWithInfo|Is], Lvs0, Stk0, Env0, St0);

%%%%% HOTFIX END %%%%%%%%%


emul_1([], Lvs, Stk, Env, St) ->
{Lvs,Stk,Env,St}.

info_if_missing() ->
  luerl_comp:token_info_new("Unknown Source File, MISSING INFO structure in instruction",
    -1, % -1 means: not a normal line num, because it has to be positive
    -1,
    "unknown token",
    "unknown_internal_statement").




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

%% push_vals(Count, ValList, Stack) -> {LastVal,Stack}.
%%  Push Count values from value list onto the stack. First value is
%%  deepest. Fill with 'nil' if not enough values.

push_vals(0, _, Stk) -> Stk;
push_vals(C, [V|Vs], Stk) ->
    push_vals(C-1, Vs, [V|Stk]);
push_vals(C, [], Stk) ->
    push_vals(C-1, [], [nil|Stk]).

%% do_block(Instrs, LocalVars, Stack, Env, State,
%%          LocalSize, EnvSize, BlockInstrs) -> ReturnFromEmul.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

%% do_block(Is, Lvs0, Stk0, Env, St0, 0, 0, Bis) ->
%%     %% No variables at all.
%%     {Lvs1,Stk1,_,St1} = emul(Bis, Lvs0, Stk0, Env, St0),
%%     emul(Is, Lvs1, Stk1, Env, St1);
%% do_block(Is, Lvs0, Stk0, Env, St0, 0, Esz, Bis) ->
%%     %% No local variables, only env variables.
%%     E = erlang:make_tuple(Esz, nil),
%%     {Fref,St1} = alloc_frame(E, St0),
%%     {Lvs1,Stk1,_,St2} = emul(Bis, Lvs0, Stk0, [Fref|Env], St1),
%%     emul(Is, Lvs1, Stk1, Env, St2);
%% do_block(Is, Lvs0, Stk0, Env, St0, Lsz, 0, Bis) ->
%%     %% No env variables, only local variables.
%%     L = erlang:make_tuple(Lsz, nil),
%%     {[_|Lvs1],Stk1,_,St1} = emul(Bis, [L|Lvs0], Stk0, Env, St0),
%%     emul(Is, Lvs1, Stk1, Env, St1);
%% do_block(Is, Lvs0, Stk0, Env, St0, Lsz, Esz, Bis) ->
%%     %% Both local and env variables.
%%     L = erlang:make_tuple(Lsz, nil),
%%     E = erlang:make_tuple(Esz, nil),
%%     {Fref,St1} = alloc_frame(E, St0),
%%     {[_|Lvs1],Stk1,_,St2} = emul(Bis, [L|Lvs0], Stk0, [Fref|Env], St1),
%%     emul(Is, Lvs1, Stk1, Env, St2).

%% do_block(BlockInstrs, LocalVars, Stack, Env, State,
%%          LocalSize, EnvSize) -> {LocalVars,Stack,Env,State}.
%%  Local vars may have been updated so must continue with returned
%%  version. We also continue with returned stack. There should be no
%%  changes in the env.

do_block(Bis, Lvs, Stk, Env, St, 0, 0) ->
    %% No variables at all.
    emul(Bis, Lvs, Stk, Env, St);
do_block(Bis, Lvs0, Stk0, Env0, St0, 0, Esz) ->
    %% No local variables, only env variables.
    E = erlang:make_tuple(Esz, nil),
    {Fref,St1} = alloc_frame(E, St0),
    {Lvs1,Stk1,[_|Env1],St2} = emul(Bis, Lvs0, Stk0, [Fref|Env0], St1),
    {Lvs1,Stk1,Env1,St2};
do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, 0) ->
    %% No env variables, only local variables.
    L = erlang:make_tuple(Lsz, nil),
    {[_|Lvs1],Stk1,Env1,St1} = emul(Bis, [L|Lvs0], Stk0, Env0, St0),
    {Lvs1,Stk1,Env1,St1};
do_block(Bis, Lvs0, Stk0, Env0, St0, Lsz, Esz) ->
    %% Both local and env variables.
    L = erlang:make_tuple(Lsz, nil),
    E = erlang:make_tuple(Esz, nil),
    {Fref,St1} = alloc_frame(E, St0),
    {[_|Lvs1],Stk1,[_|Env1],St2} = emul(Bis, [L|Lvs0], Stk0, [Fref|Env0], St1),
    {Lvs1,Stk1,Env1,St2}.

%% do_op1(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.
%% do_op2(Instrs, LocalVars, Stack, Env, State, Op) -> ReturnFromEmul.

do_op1(Is, Lvs, [A|Stk], Env, St, Op) ->
  case op(Op, A) of
	{ok,Res} ->
	    emul(Is, Lvs, [Res|Stk], Env, St);
	{meta,Meta} ->
	    functioncall(Is, Lvs, Stk, Env, St, #erl_func{code=Meta}, []);
	{error,E} ->
    lua_error(E, St)
    end.

do_op2(Is, Lvs, [A2,A1|Stk], Env, St, Op) ->
  OpResult = op(Op, A1, A2),
    case OpResult of
	{ok,Res} ->
	    emul(Is, Lvs, [Res|Stk], Env, St);
	{meta,Meta} ->
	    functioncall(Is, Lvs, Stk, Env, St, #erl_func{code=Meta}, []);
	{error,E} ->
    lua_error(E, St)
    end.

%% do_fdef(LocalSize, EnvSize, Pars, Instrs, Env, State) -> {Function,State}.

do_fdef(Lsz, Esz, Pars, Is, Env, St) ->
    {#lua_func{local_var_size =Lsz, environment_var_size =Esz, parameters =Pars, environment =Env, code_block =Is},St}.

%% do_fcall_0(Instrs, LocalVars, Stack, Env, State) ->
%% do_fcall_1(Instrs, LocalVars, Stack, Env, State) ->
%% do_fcall_2(Instrs, LocalVars, Stack, Env, State) ->
%% do_fcall(Instrs, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_fcall_0(Is, Lvs, [Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, []).

do_fcall_1(Is, Lvs, [Alast,Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, Alast).

do_fcall_2(Is, Lvs, [Alast,A1,Func|Stk], Env, St) ->
    functioncall(Is, Lvs, Stk, Env, St, Func, [A1|Alast]).

do_fcall(Is, Lvs, Stk0, Env, St, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Func|Stk2] = Stk1,				%Get function
    functioncall(Is, Lvs, Stk2, Env, St, Func, Args).

%% functioncall(Function, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

functioncall(Func, Args, #luerl{stk=Stk}=St0) ->
    functioncall(Func, Args, Stk, St0).

%% functioncall(Instrs, LocalVars, Stk, Env, State, Func, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

functioncall(Instructions, LocalVars, Stk0, Env, St0, Func, Args) ->
    Fr = #call_frame{local_vars = LocalVars,env=Env},
    Stk1 = [Fr|Stk0],
    {Ret,St1} = functioncall(Func, Args, Stk1, St0),
    emul(Instructions, LocalVars, [Ret|Stk0], Env, St1).

%% do_tail_fcall(Instrs, Acc, LocalVars, Stack, Env, State, ArgCount) ->
%%     ReturnFromEmul.

do_tail_fcall(_Is, _Var, Stk, _Env, _St, 0) ->
    error({boom,[],Stk});
do_tail_fcall(_Is, _Var, Stk0, _Env, _St, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Func|Stk2] = Stk1,				%Get function
    error({boom,Func,Args,Stk2}).

%% do_mcall_0(Instrs, LocalVars, Stack, Env, State, Method) ->
%% do_mcall_1(Instrs, LocalVars, Stack, Env, State, Method) ->
%% do_mcall_2(Instrs, LocalVars, Stack, Env, State, Method) ->
%% do_mcall(Instrs, LocalVars, Stack, Env, State, Method, ArgCount) ->
%%     ReturnFromEmul.

do_mcall_0(Is, Lvs, [Obj|Stk], Env, St, M) ->
    %% The object is in the acc.
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, []).

do_mcall_1(Is, Lvs, [Alast,Obj|Stk], Env, St, M) ->
    %% The object is on the stack and the argument is in the acc.
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, Alast).

do_mcall_2(Is, Lvs, [Alast,A1,Obj|Stk], Env, St, M) ->
    %% The object and 1st argument are on the stack, the 2nd is in the acc.
    methodcall(Is, Lvs, Stk, Env, St, Obj, M, [A1|Alast]).

do_mcall(Is, Lvs, Stk0, Env, St, M, Ac) ->
    {Args,Stk1} = pop_vals(Ac, Stk0),		%Pop arguments
    [Obj|Stk2] = Stk1,				%Get function
    methodcall(Is, Lvs, Stk2, Env, St, Obj, M, Args).

%% methodcall(Object, Method, Args, State) -> {Return,State}.
%%  This is called from "within" things, for example metamethods, and
%%  expects everything necessary to be in the state.

methodcall(Obj, M, Args, St0) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, M, St0) of
	{nil,St1} ->				%No method
	    lua_error({undef_method,Obj,M}, St1);
	{Val,St1} ->
	    {Ret,St2} = functioncall(Val, [Obj|Args], St1#luerl.stk, St1),
	    {Ret,St2}
    end.

%% methodcall(Instrs, Var, Stk, Env, State, Object, Method, Args) -> <emul>
%%  This is called from within code and continues with Instrs after
%%  call. It must move everything into State.

methodcall(Is, Lvs, Stk0, Env, St0, Obj, M, Args) ->
    %% Get the function to call from object and method.
    case get_table_key(Obj, M, St0) of
	{nil,St1} ->				%No method
	    lua_error({undef_method,Obj,M}, St1);
	{Val,St1} ->
	    Fr = #call_frame{local_vars =Lvs,env=Env},
	    Stk1 = [Fr|Stk0],
	    {Ret,St2} = functioncall(Val, [Obj|Args], Stk1, St1),
	    emul(Is, Lvs, [Ret|Stk0], Env, St2)
    end.

%% functioncall(Function, Args, Stack, State) -> {Return,State}.
%%  Setup environment for function and do the actual call.

functioncall(
      #lua_func{local_var_size =0, environment_var_size =0, environment =Env, code_block =Instructions},
    _, Stk, St0) ->

    %% No variables at all.
    functioncall(Instructions, [], Stk, Env, St0);
functioncall(
             #lua_func{ local_var_size = 0, % VarsLocalSize
                        environment_var_size = VarEnvSize,
                        parameters = Pars,
                        environment = Env,
                        code_block = Instructions},
             Args, Stk, St0) ->

    %% No local variables, only env variables.
    E0 = erlang:make_tuple(VarEnvSize, nil),
    E1 = assign_env_pars(Pars, Args, E0),
    {Fref,St1} = alloc_frame(E1, St0),
    {Ret,St2} = functioncall(Instructions, [], Stk, [Fref|Env], St1),
    {Ret,St2};
functioncall(
             #lua_func{ local_var_size = VarsLocalSize,
                        environment_var_size = 0,  % VarEnvSize
                        parameters = Params,
                        environment = Env,
                        code_block = Instructions},
	     Args, Stk, St0) ->

    %% No env variables, only local variables.
    LocalEmtpy = erlang:make_tuple(VarsLocalSize, nil),
    LocalVars = assign_local_pars(Params, Args, LocalEmtpy),
    {Ret,St1} = functioncall(Instructions, [LocalVars], Stk, Env, St0),
    {Ret,St1};

functioncall(
             #lua_func{ local_var_size = VarsLocalSize,
                        environment_var_size = VarEnvSize,
                        parameters = Params,
                        environment = Env,
                        code_block = Instructions},
	     Args, Stk, St0) ->

    LocalEmpty = erlang:make_tuple(VarsLocalSize, nil),
    EnvEmpty = erlang:make_tuple(VarEnvSize, nil),
    {LocalVars, EnvVars} = assign_pars(Params, Args, LocalEmpty, EnvEmpty),
    {Fref,St1} = alloc_frame(EnvVars, St0),

    {Ret,St2} = functioncall(Instructions, [LocalVars], Stk, [Fref|Env], St1),
    {Ret,St2};
functioncall(  #erl_func{code=Instructions},
               Args, Stack, #luerl{stk=Stk0}=St0) ->
    %% Here we must save the stack in state as function may need it.
    {Ret,St1} = Instructions(Args, St0#luerl{stk= Stack}),
    {Ret,St1#luerl{stk=Stk0}};			%Replace it
functioncall(Instructions, Args, Stack, St) ->
  case get_metamethod(Instructions, <<"__call">>, St) of
	nil -> lua_error({undef_function, Instructions}, St);
	Meta -> functioncall(Meta, [Instructions |Args], Stack, St)
    end.

functioncall( Instructions,
              LocalVars, Stack, Env, St0) ->

    Tag = St0#luerl.tag,
    %% Must use different St names else they become 'unsafe'.
    %% io:fwrite("fc: ~p\n", [{Lvs,Env,St0#luerl.env}]),
    try
	      {_,_,_,Sta} = emul(Instructions, LocalVars, Stack, Env, St0),
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

%% do_if_block([?BLOCK_INFO_EMPTY(Lsz, Esz, Bis)], Lvs0, Stk0, Env0, St0, Is) ->
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
    if Step > 0.0, N =< Limit ->		%Keep going
	    {Lvs1,Stk1,Env1,St1} =
		emul(Fis, Lvs0, [N|Stk0], Env0, St0),
	    numfor_loop(N+Step, Limit, Step, Fis, Lvs1, Stk1, Env1, St1);
       Step < 0.0, N >= Limit ->		%Keep going
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
    (?GET_TABLE(T, Ts))#table.m;
get_metatable(#uref{i=U}, #luerl{utab=Us}) ->
    (?GET_TABLE(U, Us))#userdata.m;
get_metatable(nil, #luerl{meta=Meta}) -> Meta#meta.nil;
get_metatable(B, #luerl{meta=Meta}) when is_boolean(B) ->
    Meta#meta.boolean;
get_metatable(N, #luerl{meta=Meta}) when is_number(N) ->
    Meta#meta.number;
get_metatable(S, #luerl{meta=Meta}) when is_binary(S) ->
    Meta#meta.string;
get_metatable(_, _) -> nil.			%Other types have no metatables

set_metatable(#tref{i=N}, M, #luerl{ttab=Ts0}=St) ->
    Ts1 = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=M} end, Ts0),
    St#luerl{ttab=Ts1};
set_metatable(#uref{i=N}, M, #luerl{utab=Us0}=St) ->
    Us1 = ?UPD_TABLE(N, fun (Ud) -> Ud#userdata{m=M} end, Us0),
    St#luerl{utab=Us1};
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
    [{I,V}|build_tab_last(I+1.0, Vs)];
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

first_value([V|_]) -> V;
first_value([]) -> nil.

%%multiple_value(nil) -> [];			%Or maybe [nil]?
multiple_value(V) -> [V].

%% gc(State) -> State.
%%  The garbage collector. Its main job is to reclaim unused tables
%%  and frames. It is a mark/sweep collector which passes over all
%%  objects and marks tables and frames which it has seen. All unseen
%%  tables and frames are then freed and their indexes added to the
%%  free lists.

-record(gct, {t,s}).				%Gc table info table, seen

gc(#luerl{ttab=Tt0,tfree=Tf0,ftab=Ft0,ffree=Ff0,utab=Ut0,ufree=Uf0,
          g=G,stk=Stk,meta=Meta}=St) ->
    %% The root set consisting of global table and stack.
    Root = [Meta#meta.nil,Meta#meta.boolean,Meta#meta.number,Meta#meta.string,
	    G|Stk],
    %% Mark all seen tables and frames, i.e. return them.
    GcT = #gct{t=Tt0,s=[]},
    GcF = #gct{t=Ft0,s=[]},
    GcU = #gct{t=Ut0,s=[]},
    {SeenT,SeenF,SeenU} = mark(Root, [], GcT, GcF, GcU),
    %% io:format("gc: ~p\n", [{SeenT,SeenF,SeenU}]),
    %% Free unseen tables and add freed to free list.
    {Tf1,Tt1} = filter_tables(SeenT, Tf0, Tt0),
    {Ff1,Ft1} = filter_frames(SeenF, Ff0, Ft0),
    {Uf1,Ut1} = filter_userdata(SeenU, Uf0, Ut0),
    St#luerl{ttab=Tt1,tfree=Tf1,ftab=Ft1,ffree=Ff1,utab=Ut1,ufree=Uf1}.

%% mark(ToDo, MoreTodo, GcTabs, GcFrames, GcUserdata) ->
%%     {SeenTabs,SeenFrames,SeenUserdata}.
%% Scan over all live objects and mark seen tables by adding them to
%% the seen list.

mark([{in_table,_}=_T|Todo], More, GcT, GcF, GcU) ->
    %%io:format("gc: ~p\n", [_T]),
    mark(Todo, More, GcT, GcF, GcU);
mark([#tref{i=T}|Todo], More, #gct{s=St0,t=Tt}=GcT, GcF, GcU) ->
    case ordsets:is_element(T, St0) of
	true ->					%Already done
	    mark(Todo, More, GcT, GcF, GcU);
	false ->				%Mark it and add to todo
	    St1 = ordsets:add_element(T, St0),
	    #table{a=Arr,d=Dict,m=Meta} = ?GET_TABLE(T, Tt),
	    %% Have to be careful where add Tab and Meta as Tab is
	    %% [{Key,Val}], Arr is array and Meta is
	    %% nil|#tref{i=M}. We want lists.
	    Aes = array:sparse_to_list(Arr),
	    Des = ttdict:to_list(Dict),
	    mark([Meta|Todo], [[{in_table,T}],Des,Aes,[{in_table,-T}]|More],
		 GcT#gct{s=St1}, GcF, GcU)
    end;
mark([#fref{i=F}|Todo], More, GcT, #gct{s=Sf0,t=Ft}=GcF, GcU) ->
    case ordsets:is_element(F, Sf0) of
	true ->					%Already done
	    mark(Todo, More, GcT, GcF, GcU);
	false ->				%Mark it and add to todo
	    Sf1 = ordsets:add_element(F, Sf0),
	    Ses = tuple_to_list(array:get(F, Ft)),
	    mark(Todo, [Ses|More], GcT, GcF#gct{s=Sf1}, GcU)
    end;
mark([#uref{i=U}|Todo], More, GcT, GcF, #gct{s=Su0}=GcU) ->
    case ordsets:is_element(U, Su0) of
       true ->                                 %Already done
           mark(Todo, More, GcT, GcF, GcU);
       false ->
           Su1 = ordsets:add_element(U, Su0),
           mark(Todo, More, GcT, GcF, GcU#gct{s=Su1})
    end;
mark([#lua_func{environment =Env}|Todo], More, GcT, GcF, GcU) ->
    mark(Todo, [Env|More], GcT, GcF, GcU);
%% Catch these as they would match table key-value pair.
mark([#erl_func{}|Todo], More, GcT, GcF, GcU) ->
    mark(Todo, More, GcT, GcF, GcU);
mark([#thread{}|Todo], More, GcT, GcF, GcU) ->
    mark(Todo, More, GcT, GcF, GcU);
mark([#userdata{m=Meta}|Todo], More, GcT, GcF, GcU) ->
    mark([Meta|Todo], More, GcT, GcF, GcU);
mark([#call_frame{local_vars =Lvs,env=Env}|Todo], More0, GcT, GcF, GcU) ->
    More1 = [ tuple_to_list(Lv) || Lv <- Lvs ] ++ [Env|More0],
    mark(Todo, More1, GcT, GcF, GcU);
mark([{K,V}|Todo], More, GcT, GcF, GcU) ->	%Table key-value pair
    %%io:format("mt: ~p\n", [{K,V}]),
    mark([K,V|Todo], More, GcT, GcF, GcU);
mark([_|Todo], More, GcT, GcF, GcU) ->		%Can ignore everything else
    mark(Todo, More, GcT, GcF, GcU);
mark([], [M|More], GcT, GcF, GcU) ->
    mark(M, More, GcT, GcF, GcU);
mark([], [], #gct{s=St}, #gct{s=Sf}, #gct{s=Su}) -> {St,Sf,Su}.

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
