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

%% File    : luerl_new.erl
%% Authors : Robert Virding
%% Purpose : The new basic LUA 5.3 interface.

-module(luerl).

-include("luerl.hrl").

?MODULEDOC("""
Luerl is an implementation of Lua 5.3 written in Erlang.
This is the main public API module for interfacing with Luerl.

For Elixir users, the `Elixir.Luerl` module provides an idiomatic interface
with state as the first argument for better pipe operator usage.
""").

%% Basic user API to luerl.
-export([init/0,gc/1,
         load/2,load/3,loadfile/2,loadfile/3,
         path_loadfile/2,path_loadfile/3,path_loadfile/4,
         load_module/3,load_module_dec/3,
         do/2,do_dec/2,do/3,do_dec/3,
         dofile/2,dofile/3,dofile_dec/2,dofile_dec/3,
         call/3,call_chunk/2,call_chunk/3,
         call_function/3,call_function_enc/3,call_function_dec/3,
         call_method/4,call_method_dec/4,
         get_table_keys/2,get_table_keys_dec/2,
         set_table_keys/3,set_table_keys_dec/3,
         get_table_key/3,set_table_key/4,
         get_stacktrace/1
        ]).

%% Tracing.
-export([set_trace_func/2,clear_trace_func/1,
         set_trace_data/2,get_trace_data/1]).

%% Encoding and decoding.
-export([encode/2,encode_list/2,decode/2,decode_list/2]).

%% Helping with storing VM state
-export([externalize/1,internalize/1]).

%% Storing and retrieving private data
-export([put_private/3,get_private/2,delete_private/2]).

%% init() -> State.

init() ->
    luerl_emul:init().

%% gc(State) -> State.
gc(St) ->
    luerl_heap:gc(St).

%% set_trace_func(TraceFunction, State) -> State.
%% clear_trace_func(State) -> State.
%% get_trace_data(State) -> TraceData.
%% set_trace_data(TraceData, State) -> State.
%%  Set the trace function and access the trace data.

set_trace_func(Tfunc, St) ->
    St#luerl{trace_func=Tfunc}.

clear_trace_func(St) ->
    St#luerl{trace_func=none}.

get_trace_data(St) ->
    St#luerl.trace_data.

set_trace_data(Tdata, St) ->
    St#luerl{trace_data=Tdata}.

%% load(String|Binary, State) -> {ok,FuncRef,NewState}.
%% load(String|Binary, Options, State) -> {ok,FuncRef,NewState}.

load(Bin, St) -> load(Bin, [return], St).

load(Bin, Opts, St) when is_binary(Bin) ->
    load(binary_to_list(Bin), Opts, St);
load(Str, Opts, St0) ->
    case luerl_comp:string(Str, Opts) of
        {ok,Chunk} ->
            {FunRef,St1} = luerl_emul:load_chunk(Chunk, St0),
            {ok,FunRef,St1};
        Error ->                                %Compile error
            Error
    end.

%% loadfile(FileName, State) -> {ok,FuncRef,NewState}.
%% loadfile(FileName, Options, State) -> {ok,FuncRef,NewState}.

loadfile(Name, St) -> loadfile(Name, [return], St).

loadfile(Name, Opts, St0) ->
    case luerl_comp:file(Name, Opts) of
        {ok,Chunk} ->
            {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
            {ok,Func,St1};
        Error -> Error
    end.

%% path_loadfile(FileName, State) -> {ok,Function,FullName,State}.
%% path_loadfile(Path, FileName, State) -> {ok,Function,FullName,State}.
%% path_loadfile(Path, FileName, Options, State) ->
%%     {ok,Function,FullName,State}.
%%  When no path is given we use the value of LUA_LOAD_PATH.
%%  We manually step down the path to get the correct handling of
%%  filenames by the compiler.

path_loadfile(Name, St) ->
    Path = case os:getenv("LUA_LOAD_PATH") of
               false -> [];                     %You get what you asked for
               Env ->
                   %% Get path separator depending on os type.
                   Sep = case os:type() of
                             {win32,_} -> ";";  %Windows
                             _ -> ":"           %Unix
                         end,
                   string:tokens(Env, Sep)      %Split into path list
           end,
    path_loadfile(Path, Name, [return], St).

path_loadfile(Dirs, Name, St) ->
    path_loadfile(Dirs, Name, [return], St).

path_loadfile([Dir|Dirs], Name, Opts, St0) ->
    Full = filename:join(Dir, Name),
    case loadfile(Full, Opts, St0) of
        {ok,Func,St1} ->
            {ok,Func,Full,St1};
        {error,[{_,_,enoent}],_} ->             %Couldn't find the file
            path_loadfile(Dirs, Name, St0);
        Error -> Error
    end;
path_loadfile([], _, _, _) ->
    {error,[{none,file,enoent}],[]}.

%% load_module(LuaTablePath, ModuleName, State) -> State.
%%  Load module and add module table to the path.

load_module([_|_] = Lfp, Mod, St0) ->
    {Tab,St1} = Mod:install(St0),
    luerl_emul:set_table_keys(Lfp, Tab, St1);
load_module(_, _, _) ->
    error(badarg).

%% load_module_dec(DecodedTablePath, ModuleName, State) -> State.
%%  Load module and add module table to the path.

load_module_dec([_|_] = Dfp, Mod, St0) ->
    {Efp,St1} = encode_list(Dfp, St0),
    load_module(Efp, Mod, St1);
load_module_dec(_, _, _) ->
    error(badarg).

%% luerl:do(String|Binary|Form, State) ->
%% luerl:do(String|Binary|Form, CompileOptions, State) ->
%%     {ok,Result,NewState} | {lua_error,Error,State}.

do(S, St) -> do(S, [return], St).

do(S, Opts, St0) ->
    case load(S, Opts, St0) of
        {ok,Func,St1} ->
            call_function(Func, [], St1);
        Error -> Error
    end.

do_dec(S, St) ->
    do_dec(S, [return], St).

do_dec(S, Opts, St0) ->
    case do(S, Opts, St0) of
        {ok,Eret,St1} ->
            {ok,decode_list(Eret, St1),St1};
        Error -> Error
    end.

%% luerl:dofile(FileName, State) ->
%% luerl:dofile(FileName, CompileOptions, State) ->
%%     {ok,Result,NewState} | {lua_error,Error,State}.

dofile(File, St) -> dofile(File, [], St).

dofile(File, Opts, St0) ->
    case loadfile(File, Opts, St0) of
        {ok,Func,St1} ->
            call_function(Func, [], St1);
        Error -> Error
    end.

dofile_dec(File, St) ->
    dofile_dec(File, [], St).

dofile_dec(File, Opts, St0) ->
    case dofile(File, Opts, St0) of
        {ok,Eret,St1} ->
            {ok,decode_list(Eret, St1),St1};
        Error -> Error
    end.

%% call(FuncRef, Args, State) ->
%% call_chunk(FuncRef, State) ->
%% call_chunk(FuncRef, Args, State) ->
%%     {ok,Return,State} | {lua_error,Error,State}.

call(C, As, St) ->
    call_function(C, As, St).

call_chunk(C, St) ->
    call_chunk(C, [], St).

call_chunk(C, As, St) ->
    call_function(C, As, St).

%% call_function(LuaFuncRef | LuaTablePath, Args, State) ->
%%     {ok,LuaReturn,State} | {lua_error,Error,State}.

call_function(Epath, Args, St0) when is_list(Epath) ->
    {ok,Efunc,St1} = get_table_keys(Epath, St0),
    call_function(Efunc, Args, St1);
call_function(Func, Args, St0) ->
    try
        {Ret,St1} = luerl_emul:functioncall(Func, Args, St0),
        {ok,Ret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

%% call_function_enc(DecodedFuncRef, Args, State) ->
%%     {ok,LuaReturn,State} | {lua_error,Error,State}.

call_function_enc(Dtpath, Dargs, St0) ->
    {Epath,St1} = encode_list(Dtpath, St0),
    {Eargs,St2} = encode_list(Dargs, St1),
    call_function(Epath, Eargs, St2).

%% call_function_dec(DecodedFuncRef, Args, State) ->
%%     {ok,DecodedReturn,State} | {lua_error,Error,State}.

call_function_dec(Dtpath, Dargs, St0) ->
    case call_function_enc(Dtpath, Dargs, St0) of
        {ok,Eret,St1} ->
            {ok,decode_list(Eret, St1),St1};
        LuaError -> LuaError
    end.

%% call_method(LuaObject, Method, Args, State) ->
%%     {ok,Return,State} | {lua_error,Error,State}.

call_method(Obj, Meth, Args, St0) ->
    try
        {Ret,St1} = luerl_emul:methodcall(Obj, Meth, Args, St0),
        {ok,Ret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

%% call_method_dec(DecodedObject, Method, Args, State) ->
%%     {ok,DecodedReturn,State} | {lua_error,Error,State}.

call_method_dec(Dobj, Dmeth, Dargs, St0) ->
    {ok,Eobj,St1} = get_table_keys_dec(Dobj, St0),
    {Emeth,St2} = encode(Dmeth, St1),
    {Eargs,St3} = encode_list(Dargs, St2),
    case call_method(Eobj, Emeth, Eargs, St3) of
        {ok,Eret,St4} ->
            {ok,decode_list(Eret, St4),St4};
        LuaError -> LuaError
    end.

%% get_table_keys(Keys, State) ->
%% get_table_keys_dec(DecodedKeys, State) ->
%%     {ok,Return,State} | {lua_error,Error,State}.
%% set_table_keys(Keys, Val, State) ->
%% set_table_keys_dec(DecodedKeys, DecodedVal, State) ->
%%     {ok,State} | {lua_error,Error,State}.

get_table_keys(Keys, St0) ->
    try
        {Eret,St1} = luerl_emul:get_table_keys(Keys, St0),
        {ok,Eret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

get_table_keys_dec(Dkeys, St0) ->
    {Ekeys,St1} = encode_list(Dkeys, St0),
    case get_table_keys(Ekeys, St1) of
        {ok,Eret,St2} ->
            {ok,decode(Eret, St2),St2};
        LuaError -> LuaError
    end.

set_table_keys(Keys, Val, St0) ->
    try
        St1 = luerl_emul:set_table_keys(Keys, Val, St0),
        {ok,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

set_table_keys_dec(Dkeys, Dval, St0) ->
    {Ekeys,St1} = encode_list(Dkeys, St0),
    {Eval,St2} = encode(Dval, St1),
    set_table_keys(Ekeys, Eval, St2).

%% get_table_key(Tab, Key, State) ->
%%     {ok,Value,State} | {lua_error,Error,State}.
%% set_table_key(Tab, Key, Value, State) ->
%%     {ok,State} | {lua_error,Error,State}.

get_table_key(Tab, Key, St0) ->
    try
        {Eret,St1} = luerl_emul:get_table_key(Tab, Key, St0),
        {ok,Eret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

set_table_key(Tab, Key, Val, St0) ->
    try
        St1 = luerl_emul:set_table_key(Tab, Key, Val, St0),
        {ok,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

%% get_stacktrace(State) -> [{FuncName,[{file,FileName},{line,Line}]}].

get_stacktrace(#luerl{cs=Stack}=St) ->
    Fun = fun (Frame, Acc) -> do_stackframe(Frame, Acc, St) end,
    {_,Trace} = lists:foldl(Fun, {1,[]}, Stack),
    lists:reverse(Trace).

do_stackframe(#call_frame{func=Funref,args=Args}, {Line,Trace}, St) ->
    case Funref of
        #funref{} ->
            {Func,_} = luerl_heap:get_funcdef(Funref, St),
            Anno = Func#lua_func.anno,
            Name = case luerl_anno:get(name, Anno) of
                       undefined -> <<"-no-name-">>;
                       N -> N
                   end,
            File = luerl_anno:get(file, Anno),
            {Line,[{Name,Args,[{file,File},{line,Line}]} | Trace]};
        #erl_func{code=Fun} ->
            {module,Module} = erlang:fun_info(Fun, module),
            {name,Name} = erlang:fun_info(Fun, name),
            FileName = get_filename(Module),
            {Line,[{{Module,Name},Args,[{file,FileName}]} | Trace]};
        #erl_mfa{m=M,f=F,a=A} ->
            FileName = get_filename(M),
            %% {Line,[{{M,F},{A,Args},[{file,FileName}]} | Trace]};
            %% {Line,[{{M,F},[A | Args],[{file,FileName}]} | Trace]};
            {Line,[{{M,F,A},Args,[{file,FileName}]} | Trace]};
        Other ->
            {Line,[{Other,Args,[{file,<<"-no-file-">>},{line,Line}]} | Trace]}
    end;
do_stackframe(#current_line{line=Line}, {_,Trace}, _St) ->
    {Line,Trace};
do_stackframe(#loop_frame{}, Acc, _St) ->       %Ignore these
    Acc.

get_filename(Mod) ->
    Comp = erlang:get_module_info(Mod, compile),
    case lists:keyfind(source, 1, Comp) of
        {source,FileName} ->
            BaseName = filename:basename(FileName),
            list_to_binary(BaseName);
        false ->                                %The compiler doesn't know
            <<"-no-file-">>
    end.

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% encode_list([Term], State) -> {[LuerlTerm],State}.
%% encode(Term, State) -> {LuerlTerm,State}.

encode_list(Ts, St) ->
    lists:mapfoldl(fun encode/2, St, Ts).

encode(nil, St) -> {nil,St};
encode(false, St) -> {false,St};
encode(true, St) -> {true,St};
encode(B, St) when is_binary(B) -> {B,St};
encode(A, St) when is_atom(A) -> {atom_to_binary(A, utf8),St};
encode(N, St) when is_number(N) -> {N,St};      %Integers and floats
encode(F, St) when ?IS_MAP(F) -> encode(maps:to_list(F), St);
encode(L, St0) when is_list(L) ->
    %% Encode the table elements in the list.
    EncTab = fun ({K0,V0}, {I,S0}) ->
                     {K1,S1} = encode(K0, S0),
                     {V1,S2} = encode(V0, S1),
                     {{K1,V1},{I,S2}};
                 (V0, {I,S0}) ->
                     {V1,S1} = encode(V0, S0),
                     {{I,V1},{I+1,S1}}
             end,
    {Es,{_,St1}} = lists:mapfoldl(EncTab, {1,St0}, L),
    {T,St2} = luerl_heap:alloc_table(Es, St1),
    {T,St2};                                    %No more to do for now
encode(F, St) when is_function(F, 2) ->
    F1 = fun(Args, State) -> F(Args, State) end,
    %% io:format("enc ~p\n", [#erl_func{code=F1}]),
    {#erl_func{code=F1}, St};
encode(F, St) when is_function(F, 1) ->
    F1 = fun(Args, State) -> Res = F(Args), {Res,State} end,
    %% io:format("enc ~p\n", [#erl_func{code=F1}]),
    {#erl_func{code=F1}, St};
encode({M,F,A}, St) when is_atom(M) and is_atom(F) ->
    %% io:format("enc ~p\n", [#erl_mfa{m=M,f=F,a=A}]),
    {#erl_mfa{m=M,f=F,a=A}, St};
encode({userdata,Data}, St) ->
    luerl_heap:alloc_userdata(Data, St);
%% % Table refs should not be re-encoded
%% encode(#tref{}=T, St) ->
%%     case luerl_heap:chk_table(T, St) of
%%         ok -> {T, St};
%%         error -> error(badarg)
%%     end;
encode(Term, _) -> error({badarg,Term}).        %Can't encode anything else

%% decode_list([LuerlTerm], State) -> [Term].
%% decode(LuerlTerm, State) -> Term.
%%  In decode we track of which tables we have seen to detect
%%  recursive references and generate an error when that occurs.

decode_list(Lts, St) ->
    lists:map(fun (Lt) -> decode(Lt, St) end, Lts).

decode(LT, St) ->
    decode(LT, St, []).

decode(nil, _, _) -> nil;
decode(false, _, _) -> false;
decode(true, _, _) -> true;
decode(B, _, _) when is_binary(B) -> B;
decode(N, _, _) when is_number(N) -> N;         %Integers and floats
decode(#tref{}=T, St, In) ->
    decode_table(T, St, In);
decode(#usdref{}=U, St, In) ->
    decode_userdata(U, St, In);
decode(#funref{}=Fun, St, In) ->
    decode_luafunc(Fun, St, In);
decode(#erl_func{}=Fun, St, In) ->
    decode_erlfunc(Fun, St, In);
decode(#erl_mfa{}=Mfa, St, In) ->
    decode_erlmfa(Mfa, St, In);
decode(Lua, _, _) -> error({badarg,Lua}).       %Shouldn't have anything else

decode_table(#tref{i=N}=T, St, In0) ->
    case lists:member(N, In0) of
        true -> error({recursive_table,T});     %Been here before
        false ->
            In1 = [N|In0],                      %We are in this as well
            case luerl_heap:get_table(T, St) of
                #table{a=Arr,d=Dict} ->
                    Fun = fun (K, V, Acc) ->
                                  [{decode(K, St, In1),decode(V, St, In1)}|Acc]
                          end,
                    Ts = ttdict:fold(Fun, [], Dict),
                    array:sparse_foldr(Fun, Ts, Arr);
                _Undefined -> error(badarg)
            end
    end.

decode_userdata(U, St, _In) ->
    {#userdata{d=Data},_} = luerl_heap:get_userdata(U, St),
    {userdata,Data}.

decode_luafunc(Fun, _St, _In) ->
    %% io:format("dec ~p\n", [Fun]),
    fun(Args, State) ->
            luerl_emul:functioncall(Fun, Args, State)
    end.

decode_erlfunc(#erl_func{code=Fun}=_Ef, _St, _In) ->
    %% io:format("dec ~p\n", [Ef]),
    Fun.                                        %Just the bare fun

decode_erlmfa(#erl_mfa{m=Mod,f=Func,a=Arg}=_Mfa, _St, _In) ->
    %% io:format("mfa ~p\n", [Mfa]),
    {Mod,Func,Arg}.

%% Externalize and Internalize ensure that the VM state passed in
%% can be stored externally or can be recreated from external storage.
%% Currently very simple: only random state needs special treatment.

externalize(S) ->
    luerl_lib_math:externalize(S).

internalize(S) ->
    luerl_lib_math:internalize(S).

%% put_private(Key, Value, State) ->
%%   State.
%% get_private(Key, State) ->
%%   Value.
%% delete_private(Key, State) ->
%%   Value.
put_private(Key, Value, S) ->
    Private = maps:put(Key, Value, S#luerl.private),
    S#luerl{private=Private}.

get_private(Key, S) ->
    maps:get(Key, S#luerl.private).

delete_private(Key, S) ->
    Private = maps:remove(Key, S#luerl.private),
    S#luerl{private=Private}.
