%% Copyright (c) 2020-2025 Robert Virding
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

%% File    : luerl.erl
%% Authors : Robert Virding
%% Purpose : The basic LUA 5.4 interface.

-module(luerl).

-include("luerl.hrl").

%% Use normal strings as it is easier to use " and make it readable
%% with both old and new string parsing.

?MODULEDOC(
"Luerl is an implementation of Lua 5.3 written in Erlang.
This is the main public API module for interfacing with Luerl.

The `LuaState` parameter is the state of a Lua VM instance. It must be
created with the `init/0` call and be threaded from one call to the
next.

Note that Luerl, following Lua, does do any implicit UTF-8 encoding of
input strings. This means that all strings given as arguments to the
calls or the strings to evaluate with `do/3` or `do_dec/3` need to
have already been UTF-8 encoded. This can be quite easily do with the
`~` or `~b` sigils. For example

`luerl:do(~b\"return 'árvíztűrő tükörfúrógép'\", St0)`

or

`luerl:do(~\"return 'árvíztűrő tükörfúrógép'\", St0)`").

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

?DOC( """
Create a new Lua state which is a fresh Lua VM instance.
""").

-spec init() -> LuaState when
      LuaState :: luerlstate().

init() ->
    luerl_emul:init().

?DOC( """
Runs the garbage collector on a state and returns the new state.
""").

-spec gc(LuaState) -> LuaState when
      LuaState :: luerlstate().

gc(St) ->
    luerl_heap:gc(St).

%% set_trace_func(TraceFunction, State) -> State.
%% clear_trace_func(State) -> State.
%% get_trace_data(State) -> TraceData.
%% set_trace_data(TraceData, State) -> State.
%%  Set the trace function and access the trace data.

?DOC( """
Set the trace function.
""").
?DOC( #{group => <<"Trace Control functions">>} ).

-spec set_trace_func(Function, LuaState) -> LuaState when
      Function :: fun(),
      LuaState :: luerlstate().

set_trace_func(Tfunc, St) ->
    St#luerl{trace_func=Tfunc}.

?DOC( """
Clear the trace function.
""").
?DOC( #{group => <<"Trace Control functions">>} ).

-spec clear_trace_func(LuaState) -> LuaState when
      LuaState :: luerlstate().

clear_trace_func(St) ->
    St#luerl{trace_func=none}.

?DOC( """
Get the current trace data.
""").
?DOC( #{group => <<"Trace Control functions">>} ).

-spec get_trace_data(LuaState) -> TraceData when
      LuaState :: luerlstate(),
      TraceData :: term().

get_trace_data(St) ->
    St#luerl.trace_data.

?DOC( """
Set the trace data.
""").
?DOC( #{group => <<"Trace Control functions">>} ).

-spec set_trace_data(TraceData, LuaState) -> LuaState when
      LuaState :: luerlstate(),
      TraceData :: term().

set_trace_data(Tdata, St) ->
    St#luerl{trace_data=Tdata}.

%% load(String|Binary, State) -> {ok,FuncRef,NewState}.
%% load(String|Binary, Options, State) -> {ok,FuncRef,NewState}.

?DOC( #{equiv => load(LuaChunk, [return], LuaState)} ).
?DOC( #{group => <<"Load Code functions">>} ).

-spec load(LuaCode, LuaState) -> {ok,Function,LuaState} | CompileError when
      LuaCode :: binary() | string(),
      Function :: #funref{},
      LuaState :: luerlstate(),
      CompileError :: {error,[term()],[term()]}.

load(Bin, St) -> load(Bin, [return], St).

?DOC( """
Parse Lua chunk code as string or binary, and return a compiled chunk function.
""").
?DOC( #{group => <<"Load Code functions">>} ).

-spec load(LuaCode, CompileOptions, LuaState) -> {ok,Function,LuaState} | CompileError when
      LuaCode :: binary() | string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Function :: #funref{},
      CompileError :: {error,[term()],[term()]}.

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

?DOC( #{equiv => loadfile(FileName, [return], LuaState)}).
?DOC( #{group => <<"Load Code functions">>} ).

-spec loadfile(FileName, LuaState) -> {ok,Function,LuaState} | CompileError when
      FileName :: string(),
      LuaState :: luerlstate(),
      Function :: #funref{},
      CompileError :: {error,[term()],[term()]}.

loadfile(Name, St) -> loadfile(Name, [return], St).

?DOC( """
Parse a Lua file, and return a compiled chunk function.
""").
?DOC( #{group => <<"Load Code functions">>} ).

-spec loadfile(FileName, CompileOptions, LuaState) -> {ok,Function,LuaState} | CompileError when
      FileName :: string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Function :: #funref{},
      CompileError :: {error,[term()],[term()]}.

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

?DOC( """
Calls `path_loadfile/4` with Path set the value of `LUA_LOAD_PATH` and
`CompileOptions` set to `[return]`.
""").
?DOC( #{group => <<"Load Code functions">>} ).

-spec path_loadfile(FileName, LuaState) ->
          {ok,Function,FullName,LuaState} | CompileError when
      FileName :: string(),
      LuaState :: luerlstate(),
      Function :: #funref{},
      FullName :: string(),
      CompileError :: {error,[term()],term()}.

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

?DOC( #{equiv => path_loadfile(Path, FileName, [return], LuaState)}).
?DOC( #{group => <<"Load Code functions">>} ).

-spec path_loadfile(Path, FileName, LuaState) -> {ok,Function,FullName,LuaState} | CompileError when
      Path :: [string()],
      FileName :: string(),
      LuaState :: luerlstate(),
      Function :: #funref{},
      FullName :: string(),
      CompileError :: {error,[term()],[term()]}.

path_loadfile(Dirs, Name, St) ->
    path_loadfile(Dirs, Name, [return], St).

?DOC( """
Search down a `Path` to find the Lua file and return a compiled ('form').
""").
?DOC( #{group => <<"Load Code functions">>} ).

-spec path_loadfile(Path, FileName, CompileOptions, LuaState) -> {ok,Function,FullName,LuaState} | CompileError when
      Path :: list(string()),
      FileName :: string(),
      FullName :: string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Function :: #funref{},
      CompileError :: {error,[term()],[term()]}.

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

%% load_module(KeyPath, ErlangModule, State) -> State.
%% load_module_dec(DecodedTablePath, ModuleName, State) -> State.

?DOC( """
Load `ErlangModule` and install its table at `KeyPath` in the LuaTable
which is **NOT** automatically Lua encoded.
""").
?DOC( #{group => <<"Load Code functions">>} ).

-spec load_module(KeyPath, ErlangModule, LuaState) -> LuaState when
      KeyPath :: [luerldata()],
      ErlangModule :: atom(),
      LuaState :: luerlstate().

load_module([_|_] = Lfp, Mod, St0) ->
    {Tab,St1} = Mod:install(St0),
    luerl_emul:set_table_keys(Lfp, Tab, St1);
load_module(_, _, _) ->
    error(badarg).

?DOC( """
Load `ErlangModule` and install its table at `KeyPath` in the LuaTable
which is automatically Lua encoded.
""").
?DOC( #{group => <<"Load Code functions">>} ).

-spec load_module_dec(KeyPath, ErlangModule, LuaState) -> LuaState when
      KeyPath :: [term()],
      ErlangModule :: atom(),
      LuaState :: luerlstate().

load_module_dec([_|_] = Dfp, Mod, St0) ->
    {Efp,St1} = encode_list(Dfp, St0),
    load_module(Efp, Mod, St1);
load_module_dec(_, _, _) ->
    error(badarg).

%% luerl:do(String|Binary|Form, State) ->
%% luerl:do(String|Binary|Form, CompileOptions, State) ->
%%     {ok,Result,NewState} | {lua_error,Error,State}.

?DOC( #{equiv => do(Expression, [return], LuaState)} ).
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec do(Expression, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      Expression :: string(),
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

do(S, St) -> do(S, [return], St).

?DOC( """
Compile a Lua expression string, evaluate it and return its result, which is
**NOT** decoded, and the new Lua State.
""").
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec do(Expression, CompileOptions, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      Expression :: string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

do(S, Opts, St0) ->
    case load(S, Opts, St0) of
        {ok,FuncRef,St1} ->
            call_function(FuncRef, [], St1);
        Error -> Error
    end.

?DOC( #{equiv => do_dec(Expression, [return], LuaState)} ).
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec do_dec(Expression, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      Expression :: string(),
      Result :: [term()],
      LuaState :: luerlstate(),
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

do_dec(S, St) ->
    do_dec(S, [return], St).

?DOC( """
Compile a Lua expression string, evaluate it and return its result, which is
is decoded, and the new Lua State.
""").
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec do_dec(Expression, CompileOptions, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      Expression :: string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Result :: [term()],
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

do_dec(S, Opts, St0) ->
    case do(S, Opts, St0) of
        {ok,Eret,St1} ->
            {ok,decode_list(Eret, St1),St1};
        Error -> Error
    end.

%% luerl:dofile(FileName, State) ->
%% luerl:dofile(FileName, CompileOptions, State) ->
%%     {ok,Result,NewState} | {lua_error,Error,State}.

?DOC( #{equiv => dofile(FileName, [return], LuaState)} ).
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec dofile(FileName, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      FileName :: string(),
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

dofile(File, St) -> dofile(File, [], St).

?DOC( """
Load and execute the Lua code in the file and return its result which
is **NOT** decoded, and the new Lua State. Equivalent to doing
luerl:do("return dofile('FileName')").
""").
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec dofile(FileName, CompileOptions, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      FileName :: string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

dofile(File, Opts, St0) ->
    case loadfile(File, Opts, St0) of
        {ok,Func,St1} ->
            call_function(Func, [], St1);
        Error -> Error
    end.

?DOC( #{equiv => dofile_dec(FileName, [return], LuaState)} ).
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec dofile_dec(FileName, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      FileName :: string(),
      Result :: [term()],
      LuaState :: luerlstate(),
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

dofile_dec(File, St) ->
    dofile_dec(File, [], St).

?DOC( """
Load and execute the Lua code in the file and return its result which
is Lua  decoded, and the new Lua State. Equivalent to doing
luerl:do_dec("return dofile('FileName')").
""").
?DOC( #{group => <<"Evaluate Code functions">>} ).

-spec dofile_dec(FileName, CompileOptions, LuaState) -> {ok,Result,LuaState} | LuaError | CompileError when
      FileName :: string(),
      CompileOptions :: [term()],
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState},
      CompileError :: {error,[term()],[term()]}.

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

?DOC( #{equiv => call_function(LuaFuncRef, Args, LuaState)} ).
?DOC( #{group => <<"Function/Method Call functions">>} ).

call(LuaFuncRef, Args, LuaState) ->
    call_function(LuaFuncRef, Args, LuaState).

?DOC( #{equiv => call_function(LuaFuncRef, [], LuaState)} ).
?DOC( #{group => <<"Function/Method Call functions">>} ).

call_chunk(LuaFuncRef, LuaState) ->
    call_function(LuaFuncRef, [], LuaState).

?DOC( #{equiv => call_function(LuaFuncRef, Args, LuaState)} ).
?DOC( #{group => <<"Function/Method Call functions">>} ).

call_chunk(LuaFuncRef, Args, LuaState) ->
    call_function(LuaFuncRef, Args, LuaState).

%% call_function(LuaFuncRef | LuaTablePath, Args, State) ->
%%     {ok,LuaReturn,State} | {lua_error,Error,State}.

?DOC( """
Call a function already defined in the state. `LuaFuncReaf` and `Args`
are **NOT** automatically encoded and `Result` is **NOT**
automatically decoded.
""").
?DOC( #{group => <<"Function/Method Call functions">>} ).

-spec call_function(LuaFuncRef, Args, LuaState) -> {ok,Result,LuaState} | LuaError when
      LuaFuncRef :: [luerldata()] | luerldata(),
      Args :: [luerldata()],
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState}.

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

?DOC( """
Call a function already defined in the state. `KeyPath` is a list of
keys to the function. `KeyPath` and `Args` are automatically encoded,
while `Result` is **NOT** automatically decoded.
""").
?DOC( #{group => <<"Function/Method Call functions">>} ).

-spec call_function_enc(KeyPath, Args, LuaState) -> {ok,Result,LuaState} | LuaError when
      KeyPath :: [term()],
      Args :: [term()],
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState}.

call_function_enc(Dtpath, Dargs, St0) ->
    {Epath,St1} = encode_list(Dtpath, St0),
    {Eargs,St2} = encode_list(Dargs, St1),
    call_function(Epath, Eargs, St2).

%% call_function_dec(DecodedFuncRef, Args, State) ->
%%     {ok,DecodedReturn,State} | {lua_error,Error,State}.

?DOC( """
Call a function already defined in the state. `KeyPath` is a list of keys to the function. `KeyPath` and `Args` are automatically encoded, while `Result` is automatically decoded.
""").
?DOC( #{group => <<"Function/Method Call functions">>} ).

-spec call_function_dec(KeyPath, Args, LuaState) -> {ok,Result,LuaState} | LuaError when
      KeyPath :: [term()],
      Args :: [term()],
      LuaState :: luerlstate(),
      Result :: [term()],
      LuaError :: {lua_error,term(),LuaState}.

call_function_dec(Dtpath, Dargs, St0) ->
    case call_function_enc(Dtpath, Dargs, St0) of
        {ok,Eret,St1} ->
            {ok,decode_list(Eret, St1),St1};
        LuaError -> LuaError
    end.

%% call_method(LuaObject, Method, Args, State) ->
%%     {ok,Return,State} | {lua_error,Error,State}.
%% call_method_dec(DecodedObject, Method, Args, State) ->
%%     {ok,DecodedReturn,State} | {lua_error,Error,State}.

?DOC( """
Call the `Method` in the `LuaTable` with the `Args`. The `LuaTable`,
`Method` and `Args` are **NOT** automatically encoded and the `Result`
is **NOT** automatically decoded
""").
?DOC( #{group => <<"Function/Method Call functions">>} ).

-spec call_method(LuaTable, Method, Args, LuaState) -> {ok,Result,LuaState} | LuaError when
      LuaTable :: #tref{},
      Method :: luerldata(),
      Args :: [luerldata()],
      LuaState :: luerlstate(),
      Result :: [luerldata()],
      LuaError :: {lua_error,term(),LuaState}.

call_method(Obj, Meth, Args, St0) ->
    try
        {Ret,St1} = luerl_emul:methodcall(Obj, Meth, Args, St0),
        {ok,Ret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

?DOC( """
Call the `Method` in the `Table` with the `Args`. The `Table`,
`Method` and `Args` are automatically encoded and the `Result`
is  automatically decoded.
""").
?DOC( #{group => <<"Function/Method Call functions">>} ).

-spec call_method_dec(KeyPath, Method, Args, LuaState) -> {ok,Result,LuaState} | LuaError when
      KeyPath :: [term()],
      Method :: term(),
      Args :: [term()],
      LuaState :: luerlstate(),
      Result :: [term()],
      LuaError :: {lua_error,term(),LuaState}.

?DOC( #{group => <<"Function/Method Call functions">>} ).

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

?DOC( """
Gets a value inside the Lua state. `KeyPath` is **NOT** encoded and
`Result` is **NOT** decoded.
""").
?DOC( #{group => <<"Lua Table Access functions">>} ).

-spec get_table_keys(KeyPath, LuaState) -> {ok,Result,LuaState} | LuaError when
      KeyPath :: [luerldata()],
      LuaState :: luerlstate(),
      Result :: luerldata(),
      LuaError :: {lua_error,term(),LuaState}.

get_table_keys(Keys, St0) ->
    try
        {Eret,St1} = luerl_emul:get_table_keys(Keys, St0),
        {ok,Eret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

?DOC( """
Gets a value inside the Lua state. `KeyPath` is automatically encoded
and `Result` is automatically decoded.
""").
?DOC( #{group => <<"Lua Table Access functions">>} ).

-spec get_table_keys_dec(KeyPath, LuaState) -> {ok,Result,LuaState} | LuaError when
      KeyPath :: [term()],
      LuaState :: luerlstate(),
      Result :: term(),
      LuaError :: {lua_error,term(),LuaState}.

get_table_keys_dec(Dkeys, St0) ->
    {Ekeys,St1} = encode_list(Dkeys, St0),
    case get_table_keys(Ekeys, St1) of
        {ok,Eret,St2} ->
            {ok,decode(Eret, St2),St2};
        LuaError -> LuaError
    end.

?DOC( """
Sets a value inside the Lua state. `KeyPath` and `Value` are **NOT** encoded.
""").
?DOC( #{group => <<"Lua Table Access functions">>} ).

-spec set_table_keys(KeyPath, Value, LuaState) -> {ok,LuaState} | LuaError when
      KeyPath :: [luerldata()],
      Value :: luerldata(),
      LuaState :: luerlstate(),
      LuaError :: {lua_error,term(),LuaState}.

set_table_keys(Keys, Val, St0) ->
    try
        St1 = luerl_emul:set_table_keys(Keys, Val, St0),
        {ok,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

?DOC( """
Sets a value inside the Lua state. `KeyPath` and `Value` are
automatically encoded.
""").
?DOC( #{group => <<"Lua Table Access functions">>} ).

-spec set_table_keys_dec(KeyPath, Value, LuaState) -> {ok,LuaState} | LuaError when
      KeyPath :: [term()],
      Value :: term(),
      LuaState :: luerlstate(),
      LuaError :: {lua_error,term(),LuaState}.

set_table_keys_dec(Dkeys, Dval, St0) ->
    {Ekeys,St1} = encode_list(Dkeys, St0),
    {Eval,St2} = encode(Dval, St1),
    set_table_keys(Ekeys, Eval, St2).

%% get_table_key(Tab, Key, State) ->
%%     {ok,Value,State} | {lua_error,Error,State}.
%% set_table_key(Tab, Key, Value, State) ->
%%     {ok,State} | {lua_error,Error,State}.

?DOC( """
Get the value of a key in a table. `Table`, `Key` are
**NOT** encoded and the `Result` is **NOT** decoded.
""").
?DOC( #{group => <<"Lua Table Access functions">>} ).

-spec get_table_key(Table, Key, LuaState) -> {ok,Result,LuaState} | LuaError when
      Table :: luerldata(),
      Key :: luerldata(),
      LuaState :: luerlstate(),
      Result :: luerldata(),
      LuaError :: {lua_error,term(),LuaState}.

get_table_key(Tab, Key, St0) ->
    try
        {Eret,St1} = luerl_emul:get_table_key(Tab, Key, St0),
        {ok,Eret,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

?DOC( """
Set the value of a key in a table. `Table`, `Key` and `Value` are
**NOT** encoded.
""").
?DOC( #{group => <<"Lua Table Access functions">>} ).

-spec set_table_key(Table, Key, Value, LuaState) -> {ok,LuaState} | LuaError when
      Table :: luerldata(),
      Key :: luerldata(),
      Value :: luerldata(),
      LuaState :: luerlstate(),
      LuaError :: {lua_error,term(),LuaState}.

set_table_key(Tab, Key, Val, St0) ->
    try
        St1 = luerl_emul:set_table_key(Tab, Key, Val, St0),
        {ok,St1}
    catch
        error:{lua_error,_E,_St} = LuaErr ->
            LuaErr
    end.

%% get_stacktrace(State) -> [{FuncName,[{file,FileName},{line,Line}]}].

?DOC( """
Return a stack trace of the current call stack in the state.
""").

-spec get_stacktrace(LuaState) -> [FuncCall] when
      LuaState :: luerlstate(),
      FuncCall :: {FuncName,CallArgs,ExtraInfo},
      FuncName :: atom(),
      ExtraInfo :: [{atom(),term()}],
      CallArgs :: [term()].

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

?DOC( """
Encode a list of Erlang terms into their Luerl representations if possible.
""").
?DOC( #{ group => <<"Encode/Decode Data functions">>} ).

-spec encode_list([Term], LuaState) -> {[LuerlTerm],LuaState} when
      LuerlTerm :: term(),
      Term :: term(),
      LuaState :: luerlstate().

encode_list(Terms, LuaState) ->
    lists:mapfoldl(fun encode/2, LuaState, Terms).

?DOC( """
encode(Term, LuaState)

Encode an Erlang term into its Luerl representation if possible.
""").
?DOC( #{ group => <<"Encode/Decode Data functions">>} ).

-spec encode(Term, LuaState) -> {LuerlTerm,LuaState} when
      LuerlTerm :: luerldata(),
      Term :: term(),
      LuaState :: luerlstate().

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

?DOC( """
Decode a list of Luerl terms into their standard Erlang
representation. Note that we have to detect recursive references and
generate an error when this occurs.
""").
?DOC( #{ group => <<"Encode/Decode Data functions">>} ).

-spec decode_list([LuerlTerm], LuaState) -> [Term] when
      LuerlTerm :: luerldata(),
      Term :: term(),
      LuaState :: luerlstate().

decode_list(LuerlTerms, LuaState) ->
    lists:map(fun (Lt) -> decode(Lt, LuaState) end, LuerlTerms).

?DOC( """
Decode a Luerl term into its standard Erlang representation. Note that
we have to detect recursive references and generate an error when this
occurs.
""").
?DOC( #{ group => <<"Encode/Decode Data functions">>} ).

-spec decode(LuerlTerm, LuaState) -> Term when
      LuerlTerm :: luerldata(),
      Term :: term(),
      LuaState :: luerlstate().

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

-spec externalize(LuaState) -> LuaState when
      LuaState :: luerlstate().

externalize(S) ->
    luerl_lib_math:externalize(S).

-spec internalize(LuaState) -> LuaState when
      LuaState :: luerlstate().

internalize(S) ->
    luerl_lib_math:internalize(S).

%% put_private(Key, Value, State) ->
%%   State.
%% get_private(Key, State) ->
%%   Value.
%% delete_private(Key, State) ->
%%   Value.

?DOC( """
Puts a private `Value` under `Key` that is not exposed to the runtime.
""").
?DOC( #{group => <<"Private Data functions">>} ).

-spec put_private(Key, Value, LuaState) -> LuaState when
      Key :: term(),
      Value :: term(),
      LuaState :: luerlstate().

put_private(Key, Value, St) ->
    Private = maps:put(Key, Value, St#luerl.private),
    St#luerl{private=Private}.

?DOC( """
Get the private value for `Key`.
""").
?DOC( #{group => <<"Private Data functions">>} ).

-spec get_private(Key, LuaState) -> Value when
      Key :: term(),
      Value :: term(),
      LuaState :: luerlstate().

get_private(Key, St) ->
    maps:get(Key, St#luerl.private).

?DOC( """
Delete the private value for `Key`.
""").
?DOC( #{group => <<"Private Data functions">>} ).

-spec delete_private(Key, LuaState) -> LuaState when
      Key :: term(),
      LuaState :: luerlstate().

delete_private(Key, St) ->
    Private = maps:remove(Key, St#luerl.private),
    St#luerl{private=Private}.
