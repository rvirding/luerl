%% Copyright (c) 2013-2025 Robert Virding
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

%% File    : luerl_sandbox.erl
%% Authors : Tyler Butchart
%% Purpose : Reduction limiting luerl sandbox.

-module(luerl_sandbox).

-include("luerl.hrl").

?MODULEDOC( """
Sandboxed execution environment for Luerl with reduction counting.
This module provides a way to run Lua code with controlled execution,
limiting the number of reductions (computational steps) to prevent
excessive resource consumption.

The `ControlFlags` is a map or keyword list which can contain the
following fields
```
    #{max_time => MaxTime,              % 100 msecs
      max_reductions => MaxReds,        % none
      spawn_opts => SpawnOpts}          % []
```
`MaxReds` limits the number of reductions (default no max) and
`MaxTime` (default 100 msecs) limits the maximum time to run the
string. `SpawnOpts` are spawn options to the process running the
evaluation. The default values are shown as comments. Any other fields
are ignored.
""").

-export([init/0,init/1,init/2,
         run/1,run/2,run/3,run/4,run/5]).

-define(LUERL_GLOBAL, '_G').
-define(SANDBOXED_VALUE, sandboxed).
-define(SANDBOXED_GLOBALS, [
        [?LUERL_GLOBAL, io],
        [?LUERL_GLOBAL, file],
        [?LUERL_GLOBAL, os, execute],
        [?LUERL_GLOBAL, os, exit],
        [?LUERL_GLOBAL, os, getenv],
        [?LUERL_GLOBAL, os, remove],
        [?LUERL_GLOBAL, os, rename],
        [?LUERL_GLOBAL, os, tmpname],
        [?LUERL_GLOBAL, package],
        [?LUERL_GLOBAL, load],
        [?LUERL_GLOBAL, loadfile],
        [?LUERL_GLOBAL, require],
        [?LUERL_GLOBAL, dofile],
        [?LUERL_GLOBAL, load],
        [?LUERL_GLOBAL, loadfile],
        [?LUERL_GLOBAL, loadstring]
    ]).

%% The controlflags for controlling the execition.

-type controlflags() :: #{atom() := term()} | [{atom(),term()}].

-type sandboxtable() :: [[atom()]].

-define(MAX_TIME, 100).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

?DOC( """
Create a new Luerl state with the standard sandboxing.
""").

-spec init() -> LuaState when
      LuaState :: luerlstate().

init() ->
  init(luerl:init()).

%% init([, State|TablePaths[, TablePaths]]) -> State

?DOC( """
Take an existing Luerl state and run the default sandboxing on it. Or
create a new Luerl state and run a sandboxing table on it.
""").

-spec init(LuaState) -> LuaState when LuaState :: luerlstate() ;
          (TablePaths) -> LuaState when
      LuaState :: luerlstate(),
      TablePaths :: sandboxtable().

init(TablePaths) when is_list(TablePaths) ->
  init(luerl:init(), TablePaths);
init(St) ->
  init(St, ?SANDBOXED_GLOBALS).

?DOC( """
Take an existing Luerl state and run the `TablePaths` on it to control the sandboxing.
""").

-spec init(LuaState, TablePaths) -> LuaState when
      LuaState :: luerlstate(),
      TablePaths :: [[atom()]].

init(St, []) -> luerl:gc(St);
init(St0, [Path|Tail]) ->
  {ok,St1} = luerl:set_table_keys_dec(Path, ?SANDBOXED_VALUE, St0),
  init(St1, Tail).

%% The default flags for running the sandboxed process.
default_flags() ->
    [{max_time, ?MAX_TIME},
     {max_reductions, none},
     {spawn_opts, []}].

%% run(String|Binary) -> {Term,State} | {error,Term}.
%% run(String|Binary, State) -> {Term,State} | {error,Term}.
%% run(String|Binary, Flags, State) -> {Term,State} | {error,Term}.
%%  The new interface.

?DOC( """
Run the Lua expression controlled by the default `ControlFlags` in a
new `LuaState` with the default sandboxing.
""").

-spec run(Expression) -> {Reply,LuaState} when
      Expression :: string(),
      LuaState :: luerlstate(),
      Reply :: {ok,Result,LuaState} | Error,
      Result :: luerldata(),
      Error :: term().

run(S) ->
  run(S, init()).

?DOC( """
Run the Lua expression controlled by the default `ControlFlags` in the
`LuaState`.
""").

-spec run(Expression, LuaState) -> {Reply,LuaState} when
      Expression :: string(),
      LuaState :: luerlstate(),
      Reply :: {ok,Result,LuaState} | Error,
      Result :: luerldata(),
      Error :: term().

run(S, St) ->
   do_run(S, default_flags(), St).

?DOC( """
Run the Lua expression controlled by the `ControlFlags` in the `LuaState`.
""").

-spec run(Expression, ControlFlags, LuaState) -> {Reply,LuaState} when
      Expression :: string(),
      ControlFlags :: controlflags(),
      LuaState :: luerlstate(),
      Reply :: {ok,Result,LuaState} | Error,
      Result :: luerldata(),
      Error :: term().

%% The new interface.
run(S, Flags, St) when ?IS_MAP(Flags) ->
    run(S, maps:to_list(Flags), St);
run(S, Flags, #luerl{}=St) when is_list(Flags) ->
    do_run(S, Flags ++ default_flags(), St);

%% The old interface.
run(S, St, MaxR) when is_integer(MaxR) ->
    run(S, St, MaxR, []);
run(S, St, Flags) when is_list(Flags) ->
    run(S, St, 0, Flags).

%% run(String|Binary|Form[, State[, MaxReductions|Flags[, Flags[, Timeout]]]]) -> {Term,State}|{error,Term}
%%  This is the old interface which still works.

?DOC( """
run(String, LuaState, MaxReds, SpawnOpts)
""").
?DOC( #{deprecated => "Use `run/3`" } ).

run(S, St, MaxR, Flags) ->
    run(S, St, MaxR, Flags, ?MAX_TIME).

?DOC( """
run(String, LuaState, MaxReds, SpawnOpts, Timeout)
""").
?DOC( #{deprecated => "Use `run/3`" } ).

run(S, St, 0, Opts, MaxT) ->
    %% Need to get the old no reductions to the new no reductions.
    run(S, St, none, Opts, MaxT);
run(S, St, MaxR, Opts, MaxT) ->
    Flags = [{max_time,MaxT},{max_reductions,MaxR},{spawn_opts,Opts}],
    do_run(S, Flags, St).

do_run(S, Flags, St) ->
    MaxT = proplists:get_value(max_time, Flags),
    Opts = proplists:get_value(spawn_opts, Flags),
    Runner = start(self(), S, Opts, St),
    case proplists:get_value(max_reductions, Flags) of
        none ->
            receive_response(Runner, MaxT);
        MaxR when is_integer(MaxR), MaxR > 0 ->
            case wait_reductions(Runner, MaxR) of
                {killed, R} ->
                    {error, {reductions, R}};
                ok ->
                    receive_response(Runner, MaxT)
            end;
        _Other ->
            exit(badarg)
    end.

start(Parent, S, Opts, St) ->
    spawn_opt(fun() ->
        try
            Reply = luerl:do(S, St),
            erlang:send(Parent, {self(), Reply})
        catch
            error:Reason ->
                erlang:send(Parent, {self(), {error, Reason}})
        end
     end, Opts).

wait_reductions(Runner, MaxR) ->
    case process_info(Runner, reductions) of
        undefined ->
            %% The process has died.
            ok;
        {reductions, R} when R >= MaxR ->
            exit(Runner, kill),
            {killed, R};
        {reductions, _} ->
	    %% We only check every default MAX_TIME so we don't
	    %% overload the runner process too much.
	    receive after ?MAX_TIME -> ok end,
            wait_reductions(Runner, MaxR)
    end.

receive_response(Runner, Timeout) ->
    receive
        {Runner, Reply} ->
            %% The runner has terminated.
            Reply;
        {error, Error} -> Error
    after
        Timeout ->
            %% Kill the runner as its time is up.
            exit(Runner, kill),
            {error, timeout}
    end.
