%% Copyright (c) 2013-2017 Robert Virding
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

-export([init/0,init/1,init/2,
         run/1,run/2,run/3,run/4,run/5]).

-define(LUERL_GLOBAL, '_G').
-define(SANDBOXED_VALUE, sandboxed).
-define(SANDBOXED_GLOBALS, [
        [?LUERL_GLOBAL, io],
        [?LUERL_GLOBAL, file],
        [?LUERL_GLOBAL, os, getenv],
        [?LUERL_GLOBAL, package],
        [?LUERL_GLOBAL, load],
        [?LUERL_GLOBAL, loadfile],
        [?LUERL_GLOBAL, require],
        [?LUERL_GLOBAL, dofile],
        [?LUERL_GLOBAL, load],
        [?LUERL_GLOBAL, loadfile],
        [?LUERL_GLOBAL, loadstring]
    ]).
-define(TIMEOUT, 100).

%% init([, State|TablePaths[, TablePaths]]) -> State
init() ->
  init(luerl:init()).

init(TablePaths) when is_list(TablePaths) ->
  init(luerl:init(), TablePaths);
init(St) ->
  init(St, ?SANDBOXED_GLOBALS).


init(St, []) -> luerl:gc(St);
init(St0, [Path|Tail]) ->
  St1 = luerl:set_table(Path, ?SANDBOXED_VALUE, St0),
  init(St1, Tail).

%% run(String|Binary|Form[, State[, MaxReductions|Flags[, Flags[, Timeout]]]]) -> {Term,State}|{error,Term}
run(S) ->
  run(S, init()).

run(S, St) ->
    run(S, St, 0, []).

run(S, St, MaxR) when is_integer(MaxR) ->
    run(S, St, MaxR, []);
run(S, St, Flags) when is_list(Flags) ->
    run(S, St, 0, Flags).

run(S, St, MaxR, Flags) ->
    run(S, St, MaxR, Flags, ?TIMEOUT).

run(S, St, 0, Flags, Timeout) ->
    Runner = start(self(), S, St, Flags),
    receive_response(Runner, Timeout);
run(S, St, MaxR, Flags, Timeout) ->
    Runner = start(self(), S, St, Flags),
    case wait_reductions(Runner, MaxR) of
        {killed, R} -> {error, {reductions, R}};
        ok -> receive_response(Runner, Timeout)
    end.

start(Parent, S, St, Flags) ->
    spawn_opt(fun() ->
        try
            Reply = luerl:do(S, St),
            erlang:send(Parent, {self(), Reply})
        catch
            error:Reason ->
                erlang:send(Parent, {self(), {error, Reason}})
        end
     end, Flags).

wait_reductions(Runner, MaxR) ->
    case process_info(Runner, reductions) of
        undefined ->
            ok;
        {reductions, R} when R >= MaxR ->
            exit(Runner, kill),
            {killed, R};
        {reductions, _} ->
            wait_reductions(Runner, MaxR)
    end.

receive_response(Runner, Timeout) ->
    receive
        {Runner, Reply} -> Reply;
        {error, Error} -> Error
    after
      Timeout ->
        {error, timeout}
    end.
