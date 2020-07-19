%% Copyright (c) 2019 Ferenc Boroczki
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

-module(luerl_return_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0]).
-export([simple_return/1, fun_return/1]).

all() ->
  [
    {group, return}
  ].

groups() ->
  [
    {return, [parallel], [simple_return, fun_return]}
  ].

simple_return(Config) ->
  Tests = [
    {"simple_return_1.lua", [1]},
    {"simple_return_multi.lua", [1, <<"string 2">>, 3.4]}
  ],
  run_tests(Config, Tests).

fun_return(Config) ->
  run_and_check(Config, "fun_return_multi.lua", [7, <<"str 1">>, 5.5, 11.0]).

run_tests(Config, Tests) ->
  [run_and_check(Config, Script, Expected) || {Script, Expected} <- Tests].

run_and_check(Config, Script, Expected) ->
  DataDir = ?config(data_dir, Config),
  ScriptFile = DataDir ++ Script,
  {Result, _St} = luerl:dofile(ScriptFile, luerl:init()),
  Expected = Result.
