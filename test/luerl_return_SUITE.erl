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

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([simple_return/1, fun_return/1, use_lib/1]).

init_per_suite(Config) ->
  DataDir = ?config(data_dir, Config),
  os:putenv("LUA_PATH", DataDir ++ "?.lua;" ++ DataDir ++ "?/init.lua"),
  Config.

end_per_suite(Config) ->
  Config.

all() ->
  [
    {group, return}
  ].

groups() ->
  [
    {return, [parallel], [simple_return, fun_return, use_lib]}
  ].

simple_return(Config) ->
  Tests = [
    {"simple_return_1.lua", [1]},
    {"simple_return_multi.lua", [1, <<"string 2">>, 3.4]}
  ],
  run_tests(Config, Tests).

fun_return(Config) ->
  run_and_check(Config, "fun_return_multi.lua", [7, <<"str 1">>, 5.5, 11.0]).

use_lib(Config) ->
  LuaDecimal = fun(B, E) -> [{<<"b">>, B}, {<<"e">>, E}] end,
  Expected = [LuaDecimal(B, E) || {B, E} <- [{13, 1}, {7, 1}, {3, 3}]],
  run_and_check(Config, "decimal_test.lua", Expected).

run_tests(Config, Tests) ->
  [run_and_check(Config, Script, Expected) || {Script, Expected} <- Tests].

run_and_check(Config, Script, Expected) ->
  DataDir = ?config(data_dir, Config),
  ScriptFile = DataDir ++ Script,
  {ok, Result} = luerl:evalfile(ScriptFile),
  {true, {expected, Expected}, {result, Result}} =
    {Result =:= Expected, {expected, Expected}, {result, Result}}.
