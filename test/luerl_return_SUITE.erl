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

-export([simple_return/1, fun_return/1, variable_args/1, check_unicode/1, table_tests/1]).

all() ->
  [
    {group, return}
  ].

groups() ->
  [
    {return, [parallel], [simple_return, fun_return, variable_args, check_unicode, table_tests]}
  ].

simple_return(Config) ->
  Tests = [
    {"simple_return_1.lua", [1]},
    {"simple_return_multi.lua", [1, <<"string 2">>, 3.4]}
  ],
  run_tests(Config, Tests).

fun_return(Config) ->
  run_and_check(Config, "fun_return_multi.lua", [7, <<"str 1">>, 5.5, 11.0]).

variable_args(Config) ->
  run_tests(Config, [
    {"variable_args_1.lua", [99, 88, 77]},
    {"variable_args_multi.lua", [9, <<"banana">>, 8]}
  ]).

check_unicode(Config) ->
  St = run_and_check(Config, "check_unicode.lua", []),
  check_unicode_call_fun(<<"árvíztűrő tükörfúrógép"/utf8>>, 31, check_hun, St),
  check_unicode_call_fun(<<"λ"/utf8>>, 2, check_lambda, St),
  check_unicode_call_fun(<<9810/utf8>>, 3, check_aquarius, St).

check_unicode_call_fun(Input, Length, LuaFun, St) ->
  {ok, [Input, Input, true, Length, Length], _} =
        luerl:call_function_dec([LuaFun], [Input], St).

table_tests(Config) ->
  run_and_check(Config, "table_indexed_table.lua", [111, 222, 333]).


run_tests(Config, Tests) ->
  [run_and_check(Config, Script, Expected) || {Script, Expected} <- Tests].

run_and_check(Config, Script, Expected) ->
  DataDir = ?config(data_dir, Config),
  ScriptFile = DataDir ++ Script,
  {ok, Result, St} = luerl:dofile(ScriptFile, luerl:init()),
  Expected = Result,
  St.
