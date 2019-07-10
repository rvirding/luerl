%% Copyright (c) 2019 Balazs Nyiro
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
-module(luerl_lib_string_format_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([]).

-export([all/0, groups/0]).
-export([
  format_different_result_from_native_lua/1,
  format_different_result_from_native_lua2/1,
  format_multiple_results/1,
  format_same_result_with_native_lua/1
]).

all() -> [ {group, lib_string_format} ].

groups() ->
  [
    {lib_string_format, [parallel], [
      format_different_result_from_native_lua,
      format_different_result_from_native_lua2,
      format_multiple_results,
      format_same_result_with_native_lua
    ]}
  ].

% source: http://lua-users.org/wiki/StringLibraryTutorial,
% string.format() part
format_same_result_with_native_lua(Config) ->
  Results = [
    <<"Hello \"Lua user!\"">>
    , <<"Lua">>
    , <<"3.141593">>
    , <<"\"a string with \\\"quotes\\\" and \\\n new line\"">>
    , <<"Preceding with blanks:       1977">>
    , <<"Preceding with zeros: 0000001977">>
    , <<"parameter">>
    , <<"floats: +1.230000 -4.560000">>
    , <<"1.2346">>
    , <<"abcd">>
    , <<"###x    ###">>
    , <<"To wield the %s you need to be level %i">>
    , <<"A">>
    , <<"123">>
    , <<"1520f">>
    , <<"1520F">>
    , nil  %ABCDE, 100. element is missing
    , <<"ABC">>
    , <<"">>
    , 1  % counter_ffs
    , nil % banana
    , nil % Lua after 8
    , 20
    , <<"Param is not a string">>
    , <<"14">> % octal formatting
    , <<" 01">> % integer formatting with precision
    , <<"02">> % integer with string param
    , <<"  3">> % integer without precision
    , <<" +04">> % flag + positive num
    , <<" -04">> % flag + negative num
    , <<"  +05">> % space flag
    , <<"0">> % 0 converted to octal is 0
    , <<"ApplE">>
    , <<"-100, -100">> % luerl: badarg format
  ],
  Tests = [ {"format.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

format_different_result_from_native_lua2(Config) ->
  Results = [
      <<"\\ backslash not displayed">>
    , <<"\\ 6">>
    , <<"0x1520f">> % 0x is missing from Luerl answer
    , <<"0X1520F">> % 0X is missing...
    , <<"017">> % convert to octal, luerl: 17, lua: 017
  ],
  Tests = [ {"format_different_from_native_lua2.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

format_multiple_results(Config) ->
  % LUERL return: [7.0,13.0,13.0],
  % sooner or later Robert is going to fix .0 at the end of integers,
  % to [7.0, 13.0, 13.0] -> [7, 13, 13]
  Results = [ 7, 13, 13],
  Tests = [ {"format_multiple_results.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).


format_different_result_from_native_lua(Config) ->
  Results = [
      <<"3.141593e+000, 3.141593E+000">>
    , <<"3.14159, 1e+09">> % luerl: <<"3.14159, 1.00000e+9">>

    % Lua:
    % > string.format("Some different radices: %d %x %o %#x %#o \n", 100, 100, 100, 100, 100)
    % Some different radices: 100 64 144 0x64 0144
    % Luerl, missing 0x, 0 at 64, 144:
    % Some different radices: 100 64 144 64 144
    , <<"Some different radices: 100 64 144 0x64 0144">>
    , <<"15.656">>
    , <<"4.">>

    , 65  % string.byte("ABCDE")
    , 66
    , 67
    , 68

],
  Tests = [ {"format_different_result_from_native_lua.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

