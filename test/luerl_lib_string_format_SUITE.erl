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
  format_same_result_with_native_lua/1
]).

all() -> [ {group, lib_string_format} ].

groups() ->
  [
    {lib_string_format, [parallel], [
      format_different_result_from_native_lua,
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
  ],
  Tests = [ {"format.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

format_different_result_from_native_lua(Config) ->
  Results = [
      <<"3.141593e+000, 3.141593E+000">>
    , <<"3.14159, 1e+09">> % luerl: <<"3.14159, 1.00000e+9">>
    , <<"-100, -100, 18446744073709551516">> % luerl: badarg format
    , <<"ApplE">>, 1  % luerl: 1.0, Lua: 1 in return value

    % Lua:
    % > string.format("Some different radices: %d %x %o %#x %#o \n", 100, 100, 100, 100, 100)
    % Some different radices: 100 64 144 0x64 0144
    % Luerl, missing 0x, 0 at 64, 144:
    % Some different radices: 100 64 144 64 144
    , <<"Some different radices: 100 64 144 0x64 0144">>
    , <<"15.656">>
    , <<"4.">>
],
  Tests = [ {"format_different_result_from_native_lua.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

