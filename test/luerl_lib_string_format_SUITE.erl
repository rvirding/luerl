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
  format_float/1,
  format_integer/1,
  format_multiple_results/1,
  format_same_result_with_native_lua/1,
  format_unicode_usage/1,
  unicode_why_string_length_different_eval_vs_dofile/1

]).

all() -> [ {group, lib_string_format} ].

groups() ->
  [
    {lib_string_format, [parallel], [
        format_different_result_from_native_lua
      , format_different_result_from_native_lua2
      , format_multiple_results
      , format_same_result_with_native_lua
      , format_unicode_usage
      , unicode_why_string_length_different_eval_vs_dofile

      , format_integer
      , format_float

    ]}
  ].

format_integer(Config) ->
  Results = [
      <<"Preceding with blanks:       1977">>

    , <<"Preceding with zeros: 0000001977">>           % % ?ANY_BITS(Fl, ?FL_Z), F =/= none

    , <<"To wield the %s you need to be level %i">>
    , <<"123">>
    , <<" 01">> % integer formatting with precision
    , <<"02">> % integer with string param
    , <<"  3">> % integer without precision
    , <<" +04">> % flag + positive num
    , <<" -04">> % flag + negative num
    , <<"  +05">> % space flag
    , <<"-100, -100">>
    , <<"+100, -100">>                                % ?ANY_BITS(Fl, ?FL_M)
  ],
  Tests = [ {"format_integer.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

format_float(Config) ->
  Results = [
      <<"3.141593">>
    , <<"floats: +1.230000 -4.560000">>   % if ?ANY_BITS(Fl, ?FL_M)
    , <<"1.2346">>
    , <<"000001.235">>
  ],
  Tests = [ {"format_float.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).
  %ok.

% source: http://lua-users.org/wiki/StringLibraryTutorial,
% string.format() part
format_same_result_with_native_lua(Config) ->
  Results = [
    <<"Hello \"Lua user!\"">>
    , <<"Lua">>
    , <<"?">> % char should be between 0-255, now it's bigger than 255
    , <<"\"a string with \\\"quotes\\\" and \\\n new line\"">>
    , <<"parameter">>
    , <<"abcd">>
    , <<"###x    ###">>
    , <<"A">>
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
    , <<"0">> % 0 converted to octal is 0
    , <<"ApplE">>
    , <<"no_more_test_sign_because_lua_multiple_result_return">>
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
      <<"3.141593e+000, 3.1416e+000">>   % e_float_precision(P) -> P+1
    , <<"3.14159, 3.14">> % g_float_precision(P) -> P

    % Lua:
    % > string.format("Some different radices: %d %x %o %#x %#o \n", 100, 100, 100, 100, 100)
    % Some different radices: 100 64 144 0x64 0144
    % Luerl, missing 0x, 0 at 64, 144:
    % Some different radices: 100 64 144 64 144
    , <<"Some different radices: 100 64 144 0x64 0144">>
    , <<"15.656">>
    , <<"15.656">>
    , <<"string.format('%F', 3.3) doesn't work in native Lua.">>
    , <<"4.">>
    , <<"6.E+00">>

    , 65  % string.byte("ABCDE")
    , 66
    , 67
    , 68

  ],
  Tests = [ {"format_different_result_from_native_lua.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).


format_unicode_usage(Config) ->
  Results = [
      22.0
    , 31.0
    , <<"arvizturo tukorfurog_p">>
    , 15.0
    , <<"alom">>
    , true
    , <<"no_more_test">>

  ],
  Tests = [ {"format_unicode_usage.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).


% FIXME: Luerl give back 2 different values for the same lua code
% after the parsing of .lua file, the return value is 31.0
% with luerl:eval return value == 22.
% WHY ARE THEY DIFFERENT?? 31 vs 22 ???
% eval should give back the same value than file parsing/executing
unicode_why_string_length_different_eval_vs_dofile(Config)->
  {ok, [TxtLenEval]} = luerl:eval(<<"return string.len('árvíztűrő tükörfúrógép')">>),
  % TxtLenEval == 22 here

  Results = [ TxtLenEval ],
  Tests = [ {"unicode_why_string_length_different.lua", Results} ],
  luerl_test_common:run_tests(Config, Tests).

