%% Copyright (c) 2026 Robert Virding
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

%% File    : luerl_str_gmatch_tests.erl
%% Purpose : Tests for string.gmatch/2.

-module(luerl_str_gmatch_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper: run Lua code and return decoded results.
eval(Code) ->
    St = luerl:init(),
    {ok, Res, _St1} = luerl:do(Code, St),
    Res.

%% Basic word iteration using %a+ pattern.
gmatch_words_test() ->
    Res = eval(<<"local t = {}\n"
		 "for w in string.gmatch(\"hello world foo\", \"%a+\") do\n"
		 "  t[#t+1] = w\n"
		 "end\n"
		 "return t[1], t[2], t[3]">>),
    ?assertEqual([<<"hello">>, <<"world">>, <<"foo">>], Res).

%% Digit extraction.
gmatch_digits_test() ->
    Res = eval(<<"local t = {}\n"
		 "for d in string.gmatch(\"x1y22z333\", \"%d+\") do\n"
		 "  t[#t+1] = d\n"
		 "end\n"
		 "return t[1], t[2], t[3]">>),
    ?assertEqual([<<"1">>, <<"22">>, <<"333">>], Res).

%% Multiple captures: key=value pairs.
gmatch_captures_test() ->
    Res = eval(<<"local keys, vals = {}, {}\n"
		 "for k, v in string.gmatch(\"a=1,b=2,c=3\", \"(%a+)=(%d+)\") do\n"
		 "  keys[#keys+1] = k\n"
		 "  vals[#vals+1] = v\n"
		 "end\n"
		 "return keys[1], vals[1], keys[2], vals[2], keys[3], vals[3]">>),
    ?assertEqual([<<"a">>, <<"1">>, <<"b">>, <<"2">>, <<"c">>, <<"3">>], Res).

%% No matches: iterator returns nil immediately.
gmatch_no_match_test() ->
    Res = eval(<<"local count = 0\n"
		 "for w in string.gmatch(\"12345\", \"%a+\") do\n"
		 "  count = count + 1\n"
		 "end\n"
		 "return count">>),
    ?assertEqual([0], Res).

%% Single character matches.
gmatch_single_chars_test() ->
    Res = eval(<<"local t = {}\n"
		 "for c in string.gmatch(\"abc\", \".\") do\n"
		 "  t[#t+1] = c\n"
		 "end\n"
		 "return t[1], t[2], t[3], #t">>),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>, 3], Res).

%% Empty string: no iterations.
gmatch_empty_string_test() ->
    Res = eval(<<"local count = 0\n"
		 "for w in string.gmatch(\"\", \"%a+\") do\n"
		 "  count = count + 1\n"
		 "end\n"
		 "return count">>),
    ?assertEqual([0], Res).

%% Pattern with anchored start should only match at the beginning.
%% In standard Lua, gmatch with ^ returns at most one match.
gmatch_anchor_test() ->
    Res = eval(<<"local t = {}\n"
		 "for w in string.gmatch(\"hello world\", \"^%a+\") do\n"
		 "  t[#t+1] = w\n"
		 "end\n"
		 "return t[1], #t">>),
    ?assertEqual([<<"hello">>, 1], Res).

%% Typical use: building a table from key=value string (Lua book example).
gmatch_table_build_test() ->
    Res = eval(<<"local t = {}\n"
		 "local s = \"from=world, to=hello\"\n"
		 "for k, v in string.gmatch(s, \"(%w+)=(%w+)\") do\n"
		 "  t[k] = v\n"
		 "end\n"
		 "return t[\"from\"], t[\"to\"]">>),
    ?assertEqual([<<"world">>, <<"hello">>], Res).

%% Multiple iterators can coexist independently.
gmatch_multiple_iterators_test() ->
    Res = eval(<<"local r = {}\n"
		 "local iter1 = string.gmatch(\"a b c\", \"%a\")\n"
		 "local iter2 = string.gmatch(\"1 2 3\", \"%d\")\n"
		 "r[1] = iter1()\n"
		 "r[2] = iter2()\n"
		 "r[3] = iter1()\n"
		 "r[4] = iter2()\n"
		 "return r[1], r[2], r[3], r[4]">>),
    ?assertEqual([<<"a">>, <<"1">>, <<"b">>, <<"2">>], Res).
