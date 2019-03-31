%%% @author Hans-Christian Esperer <hc@hcesperer.org>
%%% @copyright (C) 2015, Hans-Christian Esperer
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an "AS
%%% IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
%%% express or implied.  See the License for the specific language
%%% governing permissions and limitations under the License.
%%%
%%% @doc
%%
%%% @end
%%% Created : 11 Jan 2015 by Hans-Christian Esperer <hc@hcesperer.org>

-module(luerl_funcall_tests).

-include_lib("eunit/include/eunit.hrl").

external_fun_test() ->
    State = luerl:init(),
    F = fun([A], S) ->
		{[A + 2, [A + 3, A + 4]], S}
	end,
    State1 = luerl:set_table([<<"testFun">>], F, State),
    {_, State2} = luerl:do(<<"function test(i)\n  local a, b = testFun(i)\n return (a == i + 2), (b[1] == i + 3), (b[2] == i + 4) end">>, State1),
    {Res, _State3} = luerl:call_function([test], [2], State2),
    [BoolVal, BoolVal2, BoolVal3] = Res,
    ?assertEqual(true, BoolVal),
    ?assertEqual(true, BoolVal2),
    ?assertEqual(true, BoolVal3).
    
external_nostate_fun_test() ->
    State = luerl:init(),
    F = fun([A]) ->
		[A + 2, [A + 3, A + 4]]
	end,
    State1 = luerl:set_table([<<"testFun">>], F, State),
    Chunk = <<"function test(i)\n"
	      "  local a, b = testFun(i)\n"
	      "  return (a == i + 2), (b[1] == i + 3), (b[2] == i + 4)\n"
	      "end">>,
    {_, State2} = luerl:do(Chunk, State1),
    {Res, _State3} = luerl:call_function([test], [2], State2),
    [BoolVal, BoolVal2, BoolVal3] = Res,
    ?assertEqual(true, BoolVal),
    ?assertEqual(true, BoolVal2),
    ?assertEqual(true, BoolVal3).

return_lib_function_test() ->
    State = luerl:init(),
    {_, State1} = luerl:do(<<"function test()\n  return string.find  end\n">>, State),
    {[Fun], _State2} = luerl:call_function([test], [1], State1),
    {Res, _State3} = Fun([<<"barfooblafasel">>, <<"foo">>], State1),
    ?assertEqual([4.0, 6.0], Res).

define_fun_in_lua_test() ->
    State = luerl:init(),
    Chunk = <<"function mkadder(incby)\n"
	      "  return function(i)\n"
	      "    print(\"Call into Luerl!\")\n"
	      "    return i + incby\n"
	      "  end\n"
	      "end\n">>,
    {_, State1} = luerl:do(Chunk, State),
    {[Fun], _State2} = luerl:call_function([mkadder], [1], State1),
    {[Fun2], _State3} = luerl:call_function([mkadder], [2], State1),
    ?assertEqual([5.0], Fun([4])),
    ?assertEqual([6.0], Fun2([4])).

define_fun2_in_lua_test() ->
    State = luerl:init(),
    Chunk = <<"function mklist(numentries)\n"
	      "  return function(entryval)\n"
	      "    local list = {}\n"
	      "    for i = 1,numentries do\n"
	      "      list[i] = entryval\n"
	      "    end\n"
	      "    return list\n"
	      "  end\n"
	      "end\n">>,
    {_, State1} = luerl:do(Chunk, State),
    {[Fun], _State2} = luerl:call_function([mklist], [5], State1),
    {[Fun2], _State3} = luerl:call_function([mklist], [10], State1),
    ?assertEqual([[{1,4.0}, {2,4.0}, {3,4.0}, {4,4.0}, {5,4.0}]],
		 Fun([4])),
    ?assertEqual([[{1,4.0}, {2,4.0}, {3,4.0}, {4,4.0}, {5,4.0},
		   {6,4.0}, {7,4.0}, {8,4.0}, {9,4.0}, {10,4.0}]],
		 Fun2([4])).

newindex_metamethod_test() ->
    State = luerl:init(),
    Chunk = <<"local t = {}\n"
	      "local m = setmetatable({}, {__newindex = function (tab, key, value)\n"
	      "t[key] = value\n"
	      "end})\n\n"
	      "m[123] = 456\n"
	      "return t[123], m[123]">>,
    {[TVal, MVal], _State1} = luerl:do(Chunk, State),
    ?assertEqual(456.0, TVal),
    ?assertEqual(nil, MVal).
