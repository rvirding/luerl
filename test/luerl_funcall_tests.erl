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
-import(luerl_lib, [lua_error/2]).

-export([bad_return_value/3]).

external_fun_test() ->
    State = luerl:init(),
    F = fun(Args, S) ->
                %% Must decode the args and encode the return value.
                [A] = luerl:decode_list(Args, S),
                luerl:encode_list([A + 2, [A + 3, A + 4]], S)
        end,
    {ok, State1} = luerl:set_table_keys_dec([<<"testFun">>], F, State),
    Chunk = <<"function test(i)\n"
              "  local a, b = testFun(i)\n"
              "  return (a == i + 2), (b[1] == i + 3), (b[2] == i + 4)\n"
              "end\n">>,
    {ok, _, State2} = luerl:do(Chunk, State1),
    {ok, Res, _State3} = luerl:call_function_dec([test], [2], State2),
    [BoolVal, BoolVal2, BoolVal3] = Res = [true,true,true],
    ?assertEqual(true, BoolVal),
    ?assertEqual(true, BoolVal2),
    ?assertEqual(true, BoolVal3).

%% If an error happens in a function call inside Erlang,
%% allow the user to propagate it up
external_error_test() ->
    State = luerl:init(),
    F = fun([Func], S) ->
                case luerl:call(Func, [], S) of
                    {ok, Ret, S1} -> {Ret, S1};
                    Error -> Error
                end
        end,
    {ok, State1} = luerl:set_table_keys_dec([<<"foo">>], F, State),
    Chunk = <<"global = 1\n"
              "local success, message =\n"
              "  pcall(function()\n"
              "    return foo(function()\n"
              "    global = 2\n"
              "    error(\"whoopsie\")\n"
              "    return \"yay\"\n"
              "  end)\n"
              "end)\n"
              "return global, success, message\n">>,
    {ok, [Global, Success, Message], _State2} = luerl:do(Chunk, State1),
    ?assertEqual(Global, 2),
    ?assertEqual(Success, false),
    ?assertEqual(Message, <<"whoopsie">>).

pcall_error_test() ->
    State = luerl:init(),
    F = fun([Message], S) ->
                lua_error(Message, S)
        end,
    {ok, State1} = luerl:set_table_keys_dec([<<"foo">>], F, State),
    ?assertMatch({lua_error, <<"one">>, _State}, luerl:call_function_dec([foo], [<<"one">>], State1)),
    ?assertMatch({lua_error, <<"two">>, _State}, luerl:do(<<"return foo(\"two\")">>, State1)),
    ?assertMatch({ok, [false, <<"three!">>], _State}, luerl:do(<<"return pcall(function()\nreturn foo(\"three\")\nend)\n">>, State1)).

bad_return_value_test() ->
    State = luerl:init(),
    F = fun(_Args, S) ->
                {bad_value, S}
        end,
    {ok, State1} = luerl:set_table_keys_dec([<<"foo">>], F, State),
    ?assertMatch({lua_error, illegal_return_value, _State}, luerl:call_function_dec([foo], [], State1)),
    Chunk = <<"return foo();">>,
    ?assertMatch({lua_error, illegal_return_value, _State}, luerl:do_dec(Chunk, State1)).

propagate_error_mfa_pcall_test() ->
    State = luerl:init(),
    {ok, State1} = luerl:set_table_keys_dec([<<"foo">>], {luerl_funcall_tests, bad_return_value, []}, State),
    ?assertMatch({lua_error, something_bad_happened, _State}, luerl:call_function_dec([foo], [], State1)),
    Chunk = <<"global = 1\n"
              "local success, message =\n"
              "pcall(function()\n"
              "  global = 2\n"
              "  return foo();\n"
              "end)\n"
              "return global, success, message\n">>,
    ?assertMatch({ok, [2, false, <<"something_bad_happened!">>], _State}, luerl:do_dec(Chunk, State1)).

bad_return_value_pcall_test() ->
    State = luerl:init(),
    F = fun(_Args, S) ->
                {bad_value, S}
        end,
    {ok, State1} = luerl:set_table_keys_dec([<<"foo">>], F, State),
    ?assertMatch({lua_error, illegal_return_value, _State}, luerl:call_function_dec([foo], [], State1)),
    Chunk = <<"global = 1\n"
              "local success, message =\n"
              "pcall(function()\n"
              "  global = 2\n"
              "  return foo();\n"
              "end)\n"
              "return global, success, message\n">>,
    ?assertMatch({ok, [2, false, <<"illegal format of return value">>], _State}, luerl:do_dec(Chunk, State1)).

return_lib_function_test() ->
    State = luerl:init(),
    {ok, _, State1} =
        luerl:do(<<"function test()\n  return string.find  end\n">>, State),
    {ok, [{M,F,A}], _State2} = luerl:call_function_dec([test], [1], State1),
    {Res, _State3} = apply(M, F, [A, [<<"barfooblafasel">>, <<"foo">>], State1]),
    ?assertEqual([4, 6], Res).

define_fun_in_lua_test() ->
    State = luerl:init(),
    Chunk = <<"function mkadder(incby)\n"
              "  return function(i)\n"
              "    print(\"Call into Luerl!\")\n"
              "    return i + incby\n"
              "  end\n"
              "end\n">>,
    {ok, _, State1} = luerl:do(Chunk, State),
    {ok, [Fun2], State2} = luerl:call_function_dec([mkadder], [1], State1),
    {ok, [Fun3], State3} = luerl:call_function_dec([mkadder], [2], State1),

    %% Should really decode the return value, but it is only a number.
    ?assertMatch({[5], _}, Fun2([4], State2)),
    ?assertMatch({[5.0],_}, Fun2([4.0], State2)),
    ?assertMatch({[6], _}, Fun3([4], State3)).

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
    {ok, _, State1} = luerl:do(Chunk, State),

    %% Build a luerl function and safely call it.
    {Emklist, St2} = luerl:encode_list([mklist], State1),
    {ok, [Efunc5], St3} = luerl:call_function(Emklist, [5], St2),
    {ok, Res20, St4} = luerl:call_function(Efunc5, [4], St3),
    ?assertMatch([[{1,4}, {2,4}, {3,4}, {4,4}, {5,4}]],
                 luerl:decode_list(Res20, St4)),

    %% Build an Erlang fun and just unsafely call it.
    {ok, [Fun2], State2} = luerl:call_function_dec([mklist], [5], State1),
    {Res21,State21} = Fun2([4], State2),
    ?assertMatch([[{1,4}, {2,4}, {3,4}, {4,4}, {5,4}]],
                 luerl:decode_list(Res21, State21)),
    {Res22,State22} = Fun2([4.0], State2),
    ?assertMatch([[{1,4.0}, {2,4.0}, {3,4.0}, {4,4.0}, {5,4.0}]],
                 luerl:decode_list(Res22, State22)),

    {ok, [Fun3], State3} = luerl:call_function_dec([mklist], [10], State1),
    {Res3, State31} = Fun3([4], State3),
    ?assertMatch([[{1,4}, {2,4}, {3,4}, {4,4}, {5,4},
                   {6,4}, {7,4}, {8,4}, {9,4}, {10,4}]],
                 luerl:decode_list(Res3, State31)).

newindex_metamethod_test() ->
    State = luerl:init(),
    Chunk = <<"local t = {}\n"
              "local m = setmetatable({}, {__newindex = function (tab, key, value)\n"
              "t[key] = value\n"
              "end})\n\n"
              "m[123] = 456\n"
              "return t[123], m[123]">>,
    {ok, [TVal, MVal], _State1} = luerl:do_dec(Chunk, State),
    ?assertEqual(456, TVal),
    ?assertEqual(nil, MVal).

bad_return_value(_Arg, _Args, State) ->
    lua_error(something_bad_happened, State).
