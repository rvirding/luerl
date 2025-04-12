%% Copyright (C) 2024 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(luerl_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    State = luerl:init(),
    ?assertMatch({nil, _State}, luerl:encode(nil, State)),
    ?assertMatch({false, _State}, luerl:encode(false, State)),
    ?assertMatch({true, _State}, luerl:encode(true, State)),
    ?assertMatch({<<"binary">>, _State}, luerl:encode(<<"binary">>, State)),
    ?assertMatch({<<"atom">>, _State}, luerl:encode(atom, State)),
    ?assertMatch({5, _State}, luerl:encode(5, State)),
    ?assertMatch({{tref, _}, _State}, luerl:encode(#{a => 1, b => 2}, State)),
    ?assertMatch({{tref, _}, _State}, luerl:encode([{a, 1}, {b, 2}], State)).

encode_error_test() ->
    State = luerl:init(),
    ?assertException(error, {badarg, _}, luerl:encode({a, 1}, State)).

encode_table_test() ->
    {Table, State} = luerl:encode(#{a => 1}, luerl:init()),
    {ok, State1} = luerl:set_table_keys([<<"foo">>], Table, State),
    ?assertMatch(
        {ok, Table, _State2},
        luerl:get_table_keys([<<"foo">>], State1)
    ),
    ?assertMatch({tref, _}, Table).

invalid_value_test() ->
    State = luerl:init(),
    ?assertException(
        error,
        {badarg, {invalid, value}},
        luerl:encode({invalid, value}, State)
    ).

private_test() ->
    State1 = luerl:init(),
    State2 = luerl:put_private(secret, <<"mysecret">>, State1),
    ?assertMatch(<<"mysecret">>, luerl:get_private(secret, State2)),
    ?assertException(error, {badkey, missing}, luerl:get_private(missing, State2)),
    State3 = luerl:delete_private(secret, State2),
    ?assertException(error, {badkey, secret}, luerl:get_private(secret, State3)).
