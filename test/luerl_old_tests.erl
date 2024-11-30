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

-module(luerl_old_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    State = luerl_old:init(),
    ?assertMatch({nil, _State}, luerl_old:encode(nil, State)),
    ?assertMatch({false, _State}, luerl_old:encode(false, State)),
    ?assertMatch({true, _State}, luerl_old:encode(true, State)),
    ?assertMatch({<<"binary">>, _State}, luerl_old:encode(<<"binary">>, State)),
    ?assertMatch({<<"atom">>, _State}, luerl_old:encode(atom, State)),
    ?assertMatch({5, _State}, luerl_old:encode(5, State)),
    ?assertMatch({{tref, _}, _State}, luerl_old:encode(#{a => 1, b => 2}, State)),
    ?assertMatch({{tref, _}, _State}, luerl_old:encode([{a,1},{b,2}], State)).

encode_error_test() ->
    State = luerl_old:init(),
    ?assertException(error, {badarg, _}, luerl_old:encode({a,1}, State)).

encode_table_test() ->
    {Table, State} = luerl_old:encode(#{a => 1}, luerl_old:init()),
    State1 = luerl_old:set_table1([<<"foo">>], Table, State),
    ?assertMatch({Table, _State2},
                 luerl_old:get_table1([<<"foo">>], State1)),
    ?assertMatch({tref, _}, Table).

invalid_value_test() ->
    State = luerl_old:init(),
    ?assertException(error, {badarg, {invalid, value}},
		     luerl_old:encode({invalid, value}, State)).
