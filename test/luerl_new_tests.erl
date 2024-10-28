-module(luerl_new_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    State = luerl_new:init(),
    ?assertMatch({nil, _State}, luerl_new:encode(nil, State)),
    ?assertMatch({false, _State}, luerl_new:encode(false, State)),
    ?assertMatch({true, _State}, luerl_new:encode(true, State)),
    ?assertMatch({<<"binary">>, _State}, luerl_new:encode(<<"binary">>, State)),
    ?assertMatch({<<"atom">>, _State}, luerl_new:encode(atom, State)),
    ?assertMatch({5, _State}, luerl_new:encode(5, State)),
    ?assertMatch({{tref, _}, _State}, luerl_new:encode(#{a => 1, b => 2}, State)),
    ?assertMatch({{tref, _}, _State}, luerl_new:encode([{a,1},{b,2}], State)).

encode_error_test() ->
    State = luerl:init(),
    ?assertException(error, {badarg, _}, luerl:encode({a,1}, State)).

encode_table_test() ->
    {Table, State} = luerl_new:encode(#{a => 1}, luerl_new:init()),
    {ok, [], State1} = luerl_new:set_table_keys([<<"foo">>], Table, State),
    ?assertMatch({ok, Table, _State2},
                 luerl_new:get_table_keys([<<"foo">>], State1)),
    ?assertMatch({tref, _}, Table).

invalid_value_test() ->
    State = luerl_new:init(),
    ?assertException(error, {badarg, {invalid, value}},
		     luerl_new:encode({invalid, value}, State)).
