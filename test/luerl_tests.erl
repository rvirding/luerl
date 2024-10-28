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
    ?assertMatch({{tref, _}, _State}, luerl:encode([{a,1},{b,2}], State)).

encode_error_test() ->
    State = luerl:init(),
    ?assertException(error, {badarg, _}, luerl:encode({a,1}, State)).

encode_table_test() ->
    {Table, State} = luerl:encode(#{a => 1}, luerl_new:init()),
    State1 = luerl:set_table1([<<"foo">>], Table, State),
    ?assertMatch({Table, _State2},
                 luerl:get_table1([<<"foo">>], State1)),
    ?assertMatch({tref, _}, Table).

invalid_value_test() ->
    State = luerl:init(),
    ?assertException(error, {badarg, {invalid, value}},
		     luerl:encode({invalid, value}, State)).
