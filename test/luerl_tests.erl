- module(luerl_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    State = luerl:init(),
    ?assertMatch({nil, _State}, luerl:encode(nil, State)),
    ?assertMatch({false, _State}, luerl:encode(false, State)),
    ?assertMatch({true, _State}, luerl:encode(true, State)),
    ?assertMatch({<<"binary">>, _State}, luerl:encode(<<"binary">>, State)),
    ?assertMatch({<<"atom">>, _State}, luerl:encode(atom, State)),
    ?assertMatch({5, _State}, luerl:encode(5, State)),
    ?assertMatch({{tref, _}, _State}, luerl:encode(#{a => 1, b => 2}, State)).

encode_map_test() ->
    ?assertMatch({{tref, _}, _State}, luerl:encode(#{a => 1}, luerl:init())).

encode_table_test() ->
    {Table, State} = luerl:encode(#{a => 1}, luerl:init()),
    ?assertMatch({tref, _}, Table),
    ?assertMatch({Table, _State}, luerl:encode(Table, State)).

invalid_table_test() ->
    ?assertException(error, badarg, luerl:encode({tref, 42}, luerl:init())).

invalid_value_test() ->
    ?assertException(error, {badarg, {invalid, value}}, luerl:encode({invalid, value}, luerl_new:init())).
