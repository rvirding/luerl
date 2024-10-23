- module(luerl_new_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    State = luerl_new:init(),
    ?assertMatch({nil, _State}, luerl_new:encode(nil, State)),
    ?assertMatch({false, _State}, luerl_new:encode(false, State)),
    ?assertMatch({true, _State}, luerl_new:encode(true, State)),
    ?assertMatch({<<"binary">>, _State}, luerl_new:encode(<<"binary">>, State)),
    ?assertMatch({<<"atom">>, _State}, luerl_new:encode(atom, State)),
    ?assertMatch({5, _State}, luerl_new:encode(5, State)),
    ?assertMatch({{tref, _}, _State}, luerl_new:encode(#{a => 1, b => 2}, State)).

encode_map_test() ->
    ?assertMatch({{tref, _}, _State}, luerl_new:encode(#{a => 1}, luerl_new:init())).

encode_table_test() ->
    {Table, State} = luerl_new:encode(#{a => 1}, luerl_new:init()),
    {ok, [], State1} = luerl_new:set_table_keys([<<"foo">>], Table, State),
    ?assertMatch({ok, Table, _State2}, luerl_new:get_table_keys([<<"foo">>], State1)),
    ?assertMatch({tref, _}, Table),
    ?assertMatch({Table, _State}, luerl_new:encode(Table, State1)).

invalid_table_test() ->
    ?assertException(error, badarg, luerl_new:encode({tref, 42}, luerl_new:init())).

invalid_erlfunc_return_test() ->
    State = luerl_new:init(),
    Func = fun(_Args, State) ->
                   {invalid, State}
                       end,
    {ok, [], State1} = luerl_new:set_table_keys_dec([foo], Func, State),
    ?assertException(error, blah, luerl_new:do("return foo()", State1)).
