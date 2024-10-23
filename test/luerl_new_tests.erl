- module(luerl_new_tests).

-include_lib("eunit/include/eunit.hrl").

external_modify_global_test() ->
    % put(luerl_itrace, true),
    State = luerl_new:init(),

    ExternalFun = fun([Fun], S) ->
        {FunRef, NewState1} = luerl_new:encode(Fun, S),

        {ok, Res, NewState2} = luerl_new:call(FunRef, [], NewState1),
        {Res, NewState2}
    end,

    {ok, [], State1} = luerl_new:set_table_keys_dec([<<"external_call">>], ExternalFun, State),

    % Define a Lua function that modifies a global variable 'globalVar'
    LuaChunk = <<"globalVar = 'before'\n"
                "function modify_global(args)\n"
                "  globalVar = 'after'\n"
                "end\n"
                "external_call(modify_global)\n"  % Pass the function reference
                "return globalVar\n">>,

    {ok, Res, _State2} = luerl_new:do_dec(LuaChunk, State1),

    ?assertEqual([<<"after">>], Res).

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
