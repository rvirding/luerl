- module(luerl_tests).

-include_lib("eunit/include/eunit.hrl").

external_modify_global_test() ->
    % put(luerl_itrace, true),
    State = luerl:init(),

    ExternalFun = fun([Fun], S) ->
        {FunRef, NewState1} = luerl:encode(Fun, S),

        { Res, NewState2} = luerl:call(FunRef, [], NewState1),
        {Res, NewState2}
    end,

    State1 = luerl:set_table([<<"external_call">>], ExternalFun, State),

    % Define a Lua function that modifies a global variable 'globalVar'
    LuaChunk = <<"globalVar = 'before'\n"
                "function modify_global(args)\n"
                "  globalVar = 'after'\n"
                "end\n"
                "external_call(modify_global)\n"  % Pass the function reference
                "return globalVar\n">>,

    {Res, _State2} = luerl:do(LuaChunk, State1),

    ?assertEqual([<<"after">>], Res).

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
