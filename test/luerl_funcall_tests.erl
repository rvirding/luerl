%%% @author Hans-Christian Esperer <hc@hcesperer.org>
%%% @copyright (C) 2015, Hans-Christian Esperer
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
    ?assertEqual(BoolVal, true),
    ?assertEqual(BoolVal2, true),
    ?assertEqual(BoolVal3, true).
    
