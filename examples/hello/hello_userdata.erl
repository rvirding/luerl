%% File    : hello_userdata.erl
%% Purpose : Brief demonstration of Luerl userdata access.
%% Use     $ erlc hello_userdata.erl && erl -pa ../../ebin  -s hello_userdata run -s init stop -noshell

-module(hello_userdata).
-export([run/0]).

run() ->
    St0 = luerl:init(),
    U42 = {userdata,42},			%The original decoded data
    {Uref,St1} = luerl:encode(U42, St0),
    St2 = luerl:set_table1([<<"u1">>], Uref, St1),
    St3 = luerl:set_table1([<<"u2">>], Uref, St2),
    %% This call wraps the actual data for us.
    St4 = luerl_heap:set_userdata_data(Uref, 84, St3),
    U84 = {userdata,84},			%New decoded data
    {U84,St5} = luerl:get_table([<<"u1">>], St4),
    {U84,St6} = luerl:get_table([<<"u2">>], St5),
    St6.
