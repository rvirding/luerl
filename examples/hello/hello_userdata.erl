%% File    : hello_userdata.erl
%% Purpose : Brief demonstration of Luerl userdata access.
%% Use     $ erlc hello_userdata_new.erl && erl -pa ../../ebin  -s hello_userdata_new run -s init stop -noshell

-module(hello_userdata).
-export([run/0]).

run() ->
    St0 = luerl:init(),
    U42 = {userdata,42},                        %The original decoded data
    {Uref,St1} = luerl:encode(U42, St0),
    {ok,St2} = luerl:set_table_keys([<<"u1">>], Uref, St1),
    {ok,St3} = luerl:set_table_keys([<<"u2">>], Uref, St2),
    %% This call wraps the actual data for us.
    St4 = luerl_heap:set_userdata_data(Uref, 84, St3),
    {ok,Uref,St5} = luerl:get_table_keys([<<"u1">>], St4),
    {ok,Uref,St6} = luerl:get_table_keys([<<"u2">>], St5),
    U84 = {userdata,84},                        %New decoded data
    U84 = luerl:decode(Uref, St6),
    St6.
