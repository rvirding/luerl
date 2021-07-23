%% File    : hello_userdata_new.erl
%% Purpose : Brief demonstration of Luerl userdata access.
%% Use     $ erlc hello_table.erl && erl -pa ../../ebin  -s hello_table run -s init stop -noshell

-module(hello_userdata_new).
-export([run/0]).

run() ->
    St0 = luerl_new:init(),
    U42 = {userdata,42},                        %The original decoded data
    {Uref,St1} = luerl_new:encode(U42, St0),
    {ok,_,St2} = luerl_new:set_table_keys([<<"u1">>], Uref, St1),
    {ok,_,St3} = luerl_new:set_table_keys([<<"u2">>], Uref, St2),
    St4 = luerl_heap:set_userdata(Uref, 84, St3),
    {ok,Uref,St5} = luerl_new:get_table_keys([<<"u1">>], St4),
    {ok,Uref,St6} = luerl_new:get_table_keys([<<"u2">>], St5),
    U84 = {userdata,84},                        %New decoded data
    U84 = luerl_new:decode(Uref, St6),
    St6.
