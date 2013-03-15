%% File    : hello_table.erl
%% Purpose : Brief demonstration of Luerl table access.
%% Use     $ erlc hello_table.erl && erl -pa ../../ebin  -s hello_table run -s init stop -noshell

-module(hello_table).
-export([run/0]).

run() ->
    LuaScript = <<"hello_table = { hello=\"world\" }; return hello_table">>,
    {[_Table], Lua0} = luerl:do(LuaScript),

    {World, Lua1} = luerl:get_table([hello_table, hello], Lua0),
    {HelloDict, Lua2} = luerl:set_table([hello_table, hello], there, Lua1),
    {There, Lua3} = luerl:get_table([hello_table, hello], Lua2),
    {HelloDict, Lua4} = luerl:get_table([hello_table], Lua3),
    io:format("(1) hello ~s ~s - ~p~n", [There, World, HelloDict]),
    
    {HelloTab, Lua5} = luerl:set_table1([<<"hello_table">>, <<"goodbye">>], <<"bye">>, Lua4),
    {Bye, Lua6} = luerl:get_table1([<<"hello_table">>, <<"goodbye">>], Lua5),
    {HelloTab, _Lua7} = luerl:get_table1([<<"hello_table">>], Lua6),
    io:format("(2) ~s - ~p~n", [Bye, HelloTab]),
    done.
