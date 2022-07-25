%% File    : hello_table.erl
%% Purpose : Brief demonstration of Luerl table access.
%% Use     $ erlc hello_table_new.erl && erl -pa ../../ebin  -s hello_table_new run -s init stop -noshell

-module(hello_table_new).
-export([run/0]).

run() ->
    LuaScript = <<"hello_table = { hello=\"world\" }; return hello_table">>,
    {ok, [_Table], Lua0} = luerl_new:do(LuaScript, luerl_new:init()),

    {ok,World, Lua1} =
        luerl_new:get_table_keys_dec([hello_table, hello], Lua0),
    {ok,_,Lua2} =
        luerl_new:set_table_keys_dec([hello_table, hello], there, Lua1),
    {ok,HelloDict,Lua3} = luerl_new:get_table_keys_dec([hello_table], Lua2),
    {ok,There, Lua4} = luerl_new:get_table_keys_dec([hello_table, hello], Lua3),
    io:format("(1) hello ~s ~s - ~p~n", [There, World, HelloDict]),

    {ok,_,Lua5} =
        luerl_new:set_table_keys([<<"hello_table">>, <<"goodbye">>],
                                 <<"bye">>, Lua4),
    {ok, Bye, Lua6} =
        luerl_new:get_table_keys([<<"hello_table">>, <<"goodbye">>], Lua5),
    {ok, HelloTab, _Lua7} = luerl_new:get_table_keys([<<"hello_table">>], Lua6),
    io:format("(2) ~s - ~p~n", [Bye, HelloTab]),

    done.
