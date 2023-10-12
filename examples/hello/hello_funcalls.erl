%% File     : hello_funcalls.erl
%% Purpose  : Demonstration of various ways to expose Erlang functions to Luerl.
%% Use      $ erlc hello_funcalls.erl && erl -pa ../../ebin -s hello_funcalls run -s init stop -noshell

-module(hello_funcalls).

-include("luerl.hrl").

-export([run/0,mfa_function/3]).

regular_function(Args,St) ->
    io:format("regular_function(~p)\n", [Args]),
    {[42], St}.

mfa_function(StaticArgs,DynamicArgs,St) ->
    io:format("mfa_function(~p, ~p)\n", [StaticArgs, DynamicArgs]),
    {[42], St}.

run() ->
    LuaScript = <<"return hello_funcall(4, 5, 6)">>,
    Lua = luerl:init(),
    Lua1 = luerl:set_table([<<"hello_funcall">>], fun regular_function/2, Lua),
    % The argument part of {M,F,A} won't get encoded, so that is why we can get away with passing a tuple here.
    Lua2 = luerl:set_table([<<"hello_funcall">>], {hello_funcalls,mfa_function,{1,2}}, Lua),
    {Res1, _} = luerl:do(LuaScript, Lua1),
    io:format("regular_function got ~p~n", Res1),
    {Res2, _} = luerl:do(LuaScript, Lua2),
    io:format("mfa_function got ~p~n", Res2).
