%% File    : luafun.erl
%% Purpose : Brief demonstration of Lua Fun on Luerl.
%% Use     $ erlc luafun.erl && erl -pa ./ebin -s luafun run -s init stop -noshell
%% Or      $ make luafun

-module(luafun).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./luafun-example.lua", luerl:init()),

    done.
