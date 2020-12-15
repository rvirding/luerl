%% File    : tiny.erl
%% Purpose : Brief demonstration of tiny-ecs on Luerl.
%% Use     $ erlc tiny.erl && erl -pa ./ebin -s tiny run -s init stop -noshell
%% Or      $ make tiny

-module(tiny).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./tiny-example.lua", luerl:init()),

    done.
