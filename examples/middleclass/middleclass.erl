%% File    : middleclass.erl
%% Purpose : Brief demonstration of middleclass on Luerl.
%% Use     $ erlc middleclass.erl && erl -pa ./ebin -s middleclass run -s init stop -noshell
%% Or      $ make middleclass

-module(middleclass).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./middleclass-example.lua", luerl:init()),

    done.
