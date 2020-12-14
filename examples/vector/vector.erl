%% File    : vector.erl
%% Purpose : Brief demonstration of a 2D vector library on Luerl.
%% Use     $ erlc vector.erl && erl -pa ./ebin -s vector run -s init stop -noshell
%% Or      $ make vector

-module(vector).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./vector-example.lua", luerl:init()),

    done.
