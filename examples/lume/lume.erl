%% File    : lume.erl
%% Purpose : Brief demonstration of lume on Luerl.
%% Use     $ erlc lume.erl && erl -pa ./ebin -s lume run -s init stop -noshell
%% Or      $ make lume

-module(lume).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./lume-example.lua", luerl:init()),

    done.
