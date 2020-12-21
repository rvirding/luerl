%% File    : bump.erl
%% Purpose : Brief demonstration of bump.lua on Luerl.
%% Use     $ erlc bump.erl && erl -pa ./ebin -s bump run -s init stop -noshell
%% Or      $ make bump

-module(bump).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./bump-example.lua", luerl:init()),

    done.
