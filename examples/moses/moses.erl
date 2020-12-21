%% File    : moses.erl
%% Purpose : Brief demonstration of Moses on Luerl.
%% Use     $ erlc moses.erl && erl -pa ./ebin -s moses run -s init stop -noshell
%% Or      $ make moses

-module(moses).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./moses-example.lua", luerl:init()),

    done.
