%% File    : chroma.erl
%% Purpose : Brief demonstration of ANSI colors on Luerl.
%% Use     $ erlc chroma.erl && erl -pa ./ebin -s chroma run -s init stop -noshell
%% Or      $ make chroma

-module(chroma).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./chroma-example.lua", luerl:init()),

    done.
