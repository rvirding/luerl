%% File    : json.erl
%% Purpose : Brief demonstration of ANSI colors on Luerl.
%% Use     $ erlc json.erl && erl -pa ./ebin -s json run -s init stop -noshell
%% Or      $ make json

-module(chroma).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./chroma-example.lua", luerl:init()),

    done.
