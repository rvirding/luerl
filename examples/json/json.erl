%% File    : json.erl
%% Purpose : Brief demonstration of json on Luerl.
%% Use     $ erlc json.erl && erl -pa ./ebin -s json run -s init stop -noshell
%% Or      $ make json

-module(json).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./json-example.lua", luerl:init()),

    done.
