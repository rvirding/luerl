%% File    : schema.erl
%% Purpose : Brief demonstration of schema.lua on Luerl.
%% Use     $ erlc schema.erl && erl -pa ./ebin -s schema run -s init stop -noshell
%% Or      $ make schema

-module(schema).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./schema-example.lua", luerl:init()),

    done.