%% File    : jumper.erl
%% Purpose : Brief demonstration of Jumper on Luerl.
%% Use     $ erlc jumper.erl && erl -pa ./ebin -s jumper run -s init stop -noshell
%% Or      $ make jumper

-module(jumper).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./jumper-example.lua", luerl:init()),

    done.