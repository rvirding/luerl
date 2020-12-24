%% File    : utf8.erl
%% Purpose : Brief demonstration of utf8.lua on Luerl.
%% Use     $ erlc utf8.erl && erl -pa ./ebin -s utf8 run -s init stop -noshell
%% Or      $ make utf8

-module(utf8).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./utf8-example.lua", luerl:init()),

    done.