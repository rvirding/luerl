%% File    : tamale.erl
%% Purpose : Brief demonstration of tamale on Luerl.
%% Use     $ erlc tamale.erl && erl -pa ./ebin -s tamale run -s init stop -noshell
%% Or      $ make tamale

-module(tamale).
-export([run/0]).

run() ->

    % execute an example file
    luerl:dofile("./tamale-example.lua", luerl:init()),

    done.
