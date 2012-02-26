%% File    : bench.erl
%% Purpose : Running Luabench () on Luerl ()
%% Use     $ erlc bench.erl && erl -pa ./ebin -s bench run -s init stop -noshell
%% Or      $ make bench

-module(bench).
-export([run/0]).

run() ->

    % separately parse, then execute
    {ok, Chunk} = luerl:loadfile("./bench.lua"),
    State = luerl:start(),
    luerl:call(Chunk, State),
    
    done.
