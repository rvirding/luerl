%% File    : hello.erl
%% Purpose : Brief demonstration of Luerl basics.
%% Use     $ erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell
%% Or      $ make hello

-module(hello).
-export([run/0]).

run() ->

    % execute a string
    luerl:do("print(\"Hello, Robert(o)!\")"),

    % execute a file
    luerl:dofile("./examples/hello/hello.lua"),

    % separately parse, then execute
    {ok, Chunk} = luerl:load("print(\"Hello, Chunk!\")"),
    State = luerl:newstate(),
    {_Ret, _NewState} = luerl:do(Chunk, State),

    done.
