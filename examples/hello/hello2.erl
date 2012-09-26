%% File    : hello2.erl
%% Author  : Henning Diedrich
%% File    : luerl/examples/hello/hello2.erl
%% Purpose : Demonstration of the Luerl interface.
%% Author  : Henning Diedrich
%% Use     : $ erlc hello2.erl && erl -pa ../ebin -s hello2 run -s init stop -noshell
%% Or      : $ cd .. && make hello2

-module(hello2).
-export([run/0]).

run() ->

    io:format("-------------------------------------------~n"),
    io:format("This is an assortment of samples and tests.~n"),
    io:format("-------------------------------------------~n"),
    io:format("It's a comprehensive demo of the interface.~n"),
    io:format("Please check out the source to learn more.~n"),

    % execute a string
    luerl:eval("print(\"Hello, Robert!\")"),
    luerl:eval(<<"print(\"Hello, Roberto!\")">>),
    luerl:do("print(\"Hej, Robert!\")"),
    luerl:do(<<"print(\"Olà, Roberto!\")">>),
    
    % execute a string, get a result
    {ok,A} = luerl:eval("return 1 + 1"),
    {ok,A} = luerl:eval(<<"return 1 + 1">>),
    io:format("1 + 1 = ~p!~n", [A]),
   

    % execute a file
    luerl:evalfile("./examples/hello/hello2-1.lua"),
    luerl:dofile("./examples/hello/hello2-1.lua"),

    % execute a file, get a result
    {ok,B} = luerl:evalfile("./examples/hello/hello2-2.lua"),
    {B,_} = luerl:dofile("./examples/hello/hello2-2.lua"),
    io:format("2137 * 42 = ~p?~n", [B]),


    % separately parse, then execute
    {ok,Chunk1} = luerl:load("print(\"Hello, Chunk!\")"),
    {ok,Chunk1} = luerl:load(<<"print(\"Hello, Chunk!\")">>),
    luerl:eval(Chunk1),
    luerl:do(Chunk1),
    luerl:call(Chunk1, []),

    % separately parse, then execute a file
    {ok,Chunk2} = luerl:loadfile("./examples/hello/hello2-3.lua"),
    luerl:eval(Chunk2),
    luerl:do(Chunk2),
    luerl:call(Chunk2, []),

    % separately parse, then execute, get a result
    {ok,Chunk3} = luerl:load("return 'Marvelous wheater today, isn°t it!'"),
    {ok,Chunk3} = luerl:load(<<"return 'Marvelous wheater today, isn°t it!'">>),
    {ok,C} = luerl:eval(Chunk3),
    {C,_} = luerl:do(Chunk3),
    {C,_} = luerl:call(Chunk3, []),
    io:format("And I say: ~p~n", [C]),

    % separately parse, then execute a file, get a result
    {ok,Chunk4} = luerl:loadfile("./examples/hello/hello2-4.lua"),
    {ok,D} = luerl:eval(Chunk4),
    {D,_} = luerl:do(Chunk4),
    {D,_} = luerl:call(Chunk4, []),
    io:format("And he says: ~p~n", [D]),


    % Same as above, passing State in.

    % create state
    New = luerl:init(),
    {_,New2} = luerl:do("print 'hello generix'"),
    
    % change state
    {_,State} = luerl:do("a = 1000"),
    {_,State1} = luerl:do("a = 1000", New),

    % execute a string, using passed in State
    luerl:eval("print(a)", State),
    luerl:eval(<<"print(a+1)">>, State),
    luerl:do("print(a+2)", State),
    luerl:do(<<"print(a+3)">>, State),
    
    % execute a string, get a result from passed in State
    {ok,E} = luerl:eval("return 4 * a", State),
    {ok,E} = luerl:eval(<<"return 4 * a">>, State),
    {E,_} = luerl:do("return 4 * a", State),
    {E,_} = luerl:do(<<"return 4 * a">>, State),
    io:format("4 x a = ~p!~n", [E]),
   
    % execute a string, get a result, change State
    {Z,State2} = luerl:do("a = 123; return a * 3", State1),
    {Z,State3} = luerl:do(<<"return (3 * a)">>, State2),
    io:format("a = ~p~n", [Z]),
   
    % execute a file using passed in state
    luerl:evalfile("./examples/hello/hello2-5.lua", State3),
    luerl:dofile("./examples/hello/hello2-5.lua", State3),

    % execute a file that changes the State
    {_,State4} = luerl:dofile("./examples/hello/hello2-6.lua", State3),
    luerl:do("print(a)", State4),

    % execute a file, get a result
    {ok,F} = luerl:evalfile("./examples/hello/hello2-7.lua", State4),
    {F,State5} = luerl:dofile("./examples/hello/hello2-7.lua", State4),
    io:format("#1 F: ~s~n", [F]),

    % execute a file that changes the State, and get a value back
    {F,State6} = luerl:dofile("./examples/hello/hello2-7.lua", State5),
    io:format("#2 F: ~s = ", [F]),
    luerl:do("print('#3 F: ' .. a)", State6),

    % separately parse, then execute
    {ok,Chunk11} = luerl:load("print(\"Hello, \" .. a .. \"!\")"),
    {ok,Chunk11} = luerl:load(<<"print(\"Hello, \" .. a .. \"!\")">>),
    luerl:eval(Chunk11,State6),
    luerl:do(Chunk11,State6),
    luerl:call(Chunk11,[],State6),

    % separately parse, then execute a file
    {ok,Chunk12} = luerl:loadfile("./examples/hello/hello2-8.lua"),
    luerl:eval(Chunk12,State6),
    luerl:do(Chunk12,State6),
    luerl:call(Chunk12,[],State6),

    % separately parse, then execute, get a result
    {ok,Chunk13} = luerl:load("a = a .. ' (this is Greek)'; return a"),
    {ok,Chunk13} = luerl:load(<<"a = a .. ' (this is Greek)'; return a">>),
    {ok,G} = luerl:eval(Chunk13, State6),
    {G,State7} = luerl:do(Chunk13, State6),
    {G,State7} = luerl:call(Chunk13, [], State6),
    io:format("And again I said: ~s~n", [G]),

    % separately parse, then execute a file, get a result
    {ok,Chunk14} = luerl:loadfile("./examples/hello/hello2-9.lua"),
    {ok,H} = luerl:eval(Chunk14, State7),
    {H,State8} = luerl:do(Chunk14, State7),
    {H,State8} = luerl:call(Chunk14, [], State7),
    io:format("Well: ~s~n", [H]),

    io:format("done~n").
