%% File    : hello2.erl
%% File    : luerl/examples/hello/hello2.erl
%% Purpose : Demonstration of the Luerl interface.
%% Author  : Henning Diedrich
%% Use     : $ cd examples/hello && erlc hello2.erl && erl -pa ../../ebin -s hello2 run -s init stop -noshell
%% Or      : $ make examples

-module(hello2).
-export([run/0]).

run() ->

    io:format("-------------------------------------------~n"),
    io:format("This is an assortment of samples and tests.~n"),
    io:format("-------------------------------------------~n"),
    io:format("It's a comprehensive demo of the interface.~n"),
    io:format("Please check out the source to learn more.~n"),

    % execute a string
    luerl:eval("print(\"(1) Hello, Robert!\")"),
    luerl:eval(<<"print(\"(2) Hello, Roberto!\")">>),
    luerl:do("print(\"(3) Hej, Robert!\")"),
    luerl:do(<<"print(\"(4) Olà, Roberto!\")">>),
    
    % execute a string, get a result
    {ok,A} = luerl:eval("return 1 + 1"),
    {ok,A} = luerl:eval(<<"return 1 + 1">>),
    io:format("(5) 1 + 1 = ~p!~n", [A]),

    % execute a file
    luerl:evalfile("./hello2-1.lua"),
    luerl:dofile("./hello2-1.lua"),

    % execute a file, get a result
    {ok,B} = luerl:evalfile("./hello2-2.lua"),
    {B,_} = luerl:dofile("./hello2-2.lua"),
    io:format("(7) 2137 * 42 = ~p?~n", [B]),

	% execute a standard function
	luerl:call([print], [<<"(8) Hello, standard print function!">>]),
	luerl:call([print], [<<"(9) Hello, standard print function!">>],  luerl:init()),
	{Result1,_} = luerl:call([table,pack], [<<"a">>,<<"b">>,42]),
	{Result1,_} = luerl:call([table,pack], [<<"a">>,<<"b">>,42], luerl:init()),
    io:format("(10) ~p?~n", [Result1]),

    % separately parse, then execute (doubles (11) and Chunk1 as assertion)
    {ok,Chunk1} = luerl:load("print(\"(11) Hello, Chunk 1!\")"),
    {ok,Chunk1} = luerl:load(<<"print(\"(11) Hello, Chunk 1!\")">>),
    luerl:eval(Chunk1),
    luerl:do(Chunk1),

    % separately parse, then execute (doubles (12) and Chunk2 as assertion)
    {ok,Chunk2} = luerl:load("function chunk2() print(\"(12) Hello, Chunk 2!\") end"),
    {ok,Chunk2} = luerl:load(<<"function chunk2() print(\"(12) Hello, Chunk 2!\") end">>),
    {ok,Result2} = luerl:eval(Chunk2),
    {Result2,State2} = luerl:do(Chunk2),
    {Result2,State2A} = luerl:do(<<"function chunk2() print(\"(12) Hello, Chunk 2!\") end">>),
    luerl:call([chunk2], [], State2),	
    luerl:call([chunk2], [], State2A),	

    % separately parse, then execute a file. The file defines a function no()
    {ok,Chunk3} = luerl:loadfile("./hello2-3.lua"),
    {ok,Result3} = luerl:eval(Chunk3),
    {Result3,State3} = luerl:do(Chunk3),
    {[],_} = luerl:call([no], [], State3),

    % separately parse, then execute, get a result
    {ok,Chunk4} = luerl:load("return '(17b) Marvelous wheater today, isn°t it!'"),
    {ok,Chunk4} = luerl:load(<<"return '(17b) Marvelous wheater today, isn°t it!'">>),
    {ok,Result4} = luerl:eval(Chunk4),
    {Result4,_} = luerl:do(Chunk4),
    io:format("(17) And I say: ~p~n", [Result4]),

    % separately parse, then execute a file, get a result
    {ok,Chunk5} = luerl:loadfile("./hello2-4.lua"),
    {ok,Result5} = luerl:eval(Chunk5),
    {Result5,_} = luerl:do(Chunk5),
    io:format("(18) And he says: ~p~n", [Result5]),


    % Same as above, passing State in all times.

    % create state
    New = luerl:init(),
    {_,_New2} = luerl:do("print '(19) hello generix'"),
    
    % change state
    {_,State0} = luerl:do("a = 1000"),
    {_,State01} = luerl:do("a = 1000", New),

    % execute a string, using passed in State0
    luerl:eval("print('(20) ' .. a)", State0),
    luerl:eval(<<"print('(21) ' .. a+1)">>, State0),
    luerl:do("print('(22) ' .. a+2)", State0),
    luerl:do(<<"print('(23) ' .. a+3)">>, State0),
    
    % execute a string, get a result from passed in State0
    {ok,E} = luerl:eval("return 4 * a", State0),
    {ok,E} = luerl:eval(<<"return 4 * a">>, State0),
    {E,_} = luerl:do("return 4 * a", State0),
    {E,_} = luerl:do(<<"return 4 * a">>, State0),
    io:format("(24) 4 x a = ~p!~n", [E]),
   
    % execute a string, get a result, change State0
    {Z,State02} = luerl:do("a = 123; return a * 3", State01),
    {Z,State03} = luerl:do(<<"return (3 * a)">>, State02),
    io:format("(25) a = ~p~n", [Z]),
   
    % execute a file using passed in state
    luerl:evalfile("./hello2-5.lua", State03),
    luerl:dofile("./hello2-5.lua", State03),

    % execute a file that changes the State0
    {_,State04} = luerl:dofile("./hello2-6.lua", State03),
    luerl:do("print('(27) (b) ' .. a)", State04),

    % execute a file, get a result
    {ok,F} = luerl:evalfile("./hello2-7.lua", State04),
    {F,State05} = luerl:dofile("./hello2-7.lua", State04),
    io:format("(28) F: ~s~n", [F]),

    % execute a file that changes the State0, and get a value back
    {F,State06} = luerl:dofile("./hello2-7.lua", State05),
    io:format("(29) F: ~s = ", [F]),
    luerl:do("print('(30) F: ' .. a)", State06),

    % separately parse, then execute
    {ok,Chunk11} = luerl:load("print(\"(31) Hello, \" .. a .. \"!\")"),
    {ok,Chunk11} = luerl:load(<<"print(\"(31) Hello, \" .. a .. \"!\")">>),
    luerl:eval(Chunk11,State06),
    luerl:do(Chunk11,State06),

    % separately parse, then execute a file. The file defines a function old()
    {ok,Chunk12} = luerl:loadfile("./hello2-8.lua"),
    {ok,Result12} = luerl:eval(Chunk12,State06),
    {Result12,State06A} = luerl:do(Chunk12,State06),
    luerl:call([old],[],State06A),

    % separately parse, then execute, get a result
    {ok,Chunk13} = luerl:load("a = '(30a)' .. a .. ' (this is Greek)'; return a"),
    {ok,Chunk13} = luerl:load(<<"a = '(30a)' .. a .. ' (this is Greek)'; return a">>),
    {ok,Result07} = luerl:eval(Chunk13, State06),
    {Result07,State07} = luerl:do(Chunk13, State06),
    io:format("(34) And again I said: ~s~n", [Result07]),

    % separately parse, then execute a file, get a result. The file defines confirm(p)
    {ok,Chunk14} = luerl:loadfile("./hello2-9.lua"),
    {ok,Result14} = luerl:eval(Chunk14, State07),
    {Result14,State14} = luerl:do(Chunk14, State07),
    io:format("(35) And twice: ~s~n", [Result14]),
    {Result14A,_} = luerl:call([confirm], [<<"Is it?">>], State14),
    io:format("(36) Well: ~s~n", [Result14A]),

    % execute a file, get the decoded result of a table
    {ok,Result15} = luerl:evalfile("./hello2-10.lua", State14),
    io:format("(37) Decoded table from eval ~p~n", [Result15]),

    io:format("done~n").
