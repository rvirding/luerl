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

    St0A = luerl:init(),
    % execute a string
    luerl:do("print(\"(1) Hello, Robert!\")", St0A),
    luerl:do(<<"print(\"(2) Hello, Roberto!\")">>, St0A),
    
    % execute a string, get a result
    {ok,A,_} = luerl:do("return 1 + 1", St0A),
    {ok,A,_} = luerl:do(<<"return 1 + 1">>, St0A),
    io:format("(5) 1 + 1 = ~p!~n", [A]),

    % execute a file
    luerl:dofile("./hello2-1.lua", St0A),

    % execute a file, get a result
    {ok,B,_} = luerl:dofile("./hello2-2.lua", St0A),
    io:format("(7) 2137 * 42 = ~p?~n", [B]),

    % execute a standard function
    luerl:call_function_dec([print],
                            [<<"(8) Hello, standard print function!">>], St0A),
    luerl:call_function_dec([print],
                            [<<"(9) Hello, standard print function!">>], St0A),
    {ok,Result1,_} = luerl:call_function_dec([table,pack],
                                             [<<"a">>,<<"b">>,42], St0A),
    {ok,Result1,_} = luerl:call_function_dec([table,pack],
                                             [<<"a">>,<<"b">>,42], St0A),
    io:format("(10) ~p?~n", [Result1]),

    % separately parse, then execute (doubles (11) and Chunk1 as assertion)
    St1A = luerl:init(),
    {ok,Chunk1,St1B} = luerl:load("print(\"(11) Hello, Chunk 1!\")", St1A),
    {ok,Chunk1,_} = luerl:load(<<"print(\"(11) Hello, Chunk 1!\")">>, St1A),
    luerl:call_chunk(Chunk1, [], St1B),

    % separately parse, then execute (doubles (12) and Chunk2 as assertion)
    St2A = luerl:init(),
    {ok,Chunk2,St2B} = luerl:load("function chunk2() print(\"(12) Hello, Chunk 2!\") end", St2A),
    {ok,Chunk2,_} = luerl:load(<<"function chunk2() print(\"(12) Hello, Chunk 2!\") end">>, St2A),
    {ok,Result2,St2C} = luerl:call_chunk(Chunk2, [], St2B),
    {ok,Result2,St2D} = luerl:do(<<"function chunk2() print(\"(12) Hello, Chunk 2!\") end">>, St2A),
    luerl:call_function_dec([chunk2], [], St2C),
    luerl:call_function_dec([chunk2], [], St2D),

    % separately parse, then execute a file. The file defines a function no()
    St3A = luerl:init(),
    {ok,Chunk3,St3B} = luerl:loadfile("./hello2-3.lua", St3A),
    {ok,_Result3,St3C} = luerl:call_chunk(Chunk3, St3B),
    {ok,[],_} = luerl:call_function_dec([no], [], St3C),

    % separately parse, then execute, get a result
    St4A = luerl:init(),
    {ok,Chunk4,St4B} = luerl:load("return '(17b) Marvelous wheater today, isn°t it!'", St4A),
    {ok,Chunk4,_} = luerl:load(<<"return '(17b) Marvelous wheater today, isn°t it!'">>, St4A),
    {ok,Result4,_} = luerl:call_chunk(Chunk4, [], St4B),
    io:format("(17) And I say: ~p~n", [Result4]),

    % separately parse, then execute a file, get a result
    St5A = luerl:init(),
    {ok,Chunk5,St5B} = luerl:loadfile("./hello2-4.lua", St5A),
    {ok,Result5,_} = luerl:call_chunk(Chunk5, St5B),
    io:format("(18) And he says: ~p~n", [Result5]),


    % Same as above, passing State in all times.

    % create state
    New = luerl:init(),
    {ok,_,_New2} = luerl:do("print '(19) hello generix'", New),
    
    % change state
    {ok,_,State0} = luerl:do("a = 1000", New),
    {ok,_,State01} = luerl:do("a = 1000", New),

    % execute a string, using passed in State0
    luerl:do("print('(22) ' .. a+2)", State0),
    luerl:do(<<"print('(23) ' .. a+3)">>, State0),
    
    % execute a string, get a result from passed in State0
    {ok,E,_} = luerl:do("return 4 * a", State0),
    {ok,E,_} = luerl:do(<<"return 4 * a">>, State0),
    io:format("(24) 4 x a = ~p!~n", [E]),
   
    % execute a string, get a result, change State0
    {ok,Z,State02} = luerl:do("a = 123; return a * 3", State01),
    {ok,Z,State03} = luerl:do(<<"return (3 * a)">>, State02),
    io:format("(25) a = ~p~n", [Z]),
   
    % execute a file using passed in state
    luerl:dofile("./hello2-5.lua", State03),

    % execute a file that changes the State0
    {ok,_,State04} = luerl:dofile("./hello2-6.lua", State03),
    luerl:do("print('(27) (b) ' .. a)", State04),

    % execute a file, get a result
    {ok,F,State05} = luerl:dofile("./hello2-7.lua", State04),
    io:format("(28) F: ~ts~n", [F]),

    % execute a file that changes the State0, and get a value back
    {ok,F,State06} = luerl:dofile("./hello2-7.lua", State05),
    io:format("(29) F: ~ts = ", [F]),
    luerl:do("print('(30) F: ' .. a)", State06),

    % separately parse, then execute
    {ok,Chunk11,_} = luerl:load("print(\"(31) Hello, \" .. a .. \"!\")", State06),
    {ok,Chunk11,State07} = luerl:load(<<"print(\"(31) Hello, \" .. a .. \"!\")">>, State06),
    luerl:call_chunk(Chunk11,State07),

    % separately parse, then execute a file. The file defines a function old()
    {ok,Chunk12,St7} = luerl:loadfile("./hello2-8.lua", State07),
    {ok,_Result12,State07A} = luerl:call_chunk(Chunk12,St7),
    luerl:call_function_dec([old],[],State07A),

    % separately parse, then execute, get a result
    {ok,Chunk13,St8} = luerl:load("a = '(30a)' .. a .. ' (this is Greek)'; return a", State07),
    {ok,Chunk13,_} = luerl:load(<<"a = '(30a)' .. a .. ' (this is Greek)'; return a">>, State07),
    {ok,Result07,State08} = luerl:call_chunk(Chunk13, St8),
    io:format("(34) And again I said: ~ts~n", [Result07]),

    % separately parse, then execute a file, get a result. The file defines confirm(p)
    {ok,Chunk14,St9} = luerl:loadfile("./hello2-9.lua", State08),
    {ok,Result14,State14} = luerl:call_chunk(Chunk14, St9),
    io:format("(35) And twice: ~ts~n", [Result14]),
    {ok,Result14A,_} = luerl:call_function_dec([confirm],
                                               [<<"Is it?">>], State14),
    io:format("(36) Well: ~ts~n", [Result14A]),

    % execute a file, get the decoded result of a table
    {ok,Result15,_} = luerl:dofile("./hello2-10.lua", State14),
    io:format("(37) Decoded table: ~p~n", [Result15]),
 
    io:format("done~n").
