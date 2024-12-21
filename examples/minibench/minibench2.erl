%% File    : minibench2.erl
%% Author  : Henning Diedrich
%% File    : luerl/examples/minibench/minibench.erl
%% Purpose : Benchmark for frequent calls to small Luerl scripts
%% Author  : Henning Diedrich
%% Use     $ cd ./examples/minibench 
%%         $ erlc minibench.erl 
%%         $ erl -pa ../../ebin -s minibench run -s init stop -noshell
%% Or      $ make minibench

-module(minibench2).
-export([run/0]).

run() ->

    io:format("----------------------------------------------------------~n"),
    io:format("This is a benchmark of frequent fast calls into Luerl.~n"),

    % I. eval and execute
    DoStr1 = "a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c",
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, parse and execute '~s'~n", [DoStr1]),
    I1 = 10000,
    {T1,_State} = timer:tc(fun() -> do_loop(I1, DoStr1) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n",
              [T1,I1,DoStr1]),
    io:format("Per call: ~p microseconds.~n", [T1/I1]),
    

    % II. eval once, then only execute
    DoStr2 = "a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c",
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, and execute pre-parsed '~s'~n", [DoStr2]),
    I2 = 10000,
    {ok, Chunk2, State2} = luerl:load(DoStr2, luerl:init()),
    
    {T2,_State21} = timer:tc(fun() -> do_loop_state(I2, Chunk2, State2) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n",
              [T2,I2,DoStr2]),
    io:format("Per call: ~p microseconds.~n", [T2/I2]),
    

    % III. eval once, then only execute
    DoStr3 = "a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c",
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parse execute '~s', re-using same state~n",
              [DoStr3]),
    I3 = 10000,
    {ok, Chunk3, State3} = luerl:load(DoStr3, luerl:init()),
    
    {T3,_State31} = timer:tc(fun() -> do_loop_state(I3, Chunk3, State3) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n",
              [T3,I3,DoStr3]),
    io:format("Per call: ~p microseconds.~n", [T3/I3]),


    % IV. measure but state initialization
    io:format("----------------------------------------------------------~n"),
    io:format("Pure initialization of Lua state~n"),
    I4 = 10000,
    
    {T4,_State41} = timer:tc(fun() -> [luerl:init() || _ <- lists:seq(1,I4)] end),

    io:format("Adding Up: ~p microseconds for ~p x initializing a Lua state.~n", [T4,I4]),
    io:format("Per call: ~p microseconds.~n", [T4/I4]),


    % V. eval once, then only execute, re-use previous state
    DoStr5 = "a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c",
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed '~s', re-using state from last result~n",
              [DoStr5]),
    I5 = 10000,
    {ok, Chunk5, State5} = luerl:load(DoStr5, luerl:init()),
    
    {T5,_State51} = timer:tc(fun() -> do_loop_chain(I5, Chunk5, State5) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n", [T5,I5,DoStr5]),
    io:format("Per call: ~p microseconds.~n", [T5/I5]),

    % Vb. function call, re-use previous state
    DoStr5b = "function OneAndOne() a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c end",
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed function '~s', re-using state from last result~n",
              [DoStr5b]),
    I5b = 10000,
    State5b = luerl:init(),
    {ok,[],State5b1} = luerl:do(DoStr5b, State5b),
    io:format("-"),
    {T5b,_State5b1} = timer:tc(fun() -> do_loop_do(I5b, "return OneAndOne()", State5b1) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n",
              [T5b,I5b,DoStr5b]),
    io:format("Per call: ~p microseconds.~n", [T5b/I5b]),

    % Vc. empty function call, re-use previous state
    DoStr5c = "function EmptyFunc() end",
    io:format("----------------------------------------------------------~n"),
    io:format("Execute empty function, re-using state from last result~n"),
    I5c = 10000,
    State5c = luerl:init(),
    {ok,[],State5c1} = luerl:do(DoStr5c, State5c),
    io:format("-"),
    {T5c,_State5c1} = timer:tc(fun() -> do_loop_do(I5c, "EmptyFunc()", State5c1) end),

    io:format("Adding Up: ~p microseconds for ~p x calling empty function.~n",
              [T5c,I5c]),
    io:format("Per call: ~p microseconds.~n", [T5c/I5c]),
    
    % VI. measure but parsing
    DoStr6 = "a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c",
    io:format("----------------------------------------------------------~n"),
    io:format("Pure parsing~n"),
    I6 = 10000,
    {T6,_State61} = timer:tc(fun() -> [luerl:load(DoStr6, luerl:init()) ||
                                          _ <- lists:seq(1,I6)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n", [T6,I6,DoStr6]),
    io:format("Per call: ~p microseconds.~n", [T6/I6]),


    % VII. Parse and execute
    DoStr7 = "a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c",
    io:format("----------------------------------------------------------~n"),
    io:format("Parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state~n"),
    I7 = 10000,
    State7 = luerl:init(),
    {T7,_State71} = timer:tc(fun() -> do_loop_do(I7, DoStr7, State7) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of '~s'.~n", [T7,I7,DoStr7]),
    io:format("Per call: ~p microseconds.~n", [T7/I7]),


    done.

% helper
do_loop(N, Chunk) when N > 0 ->
    luerl:do(Chunk, luerl:init()),
    do_loop(N-1, Chunk);
do_loop(0, _) -> ok.

do_loop_state(N, Chunk, State) when N > 0 ->
    luerl:call_chunk(Chunk, [], State),
    do_loop_state(N-1, Chunk, State);
do_loop_state(0, _, _) -> ok.

do_loop_do(N, String, State) when N > 0 ->
    {ok,_,State1} = luerl:do(String, State),
    do_loop_do(N-1, String, State1);
do_loop_do(0, _, _) -> ok.

do_loop_chain(N, Chunk, State0) when N > 0 ->
    {ok,_,State1} = luerl:call_chunk(Chunk, State0),
    do_loop_chain(N-1, Chunk, State1);
do_loop_chain(0, _, _) -> ok.
