%% File    : hello_sandbox.erl
%% Purpose : Brief demonstration of Luerl sandbox basics.
%% Use     $ erlc hello_sandbox.erl && erl -pa ./ebin -s hello_sandbox run -s init stop -noshell
%% Or      $ make hello_sandbox

-module(hello_sandbox).
-export([run/0]).

run() ->
    %% sandboxing globals
    St0 = luerl_sandbox:init(),
    {error, {lua_error, Reason, _}} = luerl_sandbox:run("return os.getenv(\"HOME\")"),
    io:format("os.getenv with sandbox: ~p~n",[Reason]),

    %% customizing sandbox
    {[<<"number">>], _} = luerl_sandbox:run("return type(1)", luerl:init()),
    {error,  {lua_error, _, _}} = luerl_sandbox:run("return type(1)", luerl_sandbox:init([['_G', type]])),

    %% using sandboxed state outside of runner
    try
        luerl:do("return os.getenv(\"HOME\")", St0)
    catch
        _:_ ->
            io:format("catch error with os.getenv(\"HOME\") with sandbox~n", [])
    end,

    %% script runner with reduction counting and process flags
    MaxReductions = 100,
    ProcessFlags = [{priority, low}],
    Timeout = 1000,
    {error, {reductions, R0}} = luerl_sandbox:run("a={}; for i=1,1000000 do a[i] = 5 end", St0, MaxReductions),
    io:format("killed process with reductions ~p > 100~n",[R0]),
    {error, {reductions, R1}} = luerl_sandbox:run("x = 'a'; while true do x = x .. x end", luerl:init(), MaxReductions, ProcessFlags, Timeout),
    io:format("killed process with reductions ~p > 100~n",[R1]),

    %% unlimited reductions
    UnlimitedReductions = 0,
    {[], _} = luerl_sandbox:run("a={}; for i=1,10 do a[i] = 5 end", St0, UnlimitedReductions),
    io:format("Finished running with unlimited reductions ~n",[]),

    done.


