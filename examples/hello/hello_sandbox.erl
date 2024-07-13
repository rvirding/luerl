%% File    : hello_sandbox.erl
%% Purpose : Brief demonstration of Luerl sandbox basics.
%% Use     $ erlc hello_sandbox.erl && erl -pa ./ebin -s hello_sandbox run -s init stop -noshell
%% Or      $ make hello_sandbox

-module(hello_sandbox).
-export([run/0]).

run() ->
    %% Default sandboxed state.
    SbSt = luerl_sandbox:init(),

    %% Sandboxing globals
    {error, {lua_error, Reason, _}} =
        luerl_sandbox:run("return os.getenv(\"HOME\")", SbSt),
    io:format("os.getenv with sandbox: ~p~n",[Reason]),

    %% Customizing sandbox
    %%  first with default Luerl state.
    {[<<"number">>], _} = luerl_sandbox:run("return type(1)", luerl:init()),

    %%  then with sandboxed type function
    {error,  {lua_error, _, _}} =
        luerl_sandbox:run("return type(1)", luerl_sandbox:init([['_G', type]])),

    %% Using sandboxed state outside of runner
    try
        luerl:do("return os.getenv(\"HOME\")", SbSt)
    catch
        _:_ ->
            io:format("catch error with os.getenv(\"HOME\") with sandbox~n", [])
    end,

    %% Setting values.
    MaxReductions = 100,
    SpawnOpts = [{priority, low}],
    Timeout = 1000,

    %% Script runner with reduction counting.
    Flags0 = #{max_reductions => MaxReductions},
    {error, {reductions, R0}} =
        luerl_sandbox:run("a={}; for i=1,1000000 do a[i] = 5 end",
                          Flags0, SbSt),
    io:format("killed process with reductions ~p > 100~n",[R0]),

    %% Sandboxed run with default Luerl state.
    Flags1 = #{max_reductions => MaxReductions,
               spawn_opts => SpawnOpts,
               max_time => Timeout},
    {error, {reductions, R1}} =
        luerl_sandbox:run("x = 'a'; while true do x = x .. x end",
                          Flags1, luerl:init()),
    io:format("killed process with reductions ~p > 100~n",[R1]),

    %% Unlimited reductions
    Flags3 = #{max_reductions => none},
    {[], _} = luerl_sandbox:run("a={}; for i=1,10 do a[i] = 5 end",
                                Flags3, SbSt),
    io:format("Finished running with unlimited reductions ~n",[]),

    done.
