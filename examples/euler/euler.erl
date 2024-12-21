%% File    : euler.erl
%% Purpose : Running complex lua with luerl
%% Use: erlc -I ../../src euler.erl && erl -pa ../../ebin -s euler run -s init stop -noshell
%% Or: make

-module(euler).
-export([run/0, run/2]).

run() ->
  run("./problem_001.lua", 233168),
  run("./problem_002.lua", 4613732),
  run("./problem_003.lua", 29),
  run("./problem_004.lua", 36863),
  run("./problem_005.lua", 232792560),
  run("./problem_006.lua", 25164150),
  run("./problem_007.lua", 617),
  run("./problem_008.lua", 32),
  run("./problem_009.lua", 31875000),
  run("./problem_010.lua", 277050.0),
  ok.

run(File, Solution) ->
    Lua0 = luerl:init(),
    {ok, Form, Lua1} = luerl:loadfile(File, Lua0),
    case timer:tc(luerl, call_chunk, [Form, [], Lua1]) of
	{T, {ok, [Return], _}} when Return == Solution -> 
	    io:format("~s (returned ~p in ~pus)~n", [File, Return, T]);
	{T, {ok, [Return], _}} -> 
	    io:format("~s (expected ~p but got ~p in ~pus)~n",
		      [File, Solution, Return, T]);
	{_, {lua_error, Error, State}} ->
	    io:format("luerl error: ~p~n", [{Error,State}]);
	{_, {error, Error, State}} ->
	    io:format("Error: ~p~n", [{Error,State}])
    end.
