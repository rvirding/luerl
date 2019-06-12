%% Copyright (c) 2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : luerl_lib_os.erl
%% Author  : Robert Virding
%% Purpose : The os library for Luerl.

-module(luerl_lib_os).

-include("luerl.hrl").

-export([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St) ->
    luerl_emul:alloc_table(table(), St).

table() ->
    [{<<"clock">>,#erl_func{code=fun clock/2}},
     {<<"date">>,#erl_func{code=fun date/2}},
     {<<"difftime">>,#erl_func{code=fun difftime/2}},
     {<<"execute">>,#erl_func{code=fun execute/2}},
     {<<"getenv">>,#erl_func{code=fun getenv/2}},
     {<<"time">>,#erl_func{code=fun time/2}}].

getenv([<<>>|_], St) -> {[nil],St};
getenv([A|_], St) when is_binary(A) ; is_number(A) ->
    case os:getenv(luerl_lib:to_list(A)) of
	Env when is_list(Env) ->
	    {[list_to_binary(Env)],St};
	false -> {[nil],St}
    end;
getenv(As, St) -> badarg_error(getenv, As, St).

%% Execute a command and get the return code.
execute([<<>>], St) -> {127,St};
execute([A], St) ->
    case A of
        S when is_binary(S) ->
            Opts = [{args,["-c", S]},hide,in,eof,exit_status,use_stdio,
                    stderr_to_stdout],
            P = open_port({spawn_executable,"/bin/sh"}, Opts),
            %% Print stdout/stderr like Lua does.
            {N,So} = execute_handle(P),
            io:format(So),
            O = case N of
                    0 -> true;
                    _ -> nil
                end,
            {[O],St};
        false -> {[nil],St}
    end.

execute_handle(P) -> execute_handle(P, []).

execute_handle(P, D) ->
    receive
        {P,{data,D1}} -> execute_handle(P,[D1|D]);
        {P, eof} ->
            port_close(P),
            receive
                {P,{exit_status,N}} -> {N,D}
            end
    end.

%% Time functions.

clock(As, St) ->
    Type = case As of				%Choose which we want
               [<<"runtime">>|_] -> runtime;
               _ -> wall_clock
           end,
    {Tot,_} = erlang:statistics(Type),		%Milliseconds
    {[Tot*1.0e-3],St}.

date(_, St) ->
    {{Ye,Mo,Da},{Ho,Mi,Sec}} = calendar:local_time(),
    Str = io_lib:fwrite("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                        [Ye,Mo,Da,Ho,Mi,Sec]),
    {[iolist_to_binary(Str)],St}.

difftime([A1,A2|_], St) ->
    {[A2-A1],St}.

time(_, St) ->					%Time since 1 Jan 1970
    {Mega,Sec,Micro} = os:timestamp(),
    {[1.0e6*Mega+Sec+Micro*1.0e-6],St}.
