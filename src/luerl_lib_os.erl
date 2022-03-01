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

-import(luerl_lib, [lua_error/2,badarg_error/3]).       %Shorten this

%% For `remove/2'.
-include_lib("kernel/include/file.hrl").

%% For `tmpname/2' in `luerl_lib_os'.
-define(TMPNAM_MAXTRIES, 100).
-define(TMPNAM_TEMPLATE(S), "/tmp/lua_" ++ S).

install(St) ->
    luerl_heap:alloc_table(table(), St).

table() ->
    [{<<"clock">>,#erl_func{code=fun clock/2}},
     {<<"date">>,#erl_func{code=fun date/2}},
     {<<"difftime">>,#erl_func{code=fun difftime/2}},
     {<<"execute">>,#erl_func{code=fun execute/2}},
     {<<"exit">>,#erl_func{code=fun lua_exit/2}},
     {<<"getenv">>,#erl_func{code=fun getenv/2}},
     {<<"remove">>,#erl_func{code=fun remove/2}},
     {<<"rename">>,#erl_func{code=fun rename/2}},
     {<<"time">>,#erl_func{code=fun time/2}},
     {<<"tmpname">>,#erl_func{code=fun tmpname/2}}].

getenv([<<>>|_], St) -> {[nil],St};
getenv([A|_], St) when is_binary(A) ; is_number(A) ->
    case os:getenv(luerl_lib:arg_to_list(A)) of
        Env when is_list(Env) ->
            {[list_to_binary(Env)],St};
        false -> {[nil],St}
    end;
getenv(As, St) -> badarg_error(getenv, As, St).

%% execute([Command|_], State) -> {[Ret,Type,Stat],State}.
%%  Execute a command and get the return code. We cannot yet properly
%%  handle if our command terminated with a signal.

execute([], St) -> {true,St};                   %We have a shell
execute([A|_], St) ->
    case luerl_lib:arg_to_string(A) of
        S when is_binary(S) ->
            Opts = [{arg0,"sh"},{args,["-c", S]},
                    hide,in,eof,exit_status,use_stdio,stderr_to_stdout],
            P = open_port({spawn_executable,"/bin/sh"}, Opts),
            N = execute_handle(P),
            Ret = if N =:= 0 -> true;           %Success
                     true -> nil                %Error
                  end,
            {[Ret,<<"exit">>,N],St};
        error -> badarg_error(execute, [A], St)
    end;
execute(As, St) -> badarg_error(execute, As, St).

execute_handle(P) ->
    receive
        {P,{data,D}} ->
            %% Print stdout/stderr like Lua does.
            io:put_chars(D),
            execute_handle(P);
        {P, {exit_status,N}} ->
            %% Wait for the eof then close the port.
            receive
                {P, eof} ->
                    port_close(P),
                    N
            end
    end.

%% exit([ExitCode,CloseState|_], State) -> nil.
%%  Exit the host program. If ExitCode is true, the return code is 0;
%%  if ExitCode is false, the return code is 1; if ExitCode is a number, the
%%  return code is this number. The default value for ExitCode is true.
%% NOT IMPLEMENTED:
%%  If the optional second argument CloseState is true, it will close the Lua
%%  state before exiting.
lua_exit([], St) ->
    lua_exit([true,false], St);
lua_exit([C], St) ->
    lua_exit([C,false], St);
lua_exit([Co0|_], St) -> %% lua_exit([Co0,Cl0], St) ->
    Co1 = case luerl_lib:arg_to_number(Co0) of
              X when is_integer(X) -> X;
              error ->
                  case Co0 of
                      false -> 1;
                      true -> 0;
                      error -> badarg_error(exit, [Co0], St)
                  end
          end,

    %% Uncomment this if you need the second argument to determine whether to
    %% destroy the Lua state or not.
    %% Cl1 = case Cl0 of
    %%           true -> true;
    %%           false -> false;
    %%           _ -> badarg_error(exit, [Cl0], St)
    %%       end,

    erlang:halt(Co1).

%% tmpname([], State)
%% Faithfully recreates `tmpnam'(3) in lack of a NIF.
tmpname([_|_], St) ->
    %% Discard extra arguments.
    tmpname([], St);
tmpname([], St) ->
    Out = tmpname_try(randchar(6, []), 0),
    %% We make an empty file the programmer will have to close themselves.
    %% This is done for security reasons.
    file:write_file(Out, ""),
    {[list_to_binary(Out)],St}.

%% Support function for `tmpname/2' - generates a random filename following a
%% template.
tmpname_try(_, ?TMPNAM_MAXTRIES) ->
    %% Exhausted...
    false;
tmpname_try(A, N) ->
    case file:read_file_info(?TMPNAM_TEMPLATE(A)) of
        {error,enoent} -> ?TMPNAM_TEMPLATE(A); %% Success, at last!
        _ -> tmpname_try(randchar(6, []), N+1)
    end.

%% Support function for `tmpname_try/2'.
randchar(0, A) -> A;
randchar(N, A) -> randchar(N-1, [rand:uniform(26)+96|A]).

%% rename([Source,Destination|_], State)
%%  Renames the file or directory `Source' to `Destination'. If this function
%%  fails, it returns `nil', plus a string describing the error code and the
%%  error code. Otherwise, it returns `true'.
rename([S,D|_], St) ->
    case {luerl_lib:arg_to_string(S),
          luerl_lib:arg_to_string(D)} of
        {S1,D1} when is_binary(S1) ,
                     is_binary(D1) ->
            case file:rename(S1,D1) of
                ok -> {[true],St};
                {error,R} ->
                    #{errno := En,
                      errstr := Er} = luerl_util:errname_info(R),
                    {[nil,Er,En],St}
            end;

        %% These are for throwing a `badmatch' error on the correct argument.
        {S1,D1} when not is_binary(S1) ,
                     not is_binary(D1) ->
            badarg_error(rename, [S1,D1], St);
        {S1,D1} when not is_binary(S1) ,
                     is_binary(D1) ->
            badarg_error(rename, [S1], St);
        {S1,D1} when is_binary(S1) ,
                     not is_binary(D1) ->
            badarg_error(rename, [D1], St)
    end;
rename(As, St) -> badarg_error(rename, As, St).

%% remove([Path|_], State)
%%  Deletes the file (or empty directory) with the given `Path'. If this
%%  function fails, it returns `nil' plus a string describing the error, and the
%%  error code. Otherwise, it returns `true'.
remove([A|_], St) ->
    case luerl_lib:arg_to_string(A) of
        A1 when is_binary(A1) ->
            %% Emulate the underlying call to `remove(3)'.
            case file:read_file_info(A1) of
                {ok,#file_info{type=T}} when T == directory ;
                                             T == regular ->
                    %% Select the corresponding function.
                    Op = if T == directory -> del_dir;
                            true -> delete
                         end,

                    case file:Op(A) of
                        ok -> {[true],St};
                        {error,R} -> {remove_geterr(R, A), St}
                    end;
                {error,R} ->
                    %% Something went wrong.
                    {remove_geterr(R, A), St}
            end;
        error -> badarg_error(remove, [A], St)
    end;
remove(As, St) -> badarg_error(remove, As, St).


%% Utility function to get a preformatted list to return from `remove/2'.
remove_geterr(R, F) ->
    F1 = binary_to_list(F),
    #{errno := En,
      errstr := Er} = luerl_util:errname_info(R),
    [nil, list_to_binary(F1 ++ ": " ++ Er), En].

%% Time functions.

clock(As, St) ->
    Type = case As of                           %Choose which we want
               [<<"runtime">>|_] -> runtime;
               _ -> wall_clock
           end,
    {Tot,_} = erlang:statistics(Type),          %Milliseconds
    {[Tot*1.0e-3],St}.

date(_, St) ->
    {{Ye,Mo,Da},{Ho,Mi,Sec}} = calendar:local_time(),
    Str = io_lib:fwrite("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                        [Ye,Mo,Da,Ho,Mi,Sec]),
    {[iolist_to_binary(Str)],St}.

difftime([A1,A2|_], St) ->
    {[A2-A1],St};
difftime(As, St) -> badarg_error(difftime, As, St).


time(_, St) ->                                  %Time since 1 Jan 1970
    {Mega,Sec,Micro} = os:timestamp(),
    {[1.0e6*Mega+Sec+Micro*1.0e-6],St}.
