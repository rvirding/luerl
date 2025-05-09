%% Copyright (c) 2013-2024 Robert Virding
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

?MODULEDOC(false).

-export([install/1, clock/3, date/3, difftime/3, execute/3, lua_exit/3, getenv/3, remove/3, rename/3, time/3, tmpname/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).       %Shorten this

%% For `remove/2'.
-include_lib("kernel/include/file.hrl").

%% For `tmpname/2' in `luerl_lib_os'.
-define(TMPNAM_MAXTRIES, 100).
-define(TMPNAM_TEMPLATE(S), "/tmp/lua_" ++ S).

install(St) ->
    luerl_heap:alloc_table(table(), St).

table() ->
    [{<<"clock">>,#erl_mfa{m=?MODULE,f=clock}},
     {<<"date">>,#erl_mfa{m=?MODULE,f=date}},
     {<<"difftime">>,#erl_mfa{m=?MODULE,f=difftime}},
     {<<"execute">>,#erl_mfa{m=?MODULE,f=execute}},
     {<<"exit">>,#erl_mfa{m=?MODULE,f=lua_exit}},
     {<<"getenv">>,#erl_mfa{m=?MODULE,f=getenv}},
     {<<"remove">>,#erl_mfa{m=?MODULE,f=remove}},
     {<<"rename">>,#erl_mfa{m=?MODULE,f=rename}},
     {<<"time">>,#erl_mfa{m=?MODULE,f=time}},
     {<<"tmpname">>,#erl_mfa{m=?MODULE,f=tmpname}}].

getenv(_, [<<>>|_], St) -> {[nil],St};
getenv(_, [A|_], St) when is_binary(A) ; is_number(A) ->
    case os:getenv(luerl_lib:arg_to_list(A)) of
        Env when is_list(Env) ->
            {[list_to_binary(Env)],St};
        false -> {[nil],St}
    end;
getenv(_, As, St) -> badarg_error(getenv, As, St).

%% execute([Command|_], State) -> {[Ret,Type,Stat],State}.
%%  Execute a command and get the return code. We cannot yet properly
%%  handle if our command terminated with a signal.

execute(_, [], St) -> {true,St};                   %We have a shell
execute(_, [A|_], St) ->
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
execute(_, As, St) -> badarg_error(execute, As, St).

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
lua_exit(_, [], St) ->
    lua_exit(nil, [true,false], St);
lua_exit(_, [C], St) ->
    lua_exit(nil, [C,false], St);
lua_exit(_, [Co0|_], St) -> %% lua_exit([Co0,Cl0], St) ->
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
tmpname(_, [_|_], St) ->
    %% Discard extra arguments.
    tmpname(nil, [], St);
tmpname(_, [], St) ->
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
rename(_, [S,D|_], St) ->
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
rename(_, As, St) -> badarg_error(rename, As, St).

%% remove([Path|_], State)
%%  Deletes the file (or empty directory) with the given `Path'. If this
%%  function fails, it returns `nil' plus a string describing the error, and the
%%  error code. Otherwise, it returns `true'.
remove(_, [A|_], St) ->
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
remove(_, As, St) -> badarg_error(remove, As, St).


%% Utility function to get a preformatted list to return from `remove/2'.
remove_geterr(R, F) ->
    F1 = binary_to_list(F),
    #{errno := En,
      errstr := Er} = luerl_util:errname_info(R),
    [nil, list_to_binary(F1 ++ ": " ++ Er), En].

%% Time and date functions.

clock(_, As, St) ->
    Type = case As of                           %Choose which we want
               [<<"runtime">>|_] -> runtime;
               _ -> wall_clock
           end,
    {Tot,_} = erlang:statistics(Type),          %Milliseconds
    {[Tot*1.0e-3],St}.

date(ConfArg, [], St) ->
    date(ConfArg, [<<"%c">>], St);
date(ConfArg, [Fmt], St) when is_binary(Fmt) ->
    date(ConfArg, [Fmt, current_timestamp()], St);
date(_, [Fmt, TimeStamp], St) when is_binary(Fmt) and is_number(TimeStamp) ->
    DateTime = timestamp_to_datetime(TimeStamp),
    Formatted = luerl_lib_os_date:format(DateTime, Fmt),
    {Enc, St1} = luerl:encode(Formatted, St),
    {[Enc],St1};
date(_, As, St) ->
    badarg_error(date, As, St).

difftime(_, [T2,T1|_], St) ->
    {[T2 - T1],St};
difftime(_, As, St) -> badarg_error(difftime, As, St).

time(_, As=[#tref{}=Tref], St) ->
    L = luerl:decode(Tref, St),
    compute_time(proplists:to_map(L), As, St);
time(_, _, St) ->                                  %Time since 1 Jan 1970
    {[current_timestamp()],St}.

compute_time(Map=#{<<"year">> := Y, <<"month">> := Mth, <<"day">> := D}, _, St) ->
    H = maps:get(<<"hour">>, Map, 12),
    Min = maps:get(<<"min">>, Map, 0),
    S = maps:get(<<"sec">>, Map, 0),
    LocalEpoch = calendar:universal_time_to_local_time({{1970,1,1},{0,0,0}}),
    Result = calendar:datetime_to_gregorian_seconds({{Y, Mth, D}, {H, Min, S}}) - calendar:datetime_to_gregorian_seconds(LocalEpoch),
    {[Result],St};
compute_time(Map, _As, St) ->
    MissingArg = lists:foldl(fun(K,Acc=undefined) -> case maps:is_key(K, Map) of
                                                         false -> K;
                                                         true -> Acc
                                                     end;
                                (_,Acc) -> Acc end, undefined, [<<"day">>, <<"month">>, <<"year">>]),
    badarg_error(time, MissingArg, St).

current_timestamp() ->
    {Mega,Sec,Micro} = os:timestamp(),
    1.0e6*Mega+Sec+Micro*1.0e-6.

timestamp_to_datetime(Timestamp) ->
    SecondsSinceEpoch = round(Timestamp),
    calendar:system_time_to_local_time(SecondsSinceEpoch, second).
