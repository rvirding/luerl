%% Copyright (c) 2013-2018 Robert Virding
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

%% File    : luerl.erl
%% Authors : Balazs Nyiro
%% Purpose : debugger console for another Luerl node()
%%           to follow the program execution

-module(luerl_debugger).

-export([start/0]).

%% This function is executed after 'rebar3 shell', to connect to another
%% node() and send commands to control the program execution, to read
%% variables and in the future maybe modify the variables.

%% TODO: follow gdb commands, if you can

start() ->
  io:fwrite("== Luerl Debugger ==\n"),

  {ok, Hostname} = inet:gethostname(),
  {ok,{hostent,FullHostname,[],inet,_,[_]}} = inet:gethostbyname(Hostname),

  io:fwrite("FullHostname: ~p\n", [FullHostname]),
  net_kernel:start([list_to_atom("debugger@"++FullHostname), longnames]),

  ErlCookie = user_input_get("Please Give me the program executer Luerl node's cookie!"),
  erlang:set_cookie(node(), list_to_atom(ErlCookie)),

  register(luerl_debugger, self()),

  waiting_for_server_message(),
  ok.

waiting_for_server_message() ->
  receive
    M ->
      io:format("Debugger received: ~p~n",[M]),
      waiting_for_server_message()
  after 6000000 ->
    io:format("Timeout, Debugger is finished"),
    ok
  end.
debugger_do() ->
  Task = user_input_get(),
  debugger_do(Task).

debugger_do(Task) ->
  io:fwrite("Task: " + Task),
  debugger_do().

user_input_get() ->
  user_input_get("Luerl Debugger").

user_input_get(Msg) ->
  {ok,[UserAnswer]} = io:fread(Msg ++ " > ", "~s"),
  UserAnswer.
