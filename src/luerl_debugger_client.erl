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

%% File    : luerl_debugger_client.erl
%% Authors : Balazs Nyiro
%% Purpose : debugger console for another Luerl node()
%%           to follow the program execution

-module(luerl_debugger_client).

-export([start/0]).




%% This function is executed after 'rebar3 shell', to connect to another
%% node() and send commands to control the program execution, to read
%% variables and in the future maybe modify the variables.

%% TODO: follow gdb commands, if you can

start() ->
  io:fwrite("== Luerl Debugger ==\n"),
  io:fwrite("start 'epmd' (Erlang Port Mapper Daemon) in command line, in a separated terminal, manually, "),
  io:fwrite("if you see this error message: \nnotice: Protocol 'inet_tcp': register/listen error: econnrefused  erlang\n\n"),

  io:fwrite("\nPlease set the server side after 'rebar3 shell' in Luerl console if you want to try the debugger out: \nluerl_debugger_emulatorside:nodename_cookie_set().\n\n"),

  nodename_cookie_set(),
  waiting_for_server_message(),

  ok.


nodename_cookie_set() ->
  % this is a common function to set nodename and cookie
  luerl_debugger_emulatorside:nodename_cookie_set_general(
    "== Set Cookie for debugger_client ==",
    luerl_debugger_client,
    "luerl_debugger_client").

msg_send_to_emulatorside(Msg) ->
  luerl_debugger_emulatorside:msg_send(Msg, "luerl_emulator", luerl_emulator).

waiting_for_server_message() ->
  io:fwrite("Waiting for Luerl emulator's message..."),
  receive
    M ->
      io:format("Debugger received: ~p~n",[M]),
      msg_send_to_emulatorside({client_message, execute_next_instruction, no_data}),
      waiting_for_server_message()
  after 6000000 ->
    io:format("Timeout, Debugger is finished"),
    ok
  end.
debugger_do() ->
  Task = luerl_debugger_emulatorside:user_input_get(),
  debugger_do(Task).

debugger_do(Task) ->
  io:fwrite("Task: " + Task),
  debugger_do().

