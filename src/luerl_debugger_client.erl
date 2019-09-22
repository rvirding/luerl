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
  io:fwrite("then execute this sample lua prg: \nluerl:dofile(\"test/luerl_return_SUITE_data/simple_return_multi.lua\").\n\n\n"),

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
  io:fwrite("msg send to emulatorside: ~p\n", [Msg]),
  luerl_debugger_emulatorside:msg_send(Msg, "luerl_emulator", luerl_emulator).

waiting_for_server_message() ->

  % DISCUSS IT WITH FECO: why does this line is important here?
  msg_send_to_emulatorside({execute_next_instruction, no_data}),

  io:fwrite("Waiting for Luerl emulator's message...\n"),
  receive
    MsgServer ->
      io:format("Debugger received: ~p\n",[MsgServer]),
      case MsgServer of
        {luerl_emul, coverage, instruction_inside_executed, {LuaFilePath, LineNum} } ->
          file_display(LuaFilePath, LineNum);
        _ ->
          io:format("Not handled server message\n",[])
      end

  after 6000000 ->
    io:format("Timeout, Debugger is finished"),
    ok
  end,

  _UserInput = luerl_debugger_emulatorside:user_input_get("Press <<'X' then Enter >> to execute next instruction\n"),
  msg_send_to_emulatorside({execute_next_instruction, no_data}),
  waiting_for_server_message().

file_display(LuaFilePath, LineNum) ->
  case file_read(LuaFilePath) of
    file_is_unknown_cant_open ->
      dont_do_anything;
    LuaSrcAll ->

      RowsDisplayed = 9, % odd numbers only !
      RowsDelta = (RowsDisplayed - 1) div 2, % integer division

      {LineFirst, LineLast} = case (LineNum-RowsDelta) > 0 of
                                true -> {LineNum-RowsDelta, LineNum+RowsDelta};
                                false -> {1, RowsDisplayed}
                              end,

      LuaSrcLines = string:tokens(LuaSrcAll, "\n"),


      % TODO: do line numbering
      LuaSrcLinesNumbered = LuaSrcLines, % line_numbering(1, LuaSrcLines),

      Lines = lists:sublist(LuaSrcLinesNumbered, LineFirst, LineLast),
      io:fwrite(  lists:join("\n", Lines))
  end.




file_read(LuaFilePath) ->
  case file:open(LuaFilePath, [read]) of
    {ok, Device} ->
      try get_all_lines(Device)
        after file:close(Device)
      end;
    _OtherReturnValue ->
      file_is_unknown_cant_open
  end.


get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.
