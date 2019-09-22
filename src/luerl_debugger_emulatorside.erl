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

%% File    : luerl_debugger_server.erl
%% Authors : Balazs Nyiro
%% Purpose : debugger api from server side for another Luerl node()
%%           to follow the program execution

-module(luerl_debugger_emulatorside).

-export([msg_send/3, msg_send_to_debugger_client/1, waiting_for_debugger_client_command/0]).
-export([nodename_cookie_set_general/3, nodename_cookie_set/0]).
-export([user_input_get/0, user_input_get/1]).
-export([waiting_for_debugger_client_command/0]).
%% IMPORTANT: if you use Luerl as a Library, then you have to know
%% your node's cookie and type it in the debugger's console
%% without correct cookies the start signal of call() won't arrive into the debugger

%% if you execute simple luerl with rebar3 shell,
%% set the same cookie and node names in the luerl interpreter and in the debugger

% the debugger function can receive this signal from the interpreter,
% but I don't have now enough time to implement the code
% step-by-step executing and variable reading now.
% this is a next step
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% send a signal to debugger: interpreter is running


% now for testing we use debug
% in business env don't use debug info, turn it off
is_debug_mode_on() ->
  true.

% STANDARD OF MESSAGES: {SenderModule, SenderFunction, MsgSubject, DataCanBeAnything}
msg_send_to_debugger_client(Msg) ->
    msg_send(Msg, "luerl_debugger_client", luerl_debugger_client).

msg_send(Msg, ToHostName, RegisteredProcessName) ->
  io:fwrite("msg_send() firstline ~p ~p ~p\n", [Msg, ToHostName, RegisteredProcessName]),
  case is_debug_mode_on() of
    true ->

      FullHostname = full_host_name_from_os(), % FIXME: don't define hostname at every run
      Nodename = list_to_atom(ToHostName ++ "@" ++ FullHostname),
      io:fwrite("Process name, NodeName: {~p  ~p}\n", [RegisteredProcessName, Nodename]),
      {RegisteredProcessName, Nodename} ! Msg;

      _ -> ok
  end.

waiting_for_debugger_client_command() ->
  case is_debug_mode_on() of
    true ->
         case get_next_client_message() of

           {timeout, Msg} ->
             {timeout, Msg};

           {client_message, Msg} ->
             io:fwrite("Luerl waiting for client command, received: ~p\n", [Msg]),
             % case Msg of

             %   % Exit from waiting client messages:
             %   {execute_next_instruction, _Data} ->
             %     ok;

             %   % Waiting again in unknown cases:
             %   _ -> waiting_for_debugger_client_command()

             % end
             ok
         end;
    _ ->
      io:fwrite("Luerl: no command because debug mode is off\n", []),
      no_command_because_debug_mode_is_off
  end.

get_next_client_message() ->
  io:fwrite("... get next client message ...\n"),
  receive
    Msg ->
      io:format("Luerl received: ~p~n",[Msg]),
      {client_message, Msg}
  after 6000000 ->
    {timeout, client_didnt_send_anything}
  end.

full_host_name_from_os() ->
  % sometime it doesn't work so I read it from linux hostname.
  % to get full Qualified domain name:  user@host.name.dotseparated
  % {ok, Hostname} = inet:gethostname(),
  % {ok,{hostent,FullHostname,[],inet,_,[_]}} = inet:gethostbyname(Hostname),
  string:trim(os:cmd("hostname")). % newline char removed from end

nodename_cookie_set_general(Msg, RegisteredProcessName, HostShortName) ->

  FullHostname = full_host_name_from_os(),
  io:fwrite("FullHostname: ~p\n", [FullHostname]),

  ErlCookie = user_input_get(Msg ++ "\n(Set the same cookie in debugger_client and debugger_emulatorside.)\nPlease give me the cookie text:"),

  net_kernel:start([list_to_atom(HostShortName++"@"++FullHostname), longnames]),
  erlang:set_cookie(node(), list_to_atom(ErlCookie)),

  % register(luerl_debugger_client, self()).
  register(RegisteredProcessName, self()),

  io:fwrite(">>              NodeName: ~p\n", [node()]),
  io:fwrite(">>                Cookie: ~p\n", [ErlCookie]),
  io:fwrite(">> RegisteredProcessName: ~p\n", [RegisteredProcessName]),
  ok.

user_input_get() ->
  user_input_get("Luerl Debugger").
user_input_get(Msg) ->
  {ok,[UserAnswer]} = io:fread(Msg ++ " > ", "~s"),
  UserAnswer.

% set these settings in server side, if you use luerl console
nodename_cookie_set() ->
  % this is a common function to set nodename and cookie
  nodename_cookie_set_general(
    "== Set Cookie for Luerl emulator ==",
    luerl_emulator,
    "luerl_emulator").
