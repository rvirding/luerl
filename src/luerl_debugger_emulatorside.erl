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

-export([msg_send/2, msg_send_to_debugger_client/1, waiting_for_debugger_client_command/1]).

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
    msg_send(Msg, "debugger@").

msg_send(Msg, To) ->
  case is_debug_mode_on() of
    _ -> ok;

    true ->

      {ok, Hostname} = inet:gethostname(),
      {ok,{hostent,FullHostname,[],inet,_,[_]}} = inet:gethostbyname(Hostname),
      NodenameDebugger = list_to_atom(To ++ FullHostname),
      {luerl_debugger, NodenameDebugger} ! Msg

  end.

waiting_for_debugger_client_command(State) ->
  case is_debug_mode_on() of
    _ -> ok;
    true ->
         case get_next_client_message() of
           {timeout, _} -> {client_message, timeout, nodata};
           {client_message, Msg, Data} ->
             case Msg of

               % Exit from waiting client messages:
               execute_next_instruction -> {client_message, Msg, Data};

               % Waiting again in unknown cases:
               _ -> waiting_for_debugger_client_command(State)

             end
         end
  end.

get_next_client_message() ->
  receive
    Msg ->
      get_next_client_message(),
      {client_message, Msg}
  after 6000000 ->
    {timeout, client_didnt_send_anything}
  end.
