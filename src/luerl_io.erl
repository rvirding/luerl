%% Copyright (c) 2025 Robert Virding
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

%% File    : luerl_io.erl
%% Author  : Robert Virding
%% Purpose : Some basic i/o functions for Luerl.

-module(luerl_io).

-include("luerl.hrl").

?MODULEDOC( """
This module provides a standard of set io functions for Luerl. In the
following description, many functions have an optional parameter
`IoDevice`. If included, it must be the pid of a process which handles
the IO protocols such as the IoDevice returned by `file:open/2`. Also
in the functions handling files it can also be the name of the file
which will then be opened.

Note that Luerl, following Lua, does do any implicit UTF-8 encoding of
input strings.

""").

-export([get_line/0,get_line/1,get_line/2,collect_line/2]).
-export([scan_file/1,scan_file/2,parse_file/1,parse_file/2]).

%% get_line() -> Data | {error,Error} | eof.
%%  Reads a line from the standard input (IoDevice), prompting it with
%%  Prompt. Doing it this way saves the input in history. We also make
%%  sure that any utf-8 encoding is done before we return the data.

?DOC( #{equiv => get_line(standard_io, '')}).

get_line() ->
    get_line(standard_io, '').

?DOC( #{equiv => get_line(standard_io, Prompt)}).
 
get_line(Prompt) ->
    get_line(standard_io, Prompt).

?DOC( """
Read a line of text from `IoDevice` withe the prompt `Prompt`. We make
sure that anu UTF-8 encoding is done before we return the data.
""").

get_line(IoDevice, Prompt) ->
    Get = io:request(IoDevice,
                     {get_until,latin1,Prompt,luerl_io,collect_line,[]}),
    %% Make sure unicode codepoints ahve been expanded.
    case Get of
        Line when is_list(Line) ->
            unicode:characters_to_binary(Line);
        Other -> Other
    end.

%% collect_line(OldStack, Data) -> {done,Result,Rest} | {more,NewStack}.

collect_line(Stack, Data) ->
    case io_lib:collect_line(start, Data, latin1, ignored) of
        {stop,Result,Rest} ->
            {done,lists:reverse(Stack, Result),Rest};
        MoreStack ->
            {more,MoreStack ++ Stack}
    end.

%% scan_file(FileName|Fd) -> {ok,[Token]} | {error,Error}.
%% scan_file(FileName|Fd, Line) -> {ok,[Token]} | {error,Error}.
%%  Scan a file returning the tokens found in the file. Handle errors
%%  consistently.

?DOC( #{equiv => scan_file(FileName, 1)}).

scan_file(FileName) -> scan_file(FileName, 1).

?DOC( """
Scan the file `FileName` and return the tokens in it. `FileName` can
also be an `IoDevice` of an already opened file.
""").

scan_file(FileName, Line) ->
    with_token_file(FileName,
                    fun (Ts, _LastLine) -> {ok,Ts} end,
                    Line).

%% parse_file(FileName|Fd) -> {ok,[{Sexpr,Line}]} | {error,Error}.
%% parse_file(FileName|Fd, Line) -> {ok,[{Sexpr,Line}]} | {error,Error}.
%%  Parse a file returning the chunk it contained. Handle errors
%%  consistently.

?DOC( #{equiv => parse_file(FileName, 1)}).

parse_file(FileName) -> parse_file(FileName, 1).

?DOC( """
Parse the file `FileName` and return the chunk in it. `FileName` can
also be an `IoDevice` of an already opened file.
""").

parse_file(FileName, Line) ->
    with_token_file(FileName,
                    fun (Ts, LastLine) -> parse_tokens(Ts, LastLine) end,
                    Line).

parse_tokens(Tokens, _LastLine) ->
    luerl_parse:chunk(Tokens).

%% with_token_file(FileName|Fd, DoFunc, Line)
%%  Open the file, scan all Luerl tokens and apply DoFunc on them. Note
%%  that a new file starts at line 1.

with_token_file(Fd, Do, Line) when is_pid(Fd) ->
    with_token_file_fd(Fd, Do, Line);
with_token_file(Name, Do, _Line) ->
    case file:open(Name, [read,{encoding,latin1}]) of
        {ok,Fd} ->
            %% Check if first line a script or Windows BOM, if so skip it.
            case io:get_line(Fd, '') of
                "#" ++ _ -> ok;                 %Skip line
                [239,187,191|_] ->
                    file:position(Fd, 3);       %Skip BOM
                _ -> file:position(Fd, bof)     %Get it all
            end,
            with_token_file_fd(Fd, Do, 1);      %Start at first valid line
        {error,Error} -> {error,{none,file,Error}}
    end.

with_token_file_fd(Fd, Do, Line) ->             %Called with a file descriptor
    Ret = case io:request(Fd, {get_until,latin1,'',luerl_scan,tokens,[Line]}) of
              {ok,Ts,LastLine} -> Do(Ts, LastLine);
              {eof,_}=Eof -> Eof;               %This might occur.
              {error,Error,_} -> {error,Error}
          end,
    file:close(Fd),                             %Close the file
    Ret.                                        % and return value
