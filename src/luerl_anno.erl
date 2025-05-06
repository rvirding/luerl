%% Copyright (c) 2019 Robert Virding
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

%% File    : luerl_anno.erl
%% Author  : Robert Virding
%% Purpose : Handle annotations in the Luerl abstract code.

%% We keep the same standard as in the Erlang AST:
%%
%% - annotations with just the line number can be just the integer.
%% - in an aonnotation list the line number is just an integer while
%%   all other annotations have the format {Key,Value}.

-module(luerl_anno).

-include("luerl.hrl").

?MODULEDOC(false).

-export([new/0,new/1,new/2,set_line/2,line/1,set/3,get/2]).

%% new() -> Anno.
%% new(Line) -> Anno.
%% new(Key, Val) -> Anno.
%%  Create an empty annotation, one containing Line and one containing
%%  a general Key/Val.

new() -> [].

new(Line) -> Line.

new(Key, Val) -> set(Key, Val, new()).

%% set_line(Line, Anno) -> Anno.
%% line(Anno) -> Line | undefined.
%%  Specific functions for accessing line numbers in the anno.

set_line(Line, Anno) when is_integer(Anno) -> Line;
set_line(Line, Anno) -> set_line1(Line, Anno).

set_line1(Line, [Old|Anno]) when is_integer(Old) -> [Line|Anno];
set_line1(Line, [A|Anno]) ->
    [A|set_line1(Line, Anno)];
set_line1(Line, []) -> [Line].

line(Anno) when is_integer(Anno) -> Anno;
line(Anno) -> line1(Anno).

line1([Line|_]) when is_integer(Line) -> Line;
line1([_|Anno]) -> line1(Anno);
line1([]) -> undefined.

%% set(Key, Value, Anno) -> Anno.
%% get(Key, Anno) -> Value | undefined.
%%  Generic accessing functions for the anno.

set(line, Val, Anno) -> set_line(Val, Anno);
set(Key, Val, Anno) when is_integer(Anno) ->
    [Anno,{Key,Val}];
set(Key, Val, Anno) -> set1(Key, Val, Anno).

set1(Key, Val, [{Key,_Old}|Anno]) ->
    [{Key,Val}|Anno];
set1(Key, Val, [A|Anno]) ->
    [A|set1(Key, Val, Anno)];
set1(Key, Val, []) ->
    [{Key,Val}].

get(line, Anno) -> line(Anno);                  %This is untagged
get(_Key, Anno) when is_integer(Anno) ->        %This is untagged so not Key
    undefined;
get(Key, Anno) -> get1(Key, Anno).

get1(Key, [{Key,Val}|_Anno]) -> Val;
get1(Key, [_|Anno]) -> get1(Key, Anno);
get1(_Key, []) -> undefined.
