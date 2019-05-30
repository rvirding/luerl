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

-export([new/0,new/1,new/2,set_line/2,line/1,set/3,get/2]).

%% new() -> Anno.
%% new(Line) -> Anno.
%% new(Key, Val) -> Anno.
%%  Create an empty annotation, one containing Line and one containing
%%  a general Key/Val.

new() -> [].

new(Line) -> Line.

new(Key, Val) -> set(Key, Val, new()).

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

set(line, Val, Anno) -> set_line(Val, Anno);
set(Key, Val, Anno) when is_integer(Anno) ->
    lists:keystore(Key, 1, [Anno], {Key,Val});
set(Key, Val, Anno) -> lists:keystore(Key, 1, Anno, {Key,Val}).

get(line, Anno) -> line(Anno);			%This is untagged
get(_Key, Anno) when is_integer(Anno) ->
    undefined;
get(Key, Anno) ->
    case lists:keyfind(Key, 1, Anno) of
	{Key,Val} -> Val;
	false -> undefined
    end.
