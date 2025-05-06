%% Copyright (c) 2013-2020 Robert Virding
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

%% File    : luerl_lib_io.erl
%% Author  : Robert Virding
%% Purpose : The io library for Luerl.

%% This is a quick hack to get io working. It will be improved in time.

-module(luerl_lib_io).

-include("luerl.hrl").

?MODULEDOC(false).

-export([install/1,flush/3,write/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St) ->
    luerl_heap:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"flush">>,#erl_mfa{m=?MODULE,f=flush}},
     {<<"write">>,#erl_mfa{m=?MODULE,f=write}}
    ].

flush(_, _, St) -> {[true],St}.

write(_, As, St) ->
    case luerl_lib:args_to_strings(As) of
	error -> badarg_error(write, As, St);
	Ss ->
	    lists:foreach(fun (S) -> io:format("~s", [S]) end, Ss),
	    {[#userdata{d=standard_io}],St}
    end.
