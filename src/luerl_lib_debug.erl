%% Copyright (c) 2015-2020 Robert Virding
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

%% File    : luerl_lib_debug.erl
%% Author  : Robert Virding
%% Purpose : The debug library for Luerl.

%% This is a very rudimentary debug module which contains those
%% functions which need no detailed information about the internals.

-module(luerl_lib_debug).

-include("luerl.hrl").

?MODULEDOC(false).

%% The basic entry point to set up the function table.
-export([install/1,getmetatable/3,getuservalue/3,setmetatable/3,setuservalue/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).       %Shorten this

install(St) ->
    luerl_heap:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"getmetatable">>,#erl_mfa{m=?MODULE,f=getmetatable}},
     {<<"getuservalue">>,#erl_mfa{m=?MODULE,f=getuservalue}},
     {<<"setmetatable">>,#erl_mfa{m=?MODULE,f=setmetatable}},
     {<<"setuservalue">>,#erl_mfa{m=?MODULE,f=setuservalue}}
    ].

%% getmetatable([Value|_], State) -> {[Table],State}.
%% setmetatable([Table,Table|nil|_], State) -> {[Table],State}.
%%  Can set the metatable of all types here. Return tables for all
%%  values, for tables and userdata it is the table of the object,
%%  else the metatable for the type.

getmetatable(_, [O|_], St) ->
    {[luerl_heap:get_metatable(O, St)],St};
getmetatable(_, As, St) -> badarg_error(getmetatable, As, St).

setmetatable(_, [T,M|_], St0) ->
    St1 = luerl_heap:set_metatable(T, M, St0),
    {[T],St1};
setmetatable(_, As, St) -> badarg_error(setmetatable, As, St).

%% getuservalue([User|_], State) -> {[Value],State}.
%% setuservalue([User,Value|_], State) -> {[User],State}.
%%  These are basically no-ops.

getuservalue(_, [_|_], St) -> {[nil],St};
getuservalue(_, As, St) -> badarg_error(getuservalue, As, St).

setuservalue(_, [U,_|_], St) -> {[U],St};
setuservalue(_, As, St) -> badarg_error(setuservalue, As, St).
