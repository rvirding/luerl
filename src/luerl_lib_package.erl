%% Copyright (c) 2013 Robert Virding
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

%% File    : luerl_lib_package.erl
%% Author  : Robert Virding
%% Purpose : The package library for Luerl.

%% These functions sometimes behave strangely in the Lua 5.2
%% libraries, but we try to follow them. Most of these functions KNOW
%% that a table is a ttdict! We know that the erlang array has default
%% value 'nil'.

-module(luerl_lib_package).

-include_lib("kernel/include/file.hrl").

-include("luerl.hrl").

%% The basic entry point to set up the function table.
-export([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this

install(St0) ->
    St1 = luerl_emul:set_global_key(<<"require">>,
				    {function,fun require/2}, St0),
    {T,St2} = luerl_emul:alloc_table(table(), St1),
    {M,St3} = luerl_emul:alloc_table(meta_table(), St2),
    Ts = ?UPD_TABLE(T#tref.i, fun (Tab) -> Tab#table{m=M} end, St3#luerl.ttab),
    {T,St3#luerl{ttab=Ts}}.

%% meta_table() -> [{TableName,Function}].
%% table() -> [{FuncName,Function}].

meta_table() ->
    [{<<"__index">>,{function,fun meta_values/2}}
    ].

table() ->
    [{<<"config">>,config()},
     {<<"searchpath">>,{function,fun search_path/2}}
    ].

-spec require([_], _) -> no_return().		%To keep dialyzer quiet

require(As, St) ->				%Undefined as yet.
    badarg_error(require, As, St).

config() ->
    <<"/\n",";\n","?\n","!\n","-\n">>.		%The defaults

meta_values([_,<<"bert">>], St) ->
    {[<<"/\n",";\n","?\n","!\n","-\n">>],St};
meta_values(_, St) -> {[nil],St}.		%Default undefined key

%% searchpath(Name, Path [, Sep [, Rep]]) -> [File] | [nil|Files].

search_path(As, St) ->
    case luerl_lib:conv_list(search_args(As),
			     [lua_string,lua_string,lua_string,lua_string]) of
	[N0,P,S,R] ->				%Name, path, sep, rep
	    N1 = binary:replace(N0, S, R, [global]),
	    Ts = binary:split(P, <<";">>, [global,trim]),
	    {search_loop(N1, Ts, []),St};
	_ -> badarg_error(searchpath, As, St)
    end.

search_args([N,P]) -> [N,P,<<".">>,<<"/">>];
search_args([N,P,S]) -> [N,P,S,<<"/">>];
search_args(As) -> As.

search_loop(Name, [T|Ts], Tried) ->
    File = binary:replace(T, <<"?">>, Name, [global]),
    %% Test if file can be opened for reading.
    case file:read_file_info(File) of
	{ok,#file_info{access=A}} when A =:= read; A =:= read_write ->
	    [File];
	_ -> search_loop(Name, Ts, Tried ++ [$',File,$',$\s])
    end;
search_loop(_, [], Tried) ->			%Couldn't find it
    [nil,iolist_to_binary(Tried)].
