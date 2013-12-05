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
    {S,St2} = luerl_emul:alloc_table(searchers_table(), St1),
    {L,St3} = luerl_emul:alloc_table(loaded_table(), St2),
    {P,St4} = luerl_emul:alloc_table(preload_table(), St3),
    {T,St5} = luerl_emul:alloc_table(table(S, L, P), St4),
    {T,St5}.

%% table() -> [{FuncName,Function}].
%% meta_table() -> [{TableName,Function}].
%% searchers_table()
%% preloaded_table()
%% loaded_table()

table(S, L, P) ->
    [{<<"config">>,config()},
     {<<"loaded">>,L},
     {<<"preload">>,P},
     {<<"path">>,path()},
     {<<"searchers">>,S},
     {<<"searchpath">>,{function,fun search_path/2}}
    ].

searchers_table() ->
    [{1.0,{function,fun preload_loader/2}},
     {2.0,{function,fun lua_loader/2}}].

preload_table() -> [].

loaded_table() -> [].

%% meta_table() ->
%%     [{<<"__index">>,{function,fun meta_values/2}}
%%     ].

%% config()
%% path()
%% meta_values()
%%  Generate initial data for tables.

config() ->
    <<"/\n",";\n","?\n","!\n","-\n">>.		%The defaults

path() ->
    case os:getenv("LUA_PATH") of
	false -> <<"./?.lua;./?/init.lua">>;	%Default path
	Path -> Path
    end.

%% meta_values([_,<<"bert">>], St) ->
%%     {[<<"/\n",";\n","?\n","!\n","-\n">>],St};
%% meta_values(_, St) -> {[nil],St}.		%Default undefined key

%% searchpath(Name, Path [, Sep [, Rep]]) -> [File] | [nil|Files].

search_path(As, St) ->
    case luerl_lib:conv_list(search_args(As),
			     [lua_string,lua_string,lua_string,lua_string]) of
	[N0,P,S,R] ->				%Name, path, sep, rep
	    N1 = binary:replace(N0, S, R, [global]),
	    Ts = binary:split(P, <<";">>, [global,trim]),
	    {search_path_loop(N1, Ts, []),St};
	_ -> badarg_error(searchpath, As, St)
    end.

search_args([N,P]) -> [N,P,<<".">>,<<"/">>];
search_args([N,P,S]) -> [N,P,S,<<"/">>];
search_args(As) -> As.

search_path_loop(Name, [T|Ts], Tried) ->
    File = binary:replace(T, <<"?">>, Name, [global]),
    %% Test if file can be opened for reading.
    case file:read_file_info(File) of
	{ok,#file_info{access=A}} when A =:= read; A =:= read_write ->
	    [File];
	_ -> search_path_loop(Name, Ts, Tried ++ [$',File,$',$\s])
    end;
search_path_loop(_, [], Tried) ->		%Couldn't find it
    [nil,iolist_to_binary(Tried)].

%% preload_loader()
%% lua_loader()
%%  Predefined load functions in package.searchers. These must be Lua
%%  functions as they are visible.

preload_loader(As, St0) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	[Mod] ->
	    {Pre,St1} = get_tab_keys([<<"package">>,<<"preload">>], St0),
	    case luerl_emul:get_table_key(Pre, Mod, St1) of
		{nil,St2} -> {[],St2};
		{Val,St2} -> {[Val],St2}	%Return the chunk
	    end;
	nil -> badarg_error(preload_loader, As, St0)
    end.

lua_loader(As, St0) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	[Mod] ->
	    {Path,St1} = get_tab_keys([<<"package">>,<<"path">>], St0),
	    Ts = binary:split(Path, <<";">>, [trim,global]),
	    Name = binary:replace(Mod, <<".">>, <<"/">>, [global]),
	    lua_loader_loop(Name, Ts, St1);
	nil -> badarg_error(lua_loader, As, St0)
    end.

lua_loader_loop(Name, [T|Ts], St) ->
    File = binary:replace(T, <<"?">>, Name, [global]),
    case luerl_comp:file(binary_to_list(File)) of
	{ok,C} -> {[C,File],St};		%Return the chunk and file name
	{error,_,_} ->
	    lua_loader_loop(Name, Ts, St)
    end;
lua_loader_loop(_, [], St) -> {[],St}.

-spec require([_], _) -> {_,_} | no_return().	%To keep dialyzer quiet

%% require([File|_], State) ->{Value,State}.
%%  Main require interface.

require(As, St) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	[Mod] -> do_require(Mod, St);
	nil -> badarg_error(require, As, St)
    end.

do_require(Mod, St0) ->
    {Pt,St1} = luerl_emul:get_global_key(<<"package">>, St0),
    case get_tab_keys(Pt, [<<"loaded">>,Mod], St1) of
	{nil,St2} ->
	    {Ss,St3} = luerl_emul:get_table_key(Pt, <<"searchers">>, St2),
	    {Ldr,St4} = search_loaders(Mod, Ss, St3),
	    Chunk = luerl_lib:first_value(Ldr),
	    {Val,St5} = luerl_emul:chunk(Chunk, [], St4),
	    require_ret(Mod, Val, Pt, St5);
	{Val,St2} -> {[Val],St2}
    end.

require_ret(Mod, Val, Pt, St0) ->
    Res = case luerl_lib:first_value(Val) of
	      nil -> true;
	      __tmp -> __tmp
	  end,
    {Lt,St1} = luerl_emul:get_table_key(Pt, <<"loaded">>, St0),
    St2 = luerl_emul:set_table_key(Lt, Mod, Res, St1),
    {[Res],St2}.

search_loaders(Mod, #tref{i=N}, #luerl{ttab=Ts}=St) ->
    #table{a=Arr} = ?GET_TABLE(N, Ts),
    Ls = array:sparse_to_list(Arr),
    search_loaders_loop(Mod, Ls, St).

search_loaders_loop(Mod, [nil|Ls], St) ->	%Could find some of these
    search_loaders_loop(Mod, Ls, St);
search_loaders_loop(Mod, [L|Ls], St0) ->
    {Ret,St1} = luerl_emul:functioncall(L, [Mod], St0),
    case luerl_lib:boolean_value(Ret) of
	true -> {Ret,St1};
	false -> search_loaders_loop(Mod, Ls, St1)
    end;
search_loaders_loop(Mod, [], St) ->
    lua_error({no_module,Mod}, St).

get_tab_keys(Keys, St) -> get_tab_keys(St#luerl.g, Keys, St).

get_tab_keys(First, Keys, St) ->
    Fun = fun (K, {T,S}) -> luerl_emul:get_table_key(T, K, S) end,
    lists:foldl(Fun, {First,St}, Keys).
