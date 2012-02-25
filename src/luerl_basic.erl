%% Copyright (c) 2012 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : luerl_basic.erl
%% Author  : Robert Virding
%% Purpose : The basic library for Luerl.

-module(luerl_basic).

-include("luerl.hrl").

-export([table/0]).

%% table() -> [{FuncName,Function}].
%% Caller will convert this list to the correct format.

table() ->
    [{<<"assert">>,{function,fun assert/2}},
     {<<"collectgarbage">>,{function,fun collectgarbage/2}},
     {<<"dofile">>,{function,fun dofile/2}},
     {<<"eprint">>,{function,fun eprint/2}},
     {<<"error">>,{function,fun error/2}},
     {<<"getmetatable">>,{function,fun getmetatable/2}},
     {<<"ipairs">>,{function,fun ipairs/2}},
     {<<"load">>,{function,fun load/2}},
     {<<"loadfile">>,{function,fun loadfile/2}},
     {<<"next">>,{function,fun next/2}},
     {<<"pairs">>,{function,fun pairs/2}},
     {<<"print">>,{function,fun print/2}},
     {<<"rawequal">>,{function,fun rawequal/2}},
     {<<"rawget">>,{function,fun rawget/2}},
     {<<"rawlen">>,{function,fun rawlen/2}},
     {<<"rawset">>,{function,fun rawset/2}},
     {<<"select">>,{function,fun select/2}},
     {<<"setmetatable">>,{function,fun setmetatable/2}},
     {<<"tonumber">>,{function,fun tonumber/2}},
     {<<"tostring">>,{function,fun tostring/2}},
     {<<"type">>,{function,fun type/2}},
     {<<"_VERSION">>,<<"Lua 5.2">>}		%We are optimistic
    ].

assert(As, St) ->
    case luerl_lib:is_true(As) of
	true -> {As,St};
	false ->
	    M = case As of
		[_,M0|_] -> M0;
		_ -> <<"assertion failed">>
		end,
	    error({assert_error,M})
    end.

collectgarbage([], St) -> collectgarbage([<<"collect">>], St);
collectgarbage([<<"collect">>|_], St) ->
    {[],luerl_eval:gc(St)};
collectgarbage(_, St) ->			%Ignore everything else
    {[],St}.

eprint(Args, St) ->
    lists:foreach(fun (A) -> io:format("~w ", [A]) end, Args),
    io:nl(),
    {[],St}.

error([M|_], _) -> error({lua_error,M});	%Never returns!
error(As, _) -> error({badarg,error,As}).

ipairs([{table,_}=T|_], St) ->
    {[{function,fun ipairs_next/2},T,0],St};
ipairs(As, _) -> error({badarg,ipairs,As}).
    
ipairs_next([A], St) -> ipairs_next([A,0], St);
ipairs_next([{table,T},I|_], St) ->
    {Tab,_} = ?GET_TABLE(T, St#luerl.tabs),	%Get the table
    Next = I + 1.0,				%Ensure float!
    case orddict:find(Next, Tab) of
	{ok,V} -> {[Next,V],St};
	error -> {[nil],St}
    end.

next([A], St) -> next([A,nil], St);
next([{table,T},K|_], St) ->
    {Tab,_} = ?GET_TABLE(T, St#luerl.tabs),	%Get the table
    if K == nil ->
	    case Tab of
		[{F,V}|_] -> {[F,V],St};
		[] -> {[nil],St}
	    end;
       true ->
	    case next_loop(K, Tab) of
		[{Next,V}|_] -> {[Next,V],St};
		[] -> {[nil],St};
		error -> error({invalid_key,K})
	    end
    end;
next(As, _) -> error({badarg,next,As}).

next_loop(K, [{K,_}|Tab]) -> Tab;
next_loop(K, [_|Tab]) -> next_loop(K, Tab);
next_loop(_, []) ->  error.

pairs([{table,_}=T|_], St) ->
    {[{function,fun next/2},T,nil],St};
pairs(As, _) -> error({badarg,pairs,As}).

print(Args, St0) ->
    St1 = lists:foldl(fun (A, S0) ->
			      {Str,S1} = tostring([A], S0),
			      io:format("~s ", [Str]),
			      S1
		      end, St0, Args),
    io:nl(),
    {[],St1}.

rawequal([A1,A2|_], St) -> {[A1 =:= A2],St};
rawequal(As, _) -> error({badarg,rawequal,As}).

rawget([{table,N},K|_], St) ->
    {T,_} = ?GET_TABLE(N, St#luerl.tabs),	%Get the table.
    case orddict:find(K, T) of
	{ok,Val} -> Val;
	error -> nil				%Default value
    end;
rawget(As, _) -> error({badarg,rawget,As}).

rawlen([A|_], St) when is_binary(A) -> {[byte_size(A)],St};
rawlen([{table,N}|_], St) ->
    Tab = ?GET_TABLE(N, St#luerl.tabs),
    {length(element(1, Tab)),St};
rawlen(As, _) -> error({badarg,rawlen,As}).

rawset([{table,N},Key,Val|_], #luerl{tabs=Ts0}=St) ->
    Upd = if Val =:= nil -> fun ({T,M}) -> {orddict:erase(Key, T),M} end;
	     true -> fun ({T,M}) -> {orddict:store(Key, Val, T),M} end
	  end,
    Ts1 = ?UPD_TABLE(N, Upd, Ts0),
    St#luerl{tabs=Ts1};
rawset(As, _) -> error({badarg,rawset,As}).

select([<<$#>>|As], St) -> {[length(As)],St};
select([A|As], St) ->
    case luerl_lib:tonumber(A) of
	N when is_number(N), N > 0 -> {select_front(round(N), As),St};
	N when is_number(N), N < 0 -> {select_back(-round(N), As),St};
	_ -> error({badarg,select,[A|As]})
    end;
select(As, _) -> error({badarg,select,As}).

select_front(N, As) when N < length(As) ->
    lists:nthtail(N-1, As);
select_front(_, _) -> [].

select_back(N, As) ->
    L = length(As),
    lists:sublist(As, L-N+1, N).

tonumber([Arg], St) -> {[luerl_lib:tonumber(Arg)],St};
tonumber([Arg,B|_], St) -> {[luerl_lib:tonumber(Arg, B)],St}.

tostring([Arg|_], St) ->
    case luerl_eval:getmetamethod(Arg, <<"__tostring">>, St) of
	nil -> {[tostring(Arg)],St};
	M when element(1, M) =:= function ->
	    {R,St1} = luerl_eval:functioncall(M, [Arg], St),
	    {R,St1}
    end.

tostring(nil) -> <<"nil">>;
tostring(true) -> <<"true">>;
tostring(false) -> <<"false">>;
tostring(N) when is_number(N) ->
    A = abs(N),
    %% Print really big/small "integers" as floats as well.
    S = if A < 1.0e-4 ; A > 1.0e14 -> io_lib:write(N);
	   ?IS_INTEGER(N) -> integer_to_list(round(N));
	   true -> io_lib:write(N)
	end,
    iolist_to_binary(S);
tostring(S) when is_binary(S) -> S;
tostring({table,_}=T) -> iolist_to_binary(io_lib:write(T));
tostring({function,_,_,_,_}) -> iolist_to_binary(io_lib:write(function));
tostring({function,_}) -> iolist_to_binary(io_lib:write(function));
tostring({thread,_}) -> iolist_to_binary(io_lib:write(thread));
tostring({userdata,_}) -> <<"userdata">>;
tostring(_) -> <<"unknown">>.

type([Arg|_], St) -> {[type(Arg)],St}.		%Only one return value!

type(nil) -> <<"nil">>;
type(N) when is_number(N) -> <<"number">>;
type(S) when is_binary(S) -> <<"string">>;
type(B) when is_boolean(B) -> <<"boolean">>;
type({table,_}) -> <<"table">>;
type({function,_,_,_,_}) -> <<"function">>;	%Functions defined in Lua
type({function,_}) -> <<"function">>;		%Internal functions
type({thread,_}) -> <<"thread">>;
type({userdata,_}) -> <<"userdata">>;
type(_) -> <<"unknown">>.

%% Meta table functions.

getmetatable([{table,T}|_], St) ->		%Only tables have metatables
    {_,M} = ?GET_TABLE(T, St#luerl.tabs),	%Get the table
    {[M],St};
getmetatable(_, St) -> {[nil],St}.

setmetatable([{table,N}=A1,{table,_}=A2|_], St) ->
    Ts = ?UPD_TABLE(N, fun ({T,_}) -> {T,A2} end, St#luerl.tabs),
    {[A1],St#luerl{tabs=Ts}};
setmetatable([{table,N}=A1,nil|_], St) ->
    Ts = ?UPD_TABLE(N, fun ({T,_}) -> {T,nil} end, St#luerl.tabs),
    {[A1],St#luerl{tabs=Ts}};
setmetatable(As, _) -> error({badarg,setmetatable,As}).

%% Load string and files.

load(As, St) ->
    case luerl_lib:conv_list(As, [string]) of
	[S] -> do_load(S, St);
	nil -> error({badarg,load,As})
    end.

loadfile(As, St) ->
    case luerl_lib:conv_list(As, [string]) of
	[F] ->
	    case file:read_file(F) of
		{ok,B} -> do_load(binary_to_list(B), St);
		{error,E} ->
		    Msg = iolist_to_binary(file:format_error(E)),
		    {[nil,Msg],St}
	    end;
	nil -> error({badarg,loadfile,As})
    end.

do_load(S, St) ->
    case parse_string(S) of
	{ok,C} ->
	    F = fun (_, St0) ->
			Env0 = St0#luerl.env,	%Caller's environment
			%% Evaluate at top-level,
			Env = [lists:last(Env0)],
			{Ret,St1} = luerl_eval:chunk(C, St0#luerl{env=Env}),
			St2 = St1#luerl{env=Env0},
			{Ret,St2}
		end,
	    {[{function,F}],St};
	{error,{_,Mod,E}} ->
	    Msg = iolist_to_binary(Mod:format_error(E)),
	    {[nil,Msg],St}
    end.

dofile([A1|_], St0) when is_number(A1) ; is_binary(A1) ->
    File = luerl_lib:tostring(A1),
    {ok,Bin} = file:read_file(File),
    {ok,C} = parse_string(binary_to_list(Bin)),
    luerl_eval:chunk(C, St0);
dofile(As, _) -> error({badarg,dofile,As}).

parse_string(S) ->
    case luerl_scan:string(S) of
	{ok,Ts,_} ->
	    case luerl_parse:chunk(Ts) of
		{ok,C} -> {ok,C};
		{error,E} -> {error,E}
	    end;
	{error,E} -> {error,E}
    end.
