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

-export([install/1]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

install(St) ->
    luerl_eval:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].
%% Caller will convert this list to the correct format.

table() ->
    [{<<"_VERSION">>,<<"Lua 5.2">>},		%We are optimistic
     {<<"assert">>,{function,fun assert/2}},
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
     {<<"pcall">>,{function,fun pcall/2}},
     {<<"print">>,{function,fun print/2}},
     {<<"rawequal">>,{function,fun rawequal/2}},
     {<<"rawget">>,{function,fun rawget/2}},
     {<<"rawlen">>,{function,fun rawlen/2}},
     {<<"rawset">>,{function,fun rawset/2}},
     {<<"select">>,{function,fun select/2}},
     {<<"setmetatable">>,{function,fun setmetatable/2}},
     {<<"tonumber">>,{function,fun tonumber/2}},
     {<<"tostring">>,{function,fun tostring/2}},
     {<<"type">>,{function,fun type/2}}
    ].

assert(As, St) ->
    case luerl_lib:is_true(As) of
	true -> {As,St};
	false ->
	    M = case As of
		    [_,M0|_] -> M0;
		    _ -> <<"assertion failed">>
		end,
	    lua_error({assert_error,M})
    end.

collectgarbage([], St) -> collectgarbage([<<"collect">>], St);
collectgarbage([<<"collect">>|_], St) ->
    {[],St};					%No-op for the moment
%%    {[],luerl_eval:gc(St)};
collectgarbage(_, St) ->			%Ignore everything else
    {[],St}.

eprint(Args, St) ->
    lists:foreach(fun (#tref{i=N}) ->
			  T = ?GET_TABLE(N, St#luerl.tabs),
			  io:format("~w ", [T]);
		      (A) -> io:format("~w ", [A])
		  end, Args),
    io:nl(),
    {[],St}.

error([M|_], _) -> lua_error(M);		%Never returns!
error(As, _) -> lua_error({badarg,error,As}).

ipairs([#tref{}=Tref|_], St) ->
    case luerl_eval:getmetamethod(Tref, <<"__ipairs">>, St) of
	nil -> {[{function,fun ipairs_next/2},Tref,0.0],St};
	Meta -> luerl_eval:functioncall(Meta, [Tref], St)
    end;
ipairs(As, _) -> lua_error({badarg,ipairs,As}).
    
ipairs_next([A], St) -> ipairs_next([A,0.0], St);
ipairs_next([#tref{i=T},K|_], St) ->
    #table{a=Arr} = ?GET_TABLE(T, St#luerl.tabs),	%Get the table
    case ?IS_INTEGER(K, I) of
	%% true ->
	true when I >= 0 ->
	    Next = I + 1,
	    case orddict:find(Next, Arr) of
		{ok,V} when V =/= nil ->	%Only non-nil values
		    {[float(Next),V],St};
		_ -> {[nil],St}			%No more or nil
	    end;
	_NegFalse -> lua_error({invalid_key,ipairs,K})
    end;
ipairs_next(As, _) -> lua_error({badarg,ipairs,As}).

next([A], St) -> next([A,nil], St);
next([#tref{i=T},K|_], St) ->
    #table{a=Arr,t=Tab} = ?GET_TABLE(T, St#luerl.tabs),	%Get the table
    if K == nil ->
	    %% Find the first, start with the array.
	    %% io:format("n: ~p\n", [{Arr,Tab}]),
	    case first_index(Arr) of
		{I,V} -> {[float(I),V],St};
		none ->
		    %% Nothing in the array, take table
		    case Tab of
			[{F,V}|_] -> {[F,V],St};
			[] -> {[nil],St}
		    end
	    end;
       is_number(K) ->
	    case ?IS_INTEGER(K, I0) of
		%% true ->
		true when I0 >= 1 ->
		    case next_index(I0, Arr) of
			{I1,V} -> {[float(I1),V],St};
			none ->
			    %% None left in array, take table.
			    case Tab of
				[{F,V}|_] -> {[F,V],St};
				[] -> {[nil],St}
			    end
		    end;
		_NegFalse -> next_key(K, Tab, St)	%Not integer or negative
	    end;
       true -> next_key(K, Tab, St)
    end;
next(As, _) -> lua_error({badarg,next,As}).

first_index([E|_]) -> E;
first_index([]) -> none.

next_index(I, Arr) ->
    case next_index_loop(I, Arr) of
	[{I1,V}|_] -> {I1,V};
	_ -> none
    end.

next_index_loop(I, [{I,_}|Arr]) -> Arr;		%The next one
next_index_loop(I, [{K,_}|_]=Arr) when K > I -> Arr;
next_index_loop(I, [{K,_}|Arr]) when K < I ->	%Not there yet
    next_index_loop(I, Arr);
next_index_loop(_, []) -> none.			%Nothing there

next_key(K, Tab, St) ->
    %% io:fwrite("nk: ~p\n", [{K,lists:sublist(Tab,20)}]),
    case next_key_loop(K, Tab) of
	[{Next,V}|_] -> {[Next,V],St};
	[] -> {[nil],St};
	none -> lua_error({invalid_key,next,K})
    end.

next_key_loop(K, [{K,_}|Tab]) -> next_key_loop(Tab);
next_key_loop(K, [_|Tab]) -> next_key_loop(K, Tab);
next_key_loop(_, []) ->  none.

next_key_loop([{_,nil}|Tab]) -> next_key_loop(Tab);    %Skip nil values
next_key_loop(Tab) -> Tab.

pairs([#tref{}=Tref|_], St) ->
    case luerl_eval:getmetamethod(Tref, <<"__pairs">>, St) of
	nil -> {[{function,fun next/2},Tref,nil],St};
	Meta -> luerl_eval:functioncall(Meta, [Tref], St)
    end;
pairs(As, _) -> lua_error({badarg,pairs,As}).

print(Args, St0) ->
    St1 = lists:foldl(fun (A, S0) ->
			      {Str,S1} = tostring([A], S0),
			      io:format("~s ", [Str]),
			      S1
		      end, St0, Args),
    io:nl(),
    {[],St1}.

rawequal([A1,A2|_], St) -> {[A1 =:= A2],St};
rawequal(As, _) -> lua_error({badarg,rawequal,As}).

rawget([#tref{i=N},K|_], St) when is_number(K) ->
    #table{a=Arr,t=Tab} = ?GET_TABLE(N, St#luerl.tabs),	%Get the table.
    case ?IS_INTEGER(K, I) of
	%% true ->				%Array index
	true when I >= 1 ->			%Array index
	    case orddict:find(I, Arr) of
		{ok,V} -> {[V],St};
		error -> {[nil],St}
	    end;
	_NegFalse ->				%Negative or false
	    case orddict:find(K, Tab) of
		{ok,V} -> {[V],St};
		error -> {[nil],St}
	    end
    end;
rawget([#tref{i=N},K|_], St) ->
    #table{t=Tab} = ?GET_TABLE(N, St#luerl.tabs),	%Get the table.
    case orddict:find(K, Tab) of
	{ok,V} -> {[V],St};
	error -> {[nil],St}
    end;
rawget(As, _) -> lua_error({badarg,rawget,As}).

raw_get_index(Arr, I) -> orddict:find(I, Arr).

raw_get_key(I, Tab) -> orddict:find(I, Tab).

raw_set_index(Arr, I, nil) -> orddict:erase(I, Arr);
raw_set_index(Arr, I, V) -> orddict:store(I, V, Arr).

raw_set_key(Arr, I, nil) -> orddict:erase(I, Arr);
raw_set_key(Arr, I, V) -> orddict:store(I, V, Arr).

rawlen([A|_], St) when is_binary(A) -> {[float(byte_size(A))],St};
rawlen([#tref{i=N}|_], St) ->
    #table{a=Arr} = ?GET_TABLE(N, St#luerl.tabs),
    {[length(Arr)],St};
rawlen(As, _) -> lua_error({badarg,rawlen,As}).

rawset([#tref{i=N}=Tref,K,V|_], #luerl{tabs=Ts0}=St) when is_number(K) ->
    #table{a=Arr0,t=Tab0}=T = ?GET_TABLE(N, Ts0),
    Ts1 = case ?IS_INTEGER(K, I) of
	      %% true ->
	      true when I >= 1 ->
		  Arr1 = raw_set_index(Arr0, I, V),
		  ?SET_TABLE(N, T#table{a=Arr1}, Ts0);
	      _NegFalse ->			%Negative or false
		  Tab1 = raw_set_key(Tab0, K, V),
		  ?SET_TABLE(N, T#table{t=Tab1}, Ts0)
	  end,
    {[Tref],St#luerl{tabs=Ts1}};
rawset([#tref{i=N}=Tref,K,V|_], #luerl{tabs=Ts0}=St) ->
    #table{t=Tab0}=T = ?GET_TABLE(N, Ts0),
    Tab1 = raw_set_key(Tab0, K, V),
    Ts1 = ?SET_TABLE(N, T#table{t=Tab1}, Ts0),
    {[Tref],St#luerl{tabs=Ts1}};
rawset(As, _) -> lua_error({badarg,rawset,As}).

select([<<$#>>|As], St) -> {[float(length(As))],St};
select([A|As], St) ->
    %%io:fwrite("sel:~p\n", [[A|As]]),
    Len = length(As),
    case luerl_lib:to_int(A) of
	N when is_integer(N), N > 0 -> {select_front(N, As, Len),St};
	N when is_integer(N), N < 0 -> {select_back(-N, As, Len),St};
	_ -> lua_error({badarg,select,[A|As]})
    end;
select(As, _) -> lua_error({badarg,select,As}).

select_front(N, As, Len) when N =< Len ->
    lists:nthtail(N-1, As);
select_front(_, _, _) -> [].

select_back(N, As, Len) when N =< Len ->
    lists:nthtail(Len-N, As);
select_back(_, As, _) -> As.

tonumber([Arg], St) -> {[luerl_lib:tonumber(Arg)],St};
tonumber([Arg,B|_], St) -> {[luerl_lib:tonumber(Arg, B)],St};
tonumber(As, _) -> lua_error({badarg,tonumber,As}).

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
tostring(#tref{i=I}) -> iolist_to_binary(["table: ",io_lib:write(I)]);
tostring({function,L,_,_,_}) ->
    iolist_to_binary(["function: ",io_lib:write(L)]);
tostring({function,F}) -> iolist_to_binary(["function: ",io_lib:write(F)]);
tostring(#thread{}) -> iolist_to_binary(io_lib:write(thread));
tostring(#userdata{}) -> <<"userdata">>;
tostring(_) -> <<"unknown">>.

type([Arg|_], St) -> {[type(Arg)],St}.		%Only one return value!

type(nil) -> <<"nil">>;
type(N) when is_number(N) -> <<"number">>;
type(S) when is_binary(S) -> <<"string">>;
type(B) when is_boolean(B) -> <<"boolean">>;
type(#tref{}) -> <<"table">>;
type({function,_,_,_,_}) -> <<"function">>;	%Functions defined in Lua
type({function,_}) -> <<"function">>;		%Internal functions
type(#thread{}) -> <<"thread">>;
type(#userdata{}) -> <<"userdata">>;
type(_) -> <<"unknown">>.

%% Meta table functions.

getmetatable([#tref{i=T}|_], #luerl{tabs=Ts}=St) ->
    #table{m=Meta} = ?GET_TABLE(T, Ts),		%Get the table
    {[Meta],St};
getmetatable([#userdata{m=Meta}|_], St) ->
    {[Meta],St};
getmetatable(S, #luerl{meta=Meta}=St) when is_binary(S) ->
    {[Meta#meta.string],St};
getmetatable(N, #luerl{meta=Meta}=St) when is_number(N) ->
    {[Meta#meta.number],St};
getmetatable(_, St) -> {[nil],St}.		%Other types have no metatables

setmetatable([#tref{i=N}=A1,#tref{}=A2|_], St) ->
    Ts = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=A2} end, St#luerl.tabs),
    {[A1],St#luerl{tabs=Ts}};
setmetatable([#tref{i=N}=A1,nil|_], St) ->
    Ts = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=nil} end, St#luerl.tabs),
    {[A1],St#luerl{tabs=Ts}};
setmetatable(As, _) -> lua_error({badarg,setmetatable,As}).

%% Load string and files.

load(As, St) ->
    case luerl_lib:conv_list(As, [string]) of
	[S] -> do_load(S, St);
	nil -> lua_error({badarg,load,As})
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
	nil -> lua_error({badarg,loadfile,As})
    end.

do_load(S, St) ->
    case parse_string(S) of
	{ok,C} ->
	    F = fun (As, St0) ->
			%io:fwrite("l: ~p\n", [{C,As}]),
			Env0 = St0#luerl.env,	%Caller's environment
			%% Evaluate at top-level,
			Env = [lists:last(Env0)],
			{Ret,St1} = luerl_eval:chunk(C, As, St0#luerl{env=Env}),
			St2 = St1#luerl{env=Env0},
			{Ret,St2}
		end,
	    {[{function,F}],St};
	{error,{_,Mod,E}} ->
	    Msg = iolist_to_binary(Mod:format_error(E)),
	    {[nil,Msg],St}
    end.

dofile(As, St) ->
    case luerl_lib:tostrings(As) of
	[File|_] ->
	    {ok,Bin} = file:read_file(File),
	    {ok,C} = parse_string(binary_to_list(Bin)),
	    luerl_eval:chunk(C, St);
	_ -> lua_error({badarg,dofile,As})
    end.

parse_string(S) ->
    case luerl_scan:string(S) of
	{ok,Ts,_} ->
	    case luerl_parse:chunk(Ts) of
		{ok,C} -> {ok,C};
		{error,E} -> {error,E}
	    end;
	{error,E,_} -> {error,E}
    end.

pcall([F|As], St0) ->
    try
	{Rs,St1} = luerl_eval:functioncall(F, As, St0),
	{[true|Rs],St1}
    catch
%% 	Class:Error ->
%% 	    io:fwrite("pc: ~p\n", [{Class,Error}]),
%% 	    {[false,<<>>],St0};
	%% Only catch Lua errors here, signal system errors.
	error:{lua_error,E} ->
	    %% Basic formatting for now.
	    Msg = iolist_to_binary(luerl_lib:format_error(E)),
	    {[false,Msg],St0}
    end.
