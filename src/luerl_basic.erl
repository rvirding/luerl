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

-import(luerl_lib, [lua_error/1,badarg_error/2]). %Shorten these

install(St) ->
    luerl_emul:alloc_table(table(), St).

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
    case luerl_lib:is_true_value(As) of
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
    {[],luerl_emul:gc(St)};
    %% {[],St};					%No-op for the moment
collectgarbage(_, St) ->			%Ignore everything else
    {[],St}.

eprint(Args, St) ->
    lists:foreach(fun (#tref{i=N}) ->
			  T = ?GET_TABLE(N, St#luerl.ttab),
			  io:format("~w ", [T]);
		      (A) -> io:format("~w ", [A])
		  end, Args),
    io:nl(),
    {[],St}.

error([M|_], _) -> lua_error(M);		%Never returns!
error(As, _) -> badarg_error(error, As).

%% ipairs(Args, State) -> {[Func,Table,FirstKey],State}.
%%  Return a function which on successive calls returns successive
%%  key-value pairs of integer keys.

ipairs([#tref{}=Tref|_], St) ->
    case luerl_emul:getmetamethod(Tref, <<"__ipairs">>, St) of
	nil -> {[{function,fun ipairs_next/2},Tref,0.0],St};
	Meta -> luerl_emul:functioncall(Meta, [Tref], St)
    end;
ipairs(As, _) -> badarg_error(ipairs, As).
    
ipairs_next([A], St) -> ipairs_next([A,0.0], St);
ipairs_next([#tref{i=T},K|_], St) ->
    #table{a=Arr} = ?GET_TABLE(T, St#luerl.ttab),	%Get the table
    case ?IS_INTEGER(K, I) of
	true when I >= 0 ->
	    Next = I + 1,
	    case raw_get_index(Arr, Next) of
		nil -> {[nil],St};
		V -> {[float(Next),V],St}
	    end;
	_NegFalse -> lua_error({invalid_key,ipairs,K})
    end;
ipairs_next(As, _) -> badarg_error(ipairs, As).

%% pairs(Args, State) -> {[Func,Table,Key],State}.
%%  Return a function to step over all the key-value pairs in a table.

pairs([#tref{}=Tref|_], St) ->
    case luerl_emul:getmetamethod(Tref, <<"__pairs">>, St) of
	nil -> {[{function,fun next/2},Tref,nil],St};
	Meta -> luerl_emul:functioncall(Meta, [Tref], St)
    end;
pairs(As, _) -> badarg_error(pairs, As).

%% next(Args, State) -> {[Key,Value] | [nil], State}.
%%  Given a table and a key return the next key-value pair in the
%%  table, or nil if there is no next key. The key 'nil' gives the
%%  first key-value pair.

next([A], St) -> next([A,nil], St);
next([#tref{i=T},K|_], St) ->
    #table{a=Arr,t=Tab} = ?GET_TABLE(T, St#luerl.ttab),	%Get the table
    if K == nil ->
	    %% Find the first, start with the array.
	    %% io:format("n: ~p\n", [{Arr,Tab}]),
	    next_index(0, Arr, Tab, St);
       is_number(K) ->
	    case ?IS_INTEGER(K, I0) of
		true when I0 >= 1 ->
		    next_index(I0, Arr, Tab, St);
		_NegFalse -> next_key(K, Tab, St)	%Not integer or negative
	    end;
       true -> next_key(K, Tab, St)
    end;
next(As, _) -> badarg_error(next, As).

next_index(I0, Arr, Tab, St) ->
    case next_index_loop(I0+1, Arr, array:size(Arr)) of
	{I1,V} -> {[float(I1),V],St};
	none ->
	    %% Nothing in the array, take table instead.
	    {first_key(Tab),St}
    end.

next_index_loop(I, Arr, S) when I < S ->
    case array:get(I, Arr) of
	nil -> next_index_loop(I+1, Arr, S);
	V -> {I,V}
    end;
next_index_loop(_, _, _) -> none.

first_key(Tab) ->
    case ttdict:first(Tab) of
	{ok,{K,V}} -> [K,V];
	error -> [nil]
    end.

next_key(K, Tab, St) ->
    case ttdict:next(K, Tab) of
	{ok,{N,nil}} -> next_key(N, Tab, St);	%Skip nil values
	{ok,{N,V}} -> {[N,V],St};
	error -> {[nil],St}
    end.

print(Args, St0) ->
    St1 = lists:foldl(fun (A, S0) ->
			      {Str,S1} = tostring([A], S0),
			      io:format("~s ", [Str]),
			      S1
		      end, St0, Args),
    io:nl(),
    {[],St1}.

rawequal([A1,A2|_], St) -> {[A1 =:= A2],St};
rawequal(As, _) -> badarg_error(rawequal, As).

rawget([#tref{i=N},K|_], St) when is_number(K) ->
    #table{a=Arr,t=Tab} = ?GET_TABLE(N, St#luerl.ttab),	%Get the table.
    V = case ?IS_INTEGER(K, I) of
	    true when I >= 1 ->			%Array index
		raw_get_index(Arr, I);
	    _NegFalse ->			%Negative or false
		raw_get_key(Tab, K)
	end,
    {[V],St};
rawget([#tref{i=N},K|_], St) ->
    #table{t=Tab} = ?GET_TABLE(N, St#luerl.ttab),	%Get the table.
    V = raw_get_key(Tab, K),
    {[V],St};
rawget(As, _) -> badarg_error(rawget, As).

rawlen([A|_], St) when is_binary(A) -> {[float(byte_size(A))],St};
rawlen([#tref{i=N}|_], St) ->
    #table{a=Arr} = ?GET_TABLE(N, St#luerl.ttab),
    {[float(array:size(Arr))],St};
rawlen(As, _) -> badarg_error(rawlen, As).

rawset([#tref{i=N}=Tref,K,V|_], #luerl{ttab=Ts0}=St) when is_number(K) ->
    #table{a=Arr0,t=Tab0}=T = ?GET_TABLE(N, Ts0),
    Ts1 = case ?IS_INTEGER(K, I) of
	      true when I >= 1 ->
		  Arr1 = raw_set_index(Arr0, I, V),
		  ?SET_TABLE(N, T#table{a=Arr1}, Ts0);
	      _NegFalse ->			%Negative or false
		  Tab1 = raw_set_key(Tab0, K, V),
		  ?SET_TABLE(N, T#table{t=Tab1}, Ts0)
	  end,
    {[Tref],St#luerl{ttab=Ts1}};
rawset([#tref{i=N}=Tref,K,V|_], #luerl{ttab=Ts0}=St) ->
    #table{t=Tab0}=T = ?GET_TABLE(N, Ts0),
    Tab1 = raw_set_key(Tab0, K, V),
    Ts1 = ?SET_TABLE(N, T#table{t=Tab1}, Ts0),
    {[Tref],St#luerl{ttab=Ts1}};
rawset(As, _) -> badarg_error(rawset, As).

%% raw_get_index(Array, Index) -> nil | Value.
%% raw_get_key(Table, Key) -> nil | Value.

raw_get_index(Arr, I) -> array:get(I, Arr).

raw_get_key(Tab, K) ->
    case ttdict:find(K, Tab) of
	{ok,V} -> V;
	error -> nil
    end.

raw_set_index(Arr, I, V) -> array:set(I, V, Arr).

raw_set_key(Tab, K, nil) -> ttdict:erase(K, Tab);
raw_set_key(Tab, K, V) -> ttdict:store(K, V, Tab).

select([<<$#>>|As], St) -> {[float(length(As))],St};
select([A|As], St) ->
    %%io:fwrite("sel:~p\n", [[A|As]]),
    Len = length(As),
    case luerl_lib:to_int(A) of
	N when is_integer(N), N > 0 -> {select_front(N, As, Len),St};
	N when is_integer(N), N < 0 -> {select_back(-N, As, Len),St};
	_ -> badarg_error(select, [A|As])
    end;
select(As, _) -> badarg_error(select, As).

select_front(N, As, Len) when N =< Len ->
    lists:nthtail(N-1, As);
select_front(_, _, _) -> [].

select_back(N, As, Len) when N =< Len ->
    lists:nthtail(Len-N, As);
select_back(_, As, _) -> As.

tonumber([Arg], St) -> {[luerl_lib:tonumber(Arg)],St};
tonumber([Arg,B|_], St) -> {[luerl_lib:tonumber(Arg, B)],St};
tonumber(As, _) -> badarg_error(tonumber, As).

tostring([Arg|_], St) ->
    case luerl_emul:getmetamethod(Arg, <<"__tostring">>, St) of
	nil -> {[tostring(Arg)],St};
	M when element(1, M) =:= function ->
	    {R,St1} = luerl_emul:functioncall(M, [Arg], St),
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
tostring(#function{l=L}) ->			%Functions defined in Lua
    iolist_to_binary(["function: ",io_lib:write(L)]);
tostring({function,F}) ->			%Internal functions
    iolist_to_binary(["function: ",io_lib:write(F)]);
tostring(#thread{}) -> iolist_to_binary(io_lib:write(thread));
tostring(#userdata{}) -> <<"userdata">>;
tostring(_) -> <<"unknown">>.

type([Arg|_], St) -> {[type(Arg)],St}.		%Only one return value!

type(nil) -> <<"nil">>;
type(N) when is_number(N) -> <<"number">>;
type(S) when is_binary(S) -> <<"string">>;
type(B) when is_boolean(B) -> <<"boolean">>;
type(#tref{}) -> <<"table">>;
type(#function{}) -> <<"function">>;		%Functions defined in Lua
type({function,_}) -> <<"function">>;		%Internal functions
type(#thread{}) -> <<"thread">>;
type(#userdata{}) -> <<"userdata">>;
type(_) -> <<"unknown">>.

%% Meta table functions.

getmetatable([#tref{i=T}|_], #luerl{ttab=Ts}=St) ->
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
    Ts = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=A2} end, St#luerl.ttab),
    {[A1],St#luerl{ttab=Ts}};
setmetatable([#tref{i=N}=A1,nil|_], St) ->
    Ts = ?UPD_TABLE(N, fun (Tab) -> Tab#table{m=nil} end, St#luerl.ttab),
    {[A1],St#luerl{ttab=Ts}};
setmetatable(As, _) -> badarg_error(setmetatable, As).

%% Load string and files.

load(As, St) ->
    case luerl_lib:conv_list(As, [string]) of
	[S] -> do_load(S, St);
	nil -> badarg_error(load, As)
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
	nil -> badarg_error(loadfile, As)
    end.

do_passes([Fun|Funs], St0) ->
    case Fun(St0) of
	{ok,St1} -> do_passes(Funs, St1);
	Error -> Error
    end;
do_passes([], St) -> {ok,St}.

comp_passes() ->
    [fun (S) ->
	     %% Make return values "conformant".
	     case luerl_scan:string(S) of
		 {ok,Ts,_} -> {ok,Ts};
		 {error,Error,_} -> {error,Error}
	     end
     end,
     fun (Ts) -> luerl_parse:chunk(Ts) end,
     fun (Chunk) -> luerl_comp:chunk(Chunk) end].

do_load(Str, St) ->
    case do_passes(comp_passes(), Str) of
	{ok,C} ->
	    Fun = fun (As, S) -> luerl_emul:chunk(C, As, S) end,
	    {[{function,Fun}],St};
	{error,{_,Mod,E}} ->
	    Msg = iolist_to_binary(Mod:format_error(E)),
	    {[nil,Msg],St}
    end.

dofile(As, St) ->
    case luerl_lib:tostrings(As) of
	[File|_] ->
	    {ok,Bin} = file:read_file(File),
	    case do_passes(comp_passes(), binary_to_list(Bin)) of
		{ok,Code} -> luerl_emul:chunk(Code, [], St);
		_ -> badarg_error(dofile, As)
	    end;
	_ -> badarg_error(dofile, As)
    end.

pcall([F|As], St0) ->
    try
	{Rs,St1} = luerl_emul:functioncall(F, As, St0),
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
