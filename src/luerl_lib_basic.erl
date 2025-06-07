%% Copyright (c) 2013-2024 Robert Virding
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

%% File    : luerl_lib_basic.erl
%% Author  : Robert Virding
%% Purpose : The basic library for Luerl.

-module(luerl_lib_basic).

-include("luerl.hrl").

?MODULEDOC(false).

%% The basic entry point to set up the function table.
-export([install/1,assert/3,error_call/3,collectgarbage/3,dofile/3,
         eprint/3,getmetatable/3,ipairs/3,ipairs_next/3,
         load/3,loadfile/3,loadstring/3,
         next/3,pairs/3,pcall/3,print/3,
         rawequal/3,rawget/3,rawlen/3,rawset/3,
         select/3,setmetatable/3,tonumber/3,tostring/3,type/3,unpack/3]).

%% Export some functions which can be called from elsewhere.
-export([print/2,type/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]). %Shorten these

install(St) ->
    luerl_heap:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].
%% Caller will convert this list to the correct format.

table() ->
    [{<<"_VERSION">>,<<"Lua 5.3">>},            %We are optimistic
     {<<"assert">>,#erl_mfa{m=?MODULE,f=assert}},
     {<<"collectgarbage">>,#erl_mfa{m=?MODULE,f=collectgarbage}},
     {<<"dofile">>,#erl_mfa{m=?MODULE,f=dofile}},
     {<<"eprint">>,#erl_mfa{m=?MODULE,f=eprint}},
     {<<"error">>,#erl_mfa{m=?MODULE,f=error_call}},
     {<<"getmetatable">>,#erl_mfa{m=?MODULE,f=getmetatable}},
     {<<"ipairs">>,#erl_mfa{m=?MODULE,f=ipairs}},
     {<<"load">>,#erl_mfa{m=?MODULE,f=load}},
     {<<"loadfile">>,#erl_mfa{m=?MODULE,f=loadfile}},
     {<<"loadstring">>,#erl_mfa{m=?MODULE,f=loadstring}}, %For Lua 5.1 compatibility
     {<<"next">>,#erl_mfa{m=?MODULE,f=next}},
     {<<"pairs">>,#erl_mfa{m=?MODULE,f=pairs}},
     {<<"pcall">>,#erl_mfa{m=?MODULE,f=pcall}},
     {<<"print">>,#erl_mfa{m=?MODULE,f=print}},
     {<<"rawequal">>,#erl_mfa{m=?MODULE,f=rawequal}},
     {<<"rawget">>,#erl_mfa{m=?MODULE,f=rawget}},
     {<<"rawlen">>,#erl_mfa{m=?MODULE,f=rawlen}},
     {<<"rawset">>,#erl_mfa{m=?MODULE,f=rawset}},
     {<<"select">>,#erl_mfa{m=?MODULE,f=select}},
     {<<"setmetatable">>,#erl_mfa{m=?MODULE,f=setmetatable}},
     {<<"tonumber">>,#erl_mfa{m=?MODULE,f=tonumber}},
     {<<"tostring">>,#erl_mfa{m=?MODULE,f=tostring}},
     {<<"type">>,#erl_mfa{m=?MODULE,f=type}},
     {<<"unpack">>,#erl_mfa{m=?MODULE,f=unpack}}    %For Lua 5.1 compatibility
    ].

assert(_, As, St) ->
    case luerl_lib:boolean_value(As) of
	true -> {As,St};
	false ->
	    M = case As of
		    [_,M0|_] -> M0;
		    _ -> <<"assertion failed">>
		end,
	    lua_error({assert_error,M}, St)
    end.

collectgarbage(_, [], St) -> collectgarbage(nil, [<<"collect">>], St);
collectgarbage(_, [<<"collect">>|_], St) ->
    {[],luerl_heap:gc(St)};
    %% {[],St};					%No-op for the moment
collectgarbage(_, _, St) ->			%Ignore everything else
    {[],St}.

eprint(_, Args, St) ->
    lists:foreach(fun (#tref{}=Tref) ->
			  Tab = luerl_heap:get_table(Tref, St),
			  io:format("~w ", [Tab]);
		      (A) -> io:format("~w ", [A])
		  end, Args),
    io:nl(),
    {[],St}.

-spec error_call(_, _, _) -> no_return().

%% error_call(Args, State) -> no_return().
%%  Generate an error with an error string.

error_call(_, [{tref, _}=T|_]=As, St0) ->
    case luerl_heap:get_metamethod(T, <<"__tostring">>, St0) of
        nil -> lua_error({error_call, As}, St0);
        Meta ->
            {Rets, St1} = luerl_emul:functioncall(Meta, [T], St0),
            lua_error({error_call, Rets}, St1)
    end;
error_call(_, As, St) ->                       %Never returns!
    lua_error({error_call, As}, St).

%% ipairs(Args, State) -> {[Func,Table,FirstKey],State}.
%%  Return a function which on successive calls returns successive
%%  key-value pairs of integer keys. We check that it is a table first
%%  when we access it.

ipairs(_, [Tref|_], St) ->
    case luerl_heap:get_metamethod(Tref, <<"__ipairs">>, St) of
	nil -> {[#erl_mfa{m=?MODULE,f=ipairs_next},Tref,0],St};
	Meta -> luerl_emul:functioncall(Meta, [Tref], St)
    end;
ipairs(_, As, St) -> badarg_error(ipairs, As, St).

ipairs_next(_, [A], St) -> ipairs_next(nil, [A,0], St);
ipairs_next(_, [Tref,K|_], St) when ?IS_TREF(Tref), is_integer(K) ->
    %% Get the table.
    #table{a=Arr} = luerl_heap:get_table(Tref, St),
    Next = K + 1,
    case array:get(Next, Arr) of
	nil -> {[nil],St};
	V -> {[Next,V],St}
    end;
ipairs_next(_, As, St) -> badarg_error(ipairs, As, St).

%% pairs(Args, State) -> {[Func,Table,Key],State}.
%%  Return a function to step over all the key-value pairs in a
%%  table. We check that it is a table first when we access it.

pairs(_, [Tref|_], St) ->
    case luerl_heap:get_metamethod(Tref, <<"__pairs">>, St) of
	nil -> {[#erl_mfa{m=?MODULE,f=next},Tref,nil],St};
	Meta -> luerl_emul:functioncall(Meta, [Tref], St)
    end;
pairs(_, As, St) -> badarg_error(pairs, As, St).

%% next(Args, State) -> {[Key,Value] | [nil], State}.
%%  Given a table and a key return the next key-value pair in the
%%  table, or nil if there is no next key. The key 'nil' gives the
%%  first key-value pair.

next(_, [A], St) -> next(nil, [A,nil], St);
next(_, [#tref{}=Tref,K|_], St) ->
    %% Get the table.
    #table{a=Arr,d=Dict} = luerl_heap:get_table(Tref, St),
    if K == nil ->
	    %% Find the first, start with the array.
	    next_index(0, Arr, Dict, St);
       is_integer(K), K >= 1 ->
	    next_index(K, Arr, Dict, St);
       is_float(K) ->
	    case ?IS_FLOAT_INT(K, I) of
		true when I >= 1 ->
		    next_index(I, Arr, Dict, St);
		_NegFalse ->			%Not integer or negative
		    next_key(K, Dict, St)
	    end;
       true -> next_key(K, Dict, St)
    end;
next(_, As, St) -> badarg_error(next, As, St).

next_index(I0, Arr, Dict, St) ->
    case next_index_loop(I0+1, Arr, array:size(Arr)) of
	{I1,V} -> {[I1,V],St};
	none ->
	    %% Nothing in the array, take table instead.
	    first_key(Dict, St)
    end.

next_index_loop(I, Arr, S) when I < S ->
    case array:get(I, Arr) of
	nil -> next_index_loop(I+1, Arr, S);
	V -> {I,V}
    end;
next_index_loop(_, _, _) -> none.

first_key(Dict, St) ->
    case ttdict:first(Dict) of
	{ok,{K,V}} -> {[K,V],St};
	error -> {[nil],St}
    end.

next_key(K, Dict, St) ->
    case ttdict:next(K, Dict) of
	{ok,{N,V}} -> {[N,V],St};
	error -> {[nil],St}
    end.

%% print(Args, State) -> {[],State}.
%%  Receives any number of arguments and prints their values to
%%  stdout, using the tostring function to convert each argument to a
%%  string. print is not intended for formatted output, but only as a
%%  quick way to show a value, for instance for debugging.

print(_, Args, St0) ->
    St1 = lists:foldl(fun (A, S0) ->
                              {Str,S1} = luerl_lib:tostring(A, S0),
                              print_arg(Str),
                              S1
                      end, St0, Args),
    io:nl(),
    {[],St1}.

print_arg(Str) ->
    Fun = fun (C, Acc) when C >= 128 ->
                  [$?|Acc];                     %Just mark with a ?
              %% Some special case control characters.
              (C, Acc) when
                    C =:= $\t ; C =:= $\n ; C =:= $\v ; C =:= $\f ; C =:= $\r ->
                  [C|Acc];
              (C, Acc) when C =< 31 -> Acc;     %Skip other control characters
              (C, Acc) -> [C|Acc]               %Output the rest
          end,
    Chars = lists:foldr(Fun, [], binary_to_list(Str)),
    io:format("~s ", [Chars]).

print(Args, St0) -> print(nil, Args, St0).

%% rawequal([Arg,Arg|_], State) -> {[Bool],State}.
%% rawlen([Object|_], State) -> {[Length],State}.
%% rawget([Table,Key|_], State) -> {[Val],State)}.
%% rawset([Table,Key,Value|_]], State) -> {[Table],State)}.

rawequal(_, [A1,A2|_], St) -> {[A1 =:= A2],St};
rawequal(_, As, St) -> badarg_error(rawequal, As, St).

rawlen(_, [A|_], St) when is_binary(A) -> {[float(byte_size(A))],St};
rawlen(_, [#tref{}=T|_], St) ->
    {[luerl_lib_table:raw_length(T, St)],St};
rawlen(_, As, St) -> badarg_error(rawlen, As, St).

rawget(_, [#tref{}=Tref,Key|_], St) ->
    Val = luerl_heap:raw_get_table_key(Tref, Key, St),
    {[Val],St};
rawget(_, As, St) -> badarg_error(rawget, As, St).

rawset(_, [Tref,nil=Key,_|_], St) ->
    lua_error({illegal_index,Tref,Key}, St);
rawset(_, [#tref{}=Tref,Key,Val|_], St0) ->
    St1 = luerl_heap:raw_set_table_key(Tref, Key, Val, St0),
    {[Tref],St1};
rawset(_, As, St) -> badarg_error(rawset, As, St).

%% select(Args, State) -> {[Element],State}.

select(_, [<<$#>>|As], St) -> {[float(length(As))],St};
select(_, [A|As], St) ->
    Len = length(As),
    case luerl_lib:arg_to_integer(A) of
	N when is_integer(N), N > 0 -> {select_front(N, As, Len),St};
	N when is_integer(N), N < 0 -> {select_back(-N, As, Len),St};
	_ -> badarg_error(select, [A|As], St)
    end;
select(_, As, St) -> badarg_error(select, As, St).

select_front(N, As, Len) when N =< Len ->
    lists:nthtail(N-1, As);
select_front(_, _, _) -> [].

select_back(N, As, Len) when N =< Len ->
    lists:nthtail(Len-N, As);
select_back(_, As, _) -> As.

tonumber(_, [Arg], St) -> {[tonumber(luerl_lib:arg_to_number(Arg))],St};
tonumber(_, [Arg,B|_], St) -> {[tonumber(luerl_lib:arg_to_number(Arg, B))],St};
tonumber(_, As, St) -> badarg_error(tonumber, As, St).

tonumber(Num) when is_number(Num) -> Num;
tonumber(_) -> nil.

%% tostring([Arg|_], Stated) -> {String,State}.
%%  Return the type as a string.

tostring(_, [Arg|_], St0) ->
    {Str,St1} = luerl_lib:tostring(Arg, St0),
    {[Str],St1};
tostring(_, As, St) -> badarg_error(tostring, As, St).

%% type([Data|_], State) -> {Type,State}.
%%  Return the type of the argument.

type(_, [Arg|_], St) -> {[type(Arg)],St};          %Only one return value!
type(_, As, St) -> badarg_error(type, As, St).

type(nil) -> <<"nil">>;
type(N) when is_number(N) -> <<"number">>;
type(S) when is_binary(S) -> <<"string">>;
type(B) when is_boolean(B) -> <<"boolean">>;
type(#tref{}) -> <<"table">>;
type(#usdref{}) -> <<"userdata">>;
type(#funref{}) -> <<"function">>;              %Functions defined in Lua
type(#erl_func{}) -> <<"function">>;            %Internal functions
type(#erl_mfa{}) -> <<"function">>;
type(#thread{}) -> <<"thread">>;
type(_) -> <<"unknown">>.

%% getmetatable([Value|_], State) -> {Table,State}.
%% setmetatable([Table,Table|nil|_], State) -> {Table,State}.
%%  Can only set the metatable of tables here. Return tables for all
%%  values, for tables and userdata it is the table of the object,
%%  else the metatable for the type.

getmetatable(_, [Obj|_], St) ->
    case luerl_heap:get_metatable(Obj, St) of
	#tref{}=Meta ->
	    #table{d=Dict} = luerl_heap:get_table(Meta, St),
	    case ttdict:find(<<"__metatable">>, Dict) of
		{ok,MM} -> {[MM],St};
		error -> {[Meta],St}
	    end;
	nil -> {[nil],St}
    end;
getmetatable(_, As, St) -> badarg_error(getmetatable, As, St).


setmetatable(_, [#tref{}=T,#tref{}=M|_], St) ->
    do_setmetatable(T, M, St);
setmetatable(_, [#tref{}=T,nil|_], St) ->
    do_setmetatable(T, nil, St);
setmetatable(_, As, St) -> badarg_error(setmetatable, As, St).

do_setmetatable(#tref{}=Tref, Meta, St0) ->
    case luerl_heap:get_metamethod(Tref, <<"__metatable">>, St0) of
	nil ->
	    Upd = fun (Tab) -> Tab#table{meta=Meta} end,
	    St1 = luerl_heap:upd_table(Tref, Upd, St0),
	    {[Tref],St1};
	_ -> badarg_error(setmetatable, [Tref], St0)
    end.

%% Do files.

dofile(_, As, St) ->
    case luerl_lib:conv_list(As, [erl_string]) of
       [File] ->
           %% Compile the file so it returns errors.
           Ret = luerl_comp:file(File, [verbose,return]),
           dofile_ret(Ret, As, St);
       _ -> badarg_error(dofile, As, St)
    end.

dofile_ret({ok,Chunk}, _, St0) ->
    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
    luerl_emul:call(Func, [], St1);
dofile_ret({error,[{_,Mod,E}|_],_}, _As, St) ->
    Msg = unicode:characters_to_binary(Mod:format_error(E)),
    lua_error({error_message,Msg}, St).

%% Load string and files.

load(_, As, St) ->
    case luerl_lib:conv_list(As, [erl_string,lua_string,lua_string,lua_any]) of
        [S|_] ->
            %% Compile the string so it returns errors.
            Ret = luerl_comp:string(S, [verbose,return]),
            load_ret(Ret, St);
        error -> badarg_error(load, As, St)
    end.

loadfile(_, As, St) ->
    case luerl_lib:conv_list(As, [erl_string,lua_string,lua_any]) of
        [F|_] ->
            %% Compile the file so it returns errors.
            Ret = luerl_comp:file(F, [verbose,return]),
            load_ret(Ret, St);
        error -> badarg_error(loadfile, As, St)
    end.

loadstring(_, As, St) ->
    case luerl_lib:conv_list(As, [erl_string]) of
        [S] ->
            %% Compile the string so it returns errors.
            Ret = luerl_comp:string(S, [verbose,return]),
            load_ret(Ret, St);
        error -> badarg_error(loadstring, As, St)
    end.

load_ret({ok,Chunk}, St0) ->
    {Func,St1} = luerl_emul:load_chunk(Chunk, St0),
    {[Func],St1};
load_ret({error,[{_,Mod,E}|_],_}, St) ->
    Msg = unicode:characters_to_binary(Mod:format_error(E)),
    {[nil,Msg],St}.

pcall(_, [F|As], St0) ->
    try
        {Rs,St1} = luerl_emul:functioncall(F, As, St0),
        {[true|Rs],St1}
    catch
        %% Only catch Lua errors here, signal system errors.
        error:{lua_error,{error_call, Eas},St2} ->
            Msg = case Eas of
                      [E|_] ->
                          {Str,_} = luerl_lib:tostring(E, St2),
                          Str;
                      [] -> <<"nil">>
                  end,
            {[false,Msg],St2};
        error:{lua_error,E,St2} ->
            Msg = luerl_lib:format_error(E),
            {[false,Msg],St2}
    end.

%% Lua 5.1 compatibility functions.

unpack(_, As, St) -> luerl_lib_table:unpack(As, St).
