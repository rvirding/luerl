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

%% File    : luerl_lib_utf8.erl
%% Author  : Robert Virding
%% Purpose : The utf8 library for Luerl.

-module(luerl_lib_utf8).

-include("luerl.hrl").

?MODULEDOC(false).

-export([install/1,utf8_char/3,codes/3,codepoint/3,utf8_len/3,offset/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]). %Shorten these

install(St) ->
    luerl_heap:alloc_table(table(), St).

table() ->
    [{<<"char">>,#erl_mfa{m=?MODULE,f=utf8_char}},
     {<<"charpattern">>,<<"[\0-\x7F\xC2-\xF4][\x80-\xBF]*">>},
     {<<"codes">>,#erl_mfa{m=?MODULE,f=codes}},
     {<<"codepoint">>,#erl_mfa{m=?MODULE,f=codepoint}},
     {<<"len">>,#erl_mfa{m=?MODULE,f=utf8_len}},
     {<<"offset">>,#erl_mfa{m=?MODULE,f=offset}}
    ].

%% char(...) -> String.
%%  Receives zero or more integers, converts each one to its
%%  corresponding UTF-8 byte sequence and returns a string with the
%%  concatenation of all these sequences.

utf8_char(_, As, St) ->
    case luerl_lib:args_to_integers(As) of
	Is when is_list(Is) ->
	    Ss = << <<I/utf8>> || I <- Is >>,
	    {[Ss],St};
	error -> badarg_error(char, As, St)
    end.

%% len(...) -> Integer.
%%  Returns the number of UTF-8 characters in string s that start
%%  between positions i and j (both inclusive). The default for i is 1
%%  and for j is -1. If it finds any invalid byte sequence, returns a
%%  false value plus the position of the first invalid byte.

utf8_len(_, As, St) ->
    {Str,I,J} = string_args(As, len, St),
    StrLen = byte_size(Str),
    Ret = if I > J -> [0];			%Do the same as Lua
	     true ->
		  Bin = binary_part(Str, I - 1, StrLen - I + 1),
		  case bin_len(Bin, StrLen - J, 0) of
		      {ok,Size} -> [Size];
		      {error,Rest} -> [nil,StrLen - byte_size(Rest) + 1]
		  end
	  end,
    {Ret,St}.

bin_len(Bin, Last, N) when byte_size(Bin) =< Last -> {ok,N};
bin_len(Bin0, Last, N) ->
    try
	<<_/utf8,Bin1/binary>> = Bin0,
	bin_len(Bin1, Last, N+1)
    catch
	_:_ -> {error,Bin0}
    end.

%% codepoint(...) -> [Integer].
%%  Returns the codepoints (as integers) from all characters in s that
%%  start between byte position i and j (both included). The default
%%  for i is 1 and for j is i. It raises an error if it meets any
%%  invalid byte sequence.

codepoint(_, As, St) ->
    {Str,I,J} = string_args(As, codepoint, St),
    StrLen = byte_size(Str),
    Ret = if I > J -> [];			%Do the same as Lua
	     true ->
		  Bin = binary_part(Str, I - 1, StrLen - I + 1),
		  case bin_codepoint(Bin, StrLen - J, []) of
		      {ok,Cps} -> Cps;
		      {error,_} -> badarg_error(codepoint, As, St)
		  end
	  end,
    {Ret,St}.

bin_codepoint(Bin, Last, Cps) when byte_size(Bin) =< Last ->
    {ok,lists:reverse(Cps)};
bin_codepoint(Bin0, Last, Cps) ->
    try
	<<C/utf8,Bin1/binary>> = Bin0,
	bin_codepoint(Bin1, Last, [C|Cps])
    catch
	_:_ -> {error,Bin0}
    end.

%% codes(String) -> [Fun,String,P].

codes(_, As, St) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	error -> badarg_error(codes, As, St);
	[Str|_] -> {[#erl_func{code=fun codes_next/2},Str,0],St}
    end.

codes_next([A], St) -> codes_next([A,0], St);
codes_next([Str,P|_], St) when byte_size(Str) =< P -> {[nil],St};
codes_next([Str,P|_], St) when is_binary(Str) ->
    <<_:P/binary,C/utf8,Rest/binary>> = Str,
    P1 = byte_size(Str) - byte_size(Rest),
    {[P1,C],St}.

%% offset(String, N, ...) -> Integer.
-spec offset(_, [_], any()) -> no_return().

offset(_, As, St) ->
    _ = string_args(As, offset, St),
    %% We don't do anything yet.
    lua_error({'NYI',offset}, St).

%% string_args(Args, Op, St) -> {String,I,J}.
%%  Return the string, i and j values from the arguments. Generate a
%%  badarg error on bad values.

string_args(As, Op, St) ->
    %% Get the args.
    Args = luerl_lib:conv_list(As, [lua_string,lua_integer,lua_integer]),
    case Args of			%Cunning here, export A1,A2,A3
	[A1,A2,A3|_] -> ok;
	[A1,A2] -> A3 = byte_size(A1);
	[A1] -> A2 = 1, A3 = byte_size(A1);
	error -> A1 = A2 = A3 = ok, badarg_error(Op, As, St)
    end,
    StrLen = byte_size(A1),
    %% Check args and return Str, I, J.
    Str = A1,
    I = if A2 > 0, A2 =< StrLen -> A2;
	   A2 < 0, A2 >= -StrLen -> StrLen + A2 + 1;
	   true -> badarg_error(Op, As, St)
	end,
    J = if A3 > 0, A3 =< StrLen -> A3;
	   A3 < 0, A3 >= -StrLen -> StrLen + A3 + 1;
	   true -> badarg_error(Op, As, St)
	end,
    {Str,I,J}.
