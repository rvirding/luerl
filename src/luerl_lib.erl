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

%% File    : luerl_lib.erl
%% Author  : Robert Virding
%% Purpose : Luerl libraries.

%% A collection of useful functions. Those with '_' in their names
%% generate Erlang data types while those with generate Lua data types
%% (floats and binaries).

-module(luerl_lib).

-include("luerl.hrl").

-export([lua_error/1,format_error/1,is_true/1,first_value/1,number_to_list/1,
	 to_list/1,to_lists/1,to_lists/2,to_int/1,to_ints/1,to_ints/2,
	 tonumber/1,tonumber/2,tonumbers/1,tonumbers/2,tointeger/1,
	 tointegers/1,tointegers/2,tostring/1,tostrings/1,tostrings/2,
	 conv_list/2,conv_list/3]).

-export([anew/1,asiz/1,aget/2,aset/3,aclr/2,asl/3,asr/3]).

-spec lua_error(_) -> no_return().

lua_error(E) -> error({lua_error,E}).

%% Experimental structure for the array/list part of a table. List of
%% segments containing values, all nil fields are gaps.
%% Array :: [{First,Last,Elements}].

anew(_) -> [].

asiz(A) -> asiz(A, 0).

asiz([{_,L,_}|A], _) -> asiz(A, L);
asiz([], S) -> S.

aget(I, [{I,_,Es}|_]) -> hd(Es);		%First element
aget(I, [{F,L,Es}|_]) when I >= F, I =< L ->	%It's in here
    lists:nth(I-F+1, Es);
aget(I, [{_,L,_}|A]) when I > L ->		%Not yet
    aget(I, A);
aget(_, _) -> nil.				%Not at all

aset(I, nil, A) -> aclr(I, A);			%Setting to nil is clearing
aset(I, V, [{F,L,Es}|A]) when I >= F, I =< L -> %Set it here
    [{F,L,setnth(I-F+1, V, Es)}|A];
aset(I, V, [{F,L,Es}]) when I =:= L+1, F-L < 10 ->
    [{F,I,Es ++ [V]}];
aset(I, V, [{_,L,_}=S|A]) when I > L ->		%Not yet
    [S|aset(I, V, A)];
aset(I, V, [{F,L,Es}|A]) when I == F-1 ->	%Drop it at head
    [{F-1,L,[V|Es]}|A];
aset(I, V, A) ->
    [{I,I,[V]}|A].

aclr(I, [{F,L,[_|Es]}|A]) when I =:= F ->	%Prepend it
    [{F+1,L,Es}|A];
aclr(I, [{F,L,Es}|A]) when I =:= L ->
    [{F,L-1,lists:sublist(Es, L-F)}|A];
aclr(I, [{F,L,Es}|A]) when I > F, I < L ->
    Bc = I-F,
    {Bef,[_|Aft]} = lists:split(Bc, Es),	%Split and drop
    [{F,I-1,Bef},{I+1,L,Aft}|A];
aclr(I, [S|A]) -> [S|aclr(I, A)];
aclr(I, A) -> A.

asl(_, _, A) -> A.

asr(_, _, A) -> A.

setnth(1, V, [_|Es]) -> [V|Es];
setnth(N, V, [E|Es]) -> [E|setnth(N-1, V, Es)].

%% format_error(LuaError) -> ErrorString.
%%  Some of these use same text as Lua error string, so be careful if
%%  modifying them.

format_error({badarg,Where,As}) ->
    io_lib:format("badarg in ~w: ~w", [Where,As]);
format_error({illegal_key,Tab,Key}) ->
    io_lib:format("invalid key in ~w: ~w", [Tab,Key]);
format_error({illegal_index,Where,I}) ->
    io_lib:format("invalid index in ~w: ~w", [Where,I]);
format_error({illegal_val,Where,Val}) ->
    io_lib:format("invalid value in ~w: ~w", [Where,Val]);
format_error({illegal_val,Val}) ->
    io_lib:format("invalid value: ~w", [Val]);
format_error({illegal_comp,Where}) ->
    io_lib:format("illegal comparison in ~w", [Where]);
format_error({invalid_order,Where}) ->		%Keep text!
    io_lib:format("invalid order function in ~w", [Where]);
%% Pattern errors.
format_error(invalid_pattern) ->		%Keep text!
    io_lib:format("malformed pattern", []);
format_error(invalid_capture) ->		%Keep text!
    io_lib:format("malformed pattern", []);
format_error({invalid_char_class,C}) ->		%Keep text!
    io_lib:format("malformed pattern: class ~c", [C]);
format_error(invalid_char_set) ->		%Keep text!
    io_lib:format("malformed pattern: missing ]", []);
%% Illegal or undefined ops.
format_error({illegal_op,Op}) ->
    io_lib:format("illegal op: ~w", [Op]);
format_error({undefined_op,Op}) ->
    io_lib:format("undefined op: ~w", [Op]).

%% is_true(Rets) -> boolean()>
%% first_value(Rets) -> Value | nil.

is_true([nil|_]) -> false;
is_true([false|_]) -> false;
is_true([_|_]) -> true;
is_true([]) -> false.

first_value([V|_]) -> V;
first_value([]) -> nil.

to_list(N) when is_number(N) -> number_to_list(N);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(_) -> nil.

to_lists(As) -> to_lists(As, []).

to_lists(As, Acc) ->
    to_loop(As, fun to_list/1, Acc).

to_int(N) when is_number(N) -> round(N);
to_int(B) when is_binary(B) ->
    case bin_to_float(B) of
	{ok,N} -> round(N);
	error -> nil
    end;
to_int(_) -> nil.

to_ints(As) -> to_ints(As, []).

to_ints(As, Acc) ->
    to_loop(As, fun to_int/1, Acc).

%% bin_to_float(Binary) -> {ok,Number} | error.
%% str_to_float(String) -> {ok,Number} | error.
%%  Use the scanner to process all allowed number syntaxes.

bin_to_float(B) -> str_to_float(binary_to_list(B)).

str_to_float(S) ->
    case luerl_scan:string(S) of
	{ok,[{'NUMBER',_,N}],_} -> {ok,N};
	{ok,[{'+',_},{'NUMBER',_,N}],_} -> {ok,N};
	{ok,[{'-',_},{'NUMBER',_,N}],_} -> {ok,-N};
	_ -> error
    end.

%% tonumber(Arg) -> Number | nil.
%% tonumber(Arg, Base) -> Number | nil.
%% Tonumber/2 only generates "integers". Lua does it like that.

tonumber(N) when is_number(N) -> N;
tonumber(B) when is_binary(B) ->
    case bin_to_float(B) of
	{ok,N} -> N;
	error -> nil
    end;
tonumber(_) -> nil.

tonumber(A, B) ->
    case conv_list([A,B], [list,integer]) of
	[N0,Base] ->
	    case catch begin [N1] = string:tokens(N0, [9,10,11,12,13,32,160]),
			     {ok,list_to_integer(N1, Base)} end of
		{ok,I} -> float(I);
		_ -> nil
	    end
    end.

%% tonumber(A, B) ->
%%     case tonumbers([A,B]) of
%% 	[N1,N2] when ?IS_INTEGER(N1) ->
%% 	    N1 * math:pow(10,N2);
%% 	nil -> nil
%%     end.

tointeger(A) ->
    case tonumber(A) of
	nil -> nil;
	N -> float(round(N))
    end.

tonumbers(As) -> tonumbers(As, []).

tonumbers(As, Acc) ->
    to_loop(As, fun tonumber/1, Acc).

tointegers(As) -> tointegers(As, []).

tointegers(As, Acc) ->
    to_loop(As, fun tointeger/1, Acc).

number_to_list(N) ->
    I = round(N),
    case I == N of				%Is it an "integer"?
	true -> integer_to_list(I);
	false -> io_lib:write(N)
    end.

tostring(N) when is_number(N) -> list_to_binary(number_to_list(N));
tostring(B) when is_binary(B) -> B;
tostring(_) -> nil.

tostrings(As) -> tostrings(As, []).

tostrings(As, Acc) ->
    to_loop(As, fun tostring/1, Acc).

%% to_loop(List, Convert, Acc) -> List | nil.

to_loop(As, Fun, Acc) ->
    lists:foldr(fun (_, nil) -> nil;		%Propagate nil
		    (A, Ns) ->
			case Fun(A) of
			    nil -> nil;		%Propagate nil
			    N -> [N|Ns]
			end
		end, Acc, As).

%% conv_list(Args, ToTypes) -> List | nil.
%% conv_list(Args, ToTypes, Done) -> List | nil.
%% Basically a type driven foldr where we return a list or nil.

conv_list(As, Tos) -> conv_list(As, Tos, []).

conv_list(_, _, nil) -> nil;			%Propagate nil
conv_list([A|As], [To|Tos], Rs0) ->
    case conv_list(As, Tos, Rs0) of
	nil -> nil;				%Propagate nil
	Rs1 ->
	    %% Get the right value.
	    Ret = case To of
		      %% Erlang types.
		      list -> to_list(A);
		      integer -> to_int(A);
		      string -> to_list(A);
		      %% Lua types.
		      lany -> A;
		      linteger -> tointeger(A);
		      lnumber -> tonumber(A);
		      lstring -> tostring(A);
		      lbool -> ?IS_TRUE(A)
		  end,
	    case Ret of
		nil -> nil;			%Propagate nil
		Ret -> [Ret|Rs1]
	    end
    end;
conv_list([], _, Acc) -> Acc;			%No more arguments, done
conv_list(_, [], Acc) -> Acc.			%No more conversions, done
