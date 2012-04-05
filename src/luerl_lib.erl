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

-export([lua_error/1,errormsg/1,is_true/1,first_value/1,number_to_list/1,
	 to_list/1,to_lists/1,to_lists/2,to_int/1,to_ints/1,to_ints/2,
	 tonumber/1,tonumber/2,tonumbers/1,tonumbers/2,tointeger/1,
	 tointegers/1,tointegers/2,tostring/1,tostrings/1,tostrings/2,
	 conv_list/2,conv_list/3]).

lua_error(E) -> error({lua_error,E}).

errormsg({badarg,Where,As}) ->
    io_lib:format("badarg in ~w: ~w", [Where,As]);
errormsg({illegal_key,Tab,Key}) ->
    io_lib:format("invalid key in ~w: ~w", [Tab,Key]);
errormsg({illegal_index,Where,I}) ->
    io_lib:format("invalid index in ~w: ~w", [Where,I]);
errormsg({illegal_val,Where,Val}) ->
    io_lib:format("invalid value in ~w: ~w", [Where,Val]);
errormsg({illegal_op,Op}) ->
    io_lib:format("illegal op: ~w", [Op]);
errormsg({undefined_op,Op}) ->
    io_lib:format("undefined op: ~w", [Op]).

%% is_true(Rets) -> boolean()>

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
    L = binary_to_list(B),
    case catch {ok,list_to_integer(L)} of
	{ok,I} -> I;
	{'EXIT',_} ->
	    case catch {ok,list_to_float(L)} of
		{ok,F} -> round(F);
		{'EXIT',_} -> nil
	    end
    end;
to_int(_) -> nil.

to_ints(As) -> to_ints(As, []).

to_ints(As, Acc) ->
    to_loop(As, fun to_int/1, Acc).

%% tonumber(Arg) -> Number | nil.
%% tonumber(Arg, Base) -> Number | nil.
%% Tonumber/2 only generates "integers". Lua does it like that.

tonumber(N) when is_number(N) -> N;
tonumber(B) when is_binary(B) ->
    L = binary_to_list(B),
    case catch {ok,list_to_integer(L)} of
	{ok,I} -> float(I);
	{'EXIT',_} ->
	    case catch {ok,list_to_float(L)} of
		{ok,F} -> F;
		{'EXIT',_} -> nil
	    end
    end;
tonumber(_) -> nil.

tonumber(A, B) when is_number(A), round(A) == A ->
    case catch {ok,list_to_integer(integer_to_list(round(A)), B)} of
	{ok,N} -> float(N);
	{'EXIT',_} -> nil
    end;
tonumber(A, B) when is_binary(A) ->
    L = binary_to_list(A),
    case catch {ok,list_to_integer(L, B)} of
	{ok,I} -> float(I);
	{'EXIT',_} -> nil
    end;
tonumber(_, _) -> nil.

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
