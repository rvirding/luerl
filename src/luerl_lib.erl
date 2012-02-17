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

-module(luerl_lib).

-include("luerl.hrl").

-export([is_true/1,first_value/1,is_integer/1,number_to_list/1,
	 tonumber/1,tonumber/2,tointeger/1,tonumbers/1,tonumbers/2,
	 tointegers/1,tointegers/2,tolist/1,tostring/1]).

%% is_true(Rets) -> boolean()>

is_true([nil|_]) -> false;
is_true([false|_]) -> false;
is_true([_|_]) -> true;
is_true([]) -> false.

first_value([V|_]) -> V;
first_value([]) -> nil.

%% is_integer(Number) -> boolean().

is_integer(N) -> round(N) == N.    

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
    lists:foldr(fun (_, nil) -> nil;		%Propagate nil
		    (A, Ns) ->
			case tonumber(A) of
			    nil -> nil;		%Propagate nil
			    N -> [N|Ns]
			end
		end, Acc, As).

tointegers(As) -> tointegers(As, []).

tointegers(As, Acc) ->
    lists:foldr(fun (_, nil) -> nil;		%Propagate nil
		    (A, Ns) ->
			case tonumber(A) of
			    nil -> nil;		%Propagate nil
			    N -> [float(round(N))|Ns]
			end
		end, Acc, As).

number_to_list(N) ->
    I = round(N),
    case I == N of				%Is it an "integer"?
	true -> integer_to_list(I);
	false -> io_lib:write(N)
    end.

tolist(N) when is_number(N) -> number_to_list(N);
tolist(B) when is_binary(B) -> binary_to_list(B);
tolist(_) -> nil.

tostring(N) when is_number(N) -> list_to_binary(number_to_list(N));
tostring(B) when is_binary(B) -> B;
tostring(_) -> nil.
