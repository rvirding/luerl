%% Copyright (c) 2013-2019 Robert Virding
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

%% File    : luerl_lib.erl
%% Author  : Robert Virding
%% Purpose : Luerl libraries.

%% A collection of useful functions. Those with '_' in their names
%% generate Erlang data types while those with generate Lua data types
%% (floats and binaries).

-module(luerl_lib).

-include("luerl.hrl").

-export([lua_error/2,badarg_error/3,format_error/1]).

-export([boolean_value/1,first_value/1]).

-export([number_to_list/1]).

-export([arg_to_list/1,args_to_lists/1,args_to_lists/2]).

-export([arg_to_number/1,arg_to_number/2,args_to_numbers/1,args_to_numbers/2]).

-export([arg_to_integer/1,args_to_integers/1,args_to_integers/2]).

-export([arg_to_string/1,args_to_strings/1,args_to_strings/2]).

-export([conv_list/2,conv_list/3]).

-spec lua_error(_,_) -> no_return().
-spec badarg_error(_,_,_) -> no_return().

lua_error(E, St) -> error({lua_error,E,St}).

badarg_error(What, Args, St) -> lua_error({badarg,What,Args}, St). 

%% format_error(LuaError) -> ErrorString.
%%  Some of these use same text as Lua error string, so be careful if
%%  modifying them.

format_error({undefined_method, Name, Args0, Line}) ->
    io_lib:format("undefined_method ~w with args: ~p on line ~p",
                  [Name, Args0, Line]);
format_error({badarg,Where,As}) ->
    io_lib:format("badarg in ~w: ~w", [Where,As]);
format_error({method_on_nil, Key}) ->
    io_lib:format("undefined method ~w on nil", [Key]);
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
format_error({undef_function,Name}) ->
    io_lib:format("undefined function ~w", [Name]);
format_error({undef_method,Obj,Name}) ->
    io_lib:format("undefined method in ~w: ~w", [Obj,Name]);
%% Pattern errors.
format_error(invalid_pattern) ->		%Keep text!
    io_lib:format("malformed pattern", []);
format_error(invalid_capture) ->		%Keep text!
    io_lib:format("malformed pattern", []);
format_error({invalid_char_class,C}) ->		%Keep text!
    io_lib:format("malformed pattern (class ~c)", [C]);
format_error(invalid_char_set) ->		%Keep text!
    io_lib:format("malformed pattern (missing ']')", []);
%% Illegal or undefined ops.
format_error({illegal_op,Op}) ->
    io_lib:format("illegal op: ~w", [Op]);
format_error({undefined_op,Op}) ->
    io_lib:format("undefined op: ~w", [Op]);
format_error({no_module,Mod}) ->
    io_lib:format("module '~s' not found", [Mod]).

%% boolean_value(Rets) -> boolean().
%% first_value(Rets) -> Value | nil.
%%  Test first value of return list.

boolean_value([V|_]) -> ?IS_TRUE(V);
boolean_value([]) -> false.

first_value([V|_]) -> V;
first_value([]) -> nil.

%% bin_to_number(Binary) -> {ok,Number} | error.
%% str_to_number(String) -> {ok,Number} | error.
%%  Use the scanner to process all allowed number syntaxes.

bin_to_number(B) -> str_to_number(binary_to_list(B)).

str_to_number(S) ->
    case luerl_scan:string(S) of
	{ok,[{'NUMERAL',_,N}],_} -> {ok,N};
	{ok,[{'+',_},{'NUMERAL',_,N}],_} -> {ok,N};
	{ok,[{'-',_},{'NUMERAL',_,N}],_} -> {ok,-N};
	_ -> error
    end.

number_to_list(N) ->
    case ?IS_FLOAT_INT(N, I) of			%Is it an "integer"?
	true -> integer_to_list(I);
	false -> io_lib:write(N)
    end.

%% arg_to_list(Arg) -> List | 'error'.
%% args_to_lists(Args) -> Lists | 'error'.
%% args_to_lists(Args, Acc) -> Lists | 'error'.

arg_to_list(N) when is_number(N) -> number_to_list(N);
arg_to_list(B) when is_binary(B) -> binary_to_list(B);
arg_to_list(_) -> error.

args_to_lists(As) -> args_to_lists(As, []).

args_to_lists(As, Acc) ->
    to_loop(As, fun arg_to_list/1, Acc).

%% arg_to_number(Arg) -> Number | error.
%% arg_to_number(Arg, Base) -> Number | error.
%% args_to_numbers(Args) -> Numbers | 'error'.
%% args_to_numbers(Arg, Arg) -> Numbers | 'error'.
%%  Strings always result in floats.
%%  Arg_to_number/2 only generates "integers". Lua does it like that.

arg_to_number(N) when is_number(N) -> N;
arg_to_number(B) when is_binary(B) ->
    case bin_to_number(B) of
	{ok,N} -> float(N);
	error -> error
    end;
arg_to_number(_) -> error.

arg_to_number(A, B) ->
    case conv_list([A,B], [erl_list,lua_integer]) of
	[N0,Base] ->
	    case catch begin [N1] = string:tokens(N0, [9,10,11,12,13,32,160]),
			     {ok,list_to_integer(N1, Base)} end of
		{ok,I} -> float(I);
		_ -> error
	    end
    end.

%% arg_to_number(A, B) ->
%%     case args_to_numbers([A,B]) of
%% 	[N1,N2] when ?IS_FLOAT_INT(N1) ->
%% 	    N1 * math:pow(10,N2);
%% 	error -> error
%%     end.

args_to_numbers(A1, A2) ->
    case luerl_lib:arg_to_number(A1) of
	error -> error;
	N1 ->
	    case luerl_lib:arg_to_number(A2) of
		error -> error;
		N2 -> [N1,N2]
	    end
    end.

args_to_numbers(As) ->
    to_loop(As, fun arg_to_number/1, []).

%% arg_to_integer(Arg) -> Integer | 'error'.
%% args_to_integers(Args) -> Integers | 'error'.
%% args_to_integers(Arg, Arg) -> Integers | 'error'.
%%  Convert arguments to rounded integers.

arg_to_integer(A) ->
    case arg_to_number(A) of
	N when is_integer(N) -> N;
	N when ?IS_FLOAT_INT(N) -> round(N);
	_Other -> error				%Other floats are bad here
    end.

args_to_integers(A1, A2) ->
    case arg_to_integer(A1) of
	error -> error;
	N1 ->
	    case arg_to_integer(A2) of
		error -> error;
		N2 -> [N1,N2]
	    end
    end.

args_to_integers(As) ->
    to_loop(As, fun arg_to_integer/1, []).

arg_to_string(N) when is_number(N) -> list_to_binary(number_to_list(N));
arg_to_string(B) when is_binary(B) -> B;
arg_to_string(_) -> error.

args_to_strings(As) -> args_to_strings(As, []).

args_to_strings(As, Acc) ->
    to_loop(As, fun arg_to_string/1, Acc).

%% to_loop(List, Convert, Acc) -> List | 'error'.
%%  Step over list using foldl and return list or 'error'. We assume
%%  the list won't be very long so appending is ok.

to_loop([A|As], Fun, Acc) ->
    case Fun(A) of
	error -> error;				%Terminate on error
	E -> to_loop(As, Fun, Acc ++ [E])
    end;
to_loop([], _Fun, Acc) -> Acc.

%% conv_list(Args, ToTypes) -> List | 'error'.
%% conv_list(Args, ToTypes, Done) -> List | 'error'.
%%  Basically a type driven foldl where we return a list or 'error'.

conv_list(As, Tos) -> conv_list(As, Tos, []).

conv_list(_, _, error) -> error;		%Propagate error
conv_list([A|As], [To|Tos], Rs) ->
    %% Get the right value.
    Ret = case To of
	      %% Erlang types.
	      erl_list -> arg_to_list(A);
	      erl_string -> arg_to_list(A);
	      %% Lua types.
	      lua_any -> A;
	      lua_integer -> arg_to_integer(A);
	      lua_number -> arg_to_number(A);
	      lua_string -> arg_to_string(A);
	      lua_bool -> ?IS_TRUE(A)
	  end,
    case Ret of
	error -> error;				%Return error
	Ret -> 
	    conv_list(As, Tos, [Ret|Rs])
    end;
conv_list([], _, Rs) -> lists:reverse(Rs);	%No more arguments, done
conv_list(_, [], Rs) -> lists:reverse(Rs).	%No more conversions, done
