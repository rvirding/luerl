%% Copyright (c) 2013-2025 Robert Virding
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

?MODULEDOC(false).

-export([lua_error/2,badarg_error/3,badarith_error/3,
         format_error/1,format_value/1]).

-export([boolean_value/1,first_value/1]).

-export([number_to_list/1]).

-export([arg_to_list/1,args_to_lists/1,args_to_lists/2]).

-export([arg_to_number/1,arg_to_number/2,args_to_numbers/1,args_to_numbers/2]).

-export([arg_to_integer/1,args_to_integers/1,args_to_integers/2]).

-export([arg_to_float/1,args_to_floats/1,args_to_floats/2]).

-export([arg_to_string/1,args_to_strings/1,args_to_strings/2]).

-export([conv_list/2,conv_list/3]).

-export([tostring/2]).

-dialyzer({[no_return], [lua_error/2, badarg_error/3, badarith_error/3]}).

-spec lua_error(_,_) -> no_return().
-spec badarg_error(_,_,_) -> no_return().

%% lua_error(Error, State) -> no_return().
%% badarg_error(What, Args, State) -> no_return().
%% badarith_error(What, Args, State) -> no_return().

lua_error(E, St) -> error({lua_error,E,St}).

badarg_error(What, Args, St) -> lua_error({badarg,What,Args}, St). 

badarith_error(What, Args, St) -> lua_error({badarith,What,Args}, St).

%% format_error(LuaError) -> ErrorString.
%%  The ErrorString is a UTF-8 encoded binary. The UTF-8 encoded
%%  codepoints can come from Lua/Luerl strings.  Some of these use
%%  same text as Lua error string, so be careful if modifying them.

format_error({badarg,Where,Args}) ->
    %% Note Args is a list which must be kept as its own list!
    format_error("bad argument ~ts to ~ts", [Args,Where]);
format_error({badarith,Op,Args}) ->
    %% Note Args is a list which must be kept as its own list!
    format_error("bad arithmetic ~ts on ~ts", [Op,Args]);
format_error({illegal_index,Where,Index}) ->
    format_error("invalid index in ~ts: ~ts", [Where,Index]);
format_error({illegal_value,Where,Val}) ->
    format_error("invalid value in ~ts: ~ts", [Where,Val]);
format_error({illegal_value,Val}) ->
    format_error("invalid value: ~ts", [Val]);
format_error({illegal_comp,Where}) ->
    format_error(<<"illegal comparison in ~ts">>, [Where]);
%% format_error({invalid_order,Where}) ->          %Keep text!
%%     format_error(<<"invalid order function in ~w">>, [Where]);
format_error({undefined_function,Name}) ->
    format_error(<<"undefined function ~ts">>, [Name]);
format_error({undefined_method,Object,Name}) ->
    format_error(<<"undefined method ~ts in ~ts">>, [Name,Object]);
format_error(illegal_return_value) ->
    <<"illegal format of return value">>;
format_error({illegal_return_value,Func}) ->
    format_error(<<"illegal format of return value to ~ts">>, [Func]);
%% Pattern errors.
format_error(invalid_pattern) ->                %Keep text!
    <<"malformed pattern">>;
format_error(invalid_capture) ->                %Keep text!
    <<"malformed pattern">>;
format_error({invalid_char_class,C}) ->         %Keep text!
    Msg = io_lib:format("malformed pattern (class ~c)", [C]),
    unicode:characters_to_binary(Msg);
format_error(invalid_char_set) ->               %Keep text!
    <<"malformed pattern (missing ']')">>;
%% Illegal or undefined ops.
format_error({illegal_op,Op}) ->
    format_error("illegal op: ~ts", [Op]);
format_error({no_module,Mod}) ->
    %% We know the module name is an atom.
    Msg = io_lib:format("module '~s' not found", [Mod]),
    unicode:characters_to_binary(Msg);
%% Assertions
format_error({assert_error,Obj}) ->
    if is_binary(Obj) ->
            <<Obj/binary,$!>>;
       true ->
            Type = luerl_lib_basic:type(Obj),
            <<"error object is a ",Type/binary,$!>>
    end;
%% We have an error message here already.
format_error({error_message,Msg}) ->
    Msg;
%% Error is called.
format_error({error_call,Args}) ->
    format_error_call(Args);
%% binary is passed, we treat as an error message
format_error(Binary) when is_binary(Binary) ->
    <<Binary/binary, "!">>;
%% Everything we don't recognise or know about.
format_error(Error) ->
    unicode:characters_to_binary(io_lib:format(<<"~w!">>, [Error])).

%% format_error_call(Args) -> ErrorString.
%%  Just get it more or less equivalent to what Lua does.

format_error_call([A|_]) when is_binary(A) -> A;
format_error_call([A|_]) when is_number(A) ->
    iolist_to_binary(format_value(A));
format_error_call(Args) ->
    Type = case Args of
               [A|_] -> luerl_lib_basic:type(A);
               [] -> <<"nil">>
           end,
    <<"error object is a ",Type/binary," value">>.

%% format_error(FormatString, Values) -> ErrorString.
%%  Useful when all the values in the list need to be formatted
%%  separately and will be output separately.

format_error(Format, Vals) ->
    Ps = lists:map(fun format_value/1, Vals),
    unicode:characters_to_binary(io_lib:format(Format, Ps)).

%% format_value(LuerlData) -> Iolist.
%%  Take an Luerl data and return a "printable" representation suitable
%%  to use when printing error messages.

format_value(nil) -> <<"nil">>;
format_value(true) -> <<"true">>;
format_value(false) -> <<"false">>;
format_value(N) when is_number(N) -> io_lib:write(N);
format_value(B) when is_binary(B) ->
    %% A luerl string which we print with quotes around it.
    %% Note that the string can contain unicode codepoints.
    [$\',B,$\'];
format_value(#tref{}) -> <<"table">>;
format_value(#usdref{}) -> <<"userdata">>;
format_value(#funref{}) -> <<"function">>;
format_value(#erl_func{code=Fun}) ->
    {name,Name} = erlang:fun_info(Fun, name),
    atom_to_binary(Name, utf8);
format_value(#erl_mfa{f=Func}) -> atom_to_binary(Func, utf8);
format_value(List) when is_list(List) ->
    Pl = lists:map(fun format_value/1, List),
    lists:join($\,, Pl);
%% Treat atoms as binary strings here, probably just a name.
format_value(A) when is_atom(A) ->
    [$\',atom_to_binary(A, utf8),$\'];
%% Everything else just straight through.
format_value(_Other) -> <<"unknown stuff">>.

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
    io_lib:write(N).
    %% case ?IS_FLOAT_INT(N, I) of                 %Is it an "integer"?
    %%     true -> integer_to_list(I);
    %%     false -> io_lib:write(N)
    %% end.

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

%% arg_to_float(Arg) -> Float | 'error'.
%% args_to_floats(Args) -> Floats | 'error'.
%% args_to_floats(Arg, Arg) -> Floats | 'error'.
%%  Convert arguments to rounded floats.

arg_to_float(A) ->
    case arg_to_number(A) of
	N when is_integer(N) -> float(N);
	N when is_float(N) -> N;
	_Other -> error
    end.

args_to_floats(A1, A2) ->
    case arg_to_float(A1) of
	error -> error;
	N1 ->
	    case arg_to_float(A2) of
		error -> error;
		N2 -> [N1,N2]
	    end
    end.

args_to_floats(As) ->
    to_loop(As, fun arg_to_float/1, []).

%% arg_to_string(Arg) -> String | error.
%% arg_to_strings(Args) -> Strings | error.
%% arg_to_strings(Arg, Arg) -> Strings | error.

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

%% tostring(Data, State) -> {Ret,State} | LuaError
%%  Convert Data to a string representation using the standard method
%%  taking into account both the datatype and __tostring and __name
%%  metakeys.

tostring(Data, St) ->
    case luerl_heap:get_metamethod(Data, <<"__tostring">>, St) of
        Meta when Meta =/= nil ->
            tostring_meta(Data, Meta, St);
        nil ->
            case luerl_heap:get_metamethod(Data, <<"__name">>, St) of
                Tag when is_binary(Tag) ->
                    {tostring_tag(Data, Tag),St};
                _Other ->                       %Even nil here
                    {tostring_tag(Data, false),St}
            end
    end.

%% tostring_meta(Arg, Meta, St0) when ?IS_FUNCTION(Meta) ->
tostring_meta(Arg, Meta, St0) ->
    %% We will be nice here!
    {[Ret|_],St1} = luerl_emul:functioncall(Meta, [Arg], St0),
    if is_binary(Ret) -> {Ret,St1};
       is_number(Ret) -> {iolist_to_binary(io_lib:write(Ret)),St1};
       true ->
            lua_error({illegal_return_value,tostring}, St1)
    end.

%% tostring_tag(Data, NameTag) -> Binary.
%%  We ignore the NameTag for the types with untagged values.

tostring_tag(nil, _Name) -> <<"nil">>;
tostring_tag(false, _Name) -> <<"false">>;
tostring_tag(true, _Name) -> <<"true">>;
tostring_tag(N, _Name) when is_number(N) ->
    %% A = abs(N),
    %% %% Print really big/small "integers" as floats as well.
    %% S = if ?IS_FLOAT_INT(N), A < 1.0e14 ->
    %%             integer_to_list(round(N));
    %%        true -> io_lib:write(N)
    %%     end,
    iolist_to_binary(io_lib:write(N));
tostring_tag(S, _Name) when is_binary(S) -> S;
tostring_tag(#tref{i=I}, Name) ->
    iolist_to_binary([get_tag(Name, <<"table">>),integer_to_list(I)]);
tostring_tag(#usdref{i=I}, Name) ->
    iolist_to_binary([get_tag(Name, <<"userdata">>),integer_to_list(I)]);
tostring_tag(#funref{i=I}, Name) ->           %Functions defined in Lua
    iolist_to_binary([get_tag(Name, <<"function">>),integer_to_list(I)]);
tostring_tag(#erl_func{code=C}, Name) ->      %Erlang functions
    iolist_to_binary([get_tag(Name, <<"function">>),io_lib:write(C)]);
tostring_tag(#erl_mfa{m=M,f=F}, Name) ->      %Erlang MFA triplets
    Tag = get_tag(Name, <<"function">>),
    iolist_to_binary([Tag,io_lib:write_atom(M),<<":">>,io_lib:write_atom(F)]);
tostring_tag(#thread{}, Name) ->
    get_tag(Name, <<"thread">>);
tostring_tag(_, _Name) ->
    <<"unknown">>.

get_tag(Name, Type) ->
    if is_binary(Name) -> <<Name/binary,": ">>;
       true -> <<Type/binary,": ">>
    end.
