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

%% File    : luerl_scan.xrl
%% Author  : Robert Virding
%% Purpose : Token definitions for LUA.

Definitions.

D = [0-9]
H = [0-9A-Fa-f]
U = [A-Z]
L = [a-z]

Rules.

%% Names/identifiers.
({U}|{L}|_)({U}|{L}|_|{D})* :
	name_token(TokenChars, TokenLine).
%% Numbers, we separately parse (Erlang) integers and floats.
%% Integers.
{D}+ : 
	case catch {ok,list_to_integer(TokenChars)} of
	    {ok,I} -> {token,{'NUMERAL',TokenLine,I}};
	    _ -> {error,"illegal number"}
	end.
0[xX]{H}+ :
        Int = list_to_integer(string:substr(TokenChars, 3), 16),
        {token,{'NUMERAL',TokenLine,Int}}.

%% Floats, we have separate rules to make them easier to handle.
{D}+\.{D}+([eE][-+]?{D}+)? :
	case catch {ok,list_to_float(TokenChars)} of
	    {ok,F} -> {token,{'NUMERAL',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.
{D}+[eE][-+]?{D}+ :
	[M,E] = string:tokens(TokenChars, "eE"),
	case catch {ok,list_to_float(M ++ ".0e" ++ E)} of
	    {ok,F} -> {token,{'NUMERAL',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.
{D}+\.([eE][-+]?{D}+)? :
	[M|E] = string:tokens(TokenChars, "."),
	case catch {ok,list_to_float(lists:append([M,".0"|E]))} of
	    {ok,F} -> {token,{'NUMERAL',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.
\.{D}+([eE][-+]?{D}+)? :
	case catch {ok,list_to_float("0" ++ TokenChars)} of
	    {ok,F} -> {token,{'NUMERAL',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.

%% Hexadecimal floats, we have one complex rule to handle bad formats
%% more like the Lua parser.

0[xX]{H}*\.?{H}*([pP][+-]?{D}+)? :
	hex_float_token(TokenChars, TokenLine).

%% Strings. 
%% Handle the illegal newlines in string_token.
\"(\\.|\\\n|[^"\\])*\" :
	string_token(TokenChars, TokenLen, TokenLine).
\'(\\.|\\\n|[^'\\])*\' :
	string_token(TokenChars, TokenLen, TokenLine).
\[\[([^]]|\][^]])*\]\] :
	%% Strip quotes.
	Cs = string:substr(TokenChars, 3, TokenLen - 4),
 	long_bracket(TokenLine, Cs).

%% Other known tokens.
\+  : {token,{'+',TokenLine}}.
\-  : {token,{'-',TokenLine}}.
\*  : {token,{'*',TokenLine}}.
\/  : {token,{'/',TokenLine}}.
\// : {token,{'//',TokenLine}}.
\%  : {token,{'%',TokenLine}}.
\^  : {token,{'^',TokenLine}}.
\&  : {token,{'&',TokenLine}}.
\|  : {token,{'|',TokenLine}}.
\~  : {token,{'~',TokenLine}}.
\>> : {token,{'>>',TokenLine}}.
\<< : {token,{'<<',TokenLine}}.
\#  : {token,{'#',TokenLine}}.
==  : {token,{'==',TokenLine}}.
~=  : {token,{'~=',TokenLine}}.
<=  : {token,{'<=',TokenLine}}.
>=  : {token,{'>=',TokenLine}}.
<  :  {token,{'<',TokenLine}}.
>  :  {token,{'>',TokenLine}}.
=  :  {token,{'=',TokenLine}}.
\( : {token,{'(',TokenLine}}.
\) : {token,{')',TokenLine}}.
\{ : {token,{'{',TokenLine}}.
\} : {token,{'}',TokenLine}}.
\[ : {token,{'[',TokenLine}}.
\] : {token,{']',TokenLine}}.
:: : {token,{'::',TokenLine}}.
;  : {token,{';',TokenLine}}.
:  : {token,{':',TokenLine}}.
,  : {token,{',',TokenLine}}.
\. : {token,{'.',TokenLine}}.
\.\. : {token,{'..',TokenLine}}.
\.\.\. : {token,{'...',TokenLine}}.

[\011-\015\s\240]+ : skip_token.		%Mirror Lua here

%% Comments, either -- or --[[ ]].
%%--(\[([^[\n].*|\[\n|[^[\n].*|\n) : skip_token.
--\n :		skip_token.
--[^[\n].* :	skip_token.
--\[\n :	skip_token.
--\[[^[\n].* :	skip_token.

%% comment --ab ... yz  --ab([^y]|y[^z])*yz
--\[\[([^]]|\][^]])*\]\] : skip_token.
--\[\[([^]]|\][^]])* : {error,"unfinished long comment"}.

Erlang code.

-export([is_keyword/1]).

%% name_token(Chars, Line) ->
%%     {token,{'NAME',Line,Symbol}} | {Name,Line} | {error,E}.
%% Build a name from list of legal characters, else error.

name_token(Cs, L) ->
    case catch {ok,list_to_binary(Cs)} of
	{ok,Name} ->
	    case is_keyword(Name) of
		true -> {token,{name_string(Name),L}};
		false -> {token,{'NAME',L,Name}}
	    end;
	_ -> {error,"illegal name"}
    end.

name_string(Name) ->
    binary_to_atom(Name, latin1).		%Only latin1 in Lua

%% hex_float_token(TokenChars, TokenLine) ->
%%     {token,{'NUMERAL',TokenLine,Float}} | {error,E}.
%% Build a float form a hex float string.

hex_float_token(TokenChars, TokenLine) ->
    Tcs = string:substr(TokenChars, 3),
    case lists:splitwith(fun (C) -> (C =/= $p) and (C =/= $P) end, Tcs) of
	{Mcs,[]} when Mcs /= [] ->
	    hex_float(Mcs, [], TokenLine);
	{Mcs,[_P|Ecs]} when Ecs /= [] ->
	    hex_float(Mcs, Ecs, TokenLine);
	_Other -> {error,"malformed number"}
    end.

%% hex_float(Mantissa, Exponent) -> {token,{'NUMERAL',Line,Float}} | {error,E}.
%% hex_mantissa(Chars) -> {float,Float} | error.
%% hex_fraction(Chars, Pow, SoFar) -> Fraction.

hex_float(Mcs, [], Line) ->
    case hex_mantissa(Mcs) of
	{float,M} -> {token,{'NUMERAL',Line,M}};
	error -> {error,"malformed number"}
    end;
hex_float(Mcs, Ecs, Line) ->
    case hex_mantissa(Mcs) of
	{float,M} ->
	    case catch list_to_integer(Ecs, 10) of
		{'EXIT',_} -> {error,"malformed number"};
		E -> {token,{'NUMERAL',Line,M * math:pow(2, E)}}
	    end;
	error -> {error,"malformed number"}
    end.

hex_mantissa(Mcs) ->
    case lists:splitwith(fun (C) -> C =/= $. end, Mcs) of
	{[],[]} -> error;			%Nothing at all
	{[],[$.]} -> error;			%Only a '.'
	{[],[$.|Fcs]} -> {float,hex_fraction(Fcs, 16.0, 0.0)};
	{Hcs,[]} -> {float,float(list_to_integer(Hcs, 16))};
	{Hcs,[$.|Fcs]} ->
	    H = float(list_to_integer(Hcs, 16)),
	    {float,hex_fraction(Fcs, 16.0, H)}
    end.

hex_fraction([C|Cs], Pow, SoFar) when C >= $0, C =< $9 ->
    hex_fraction(Cs, Pow*16, SoFar + (C - $0)/Pow);
hex_fraction([C|Cs], Pow, SoFar) when C >= $a, C =< $f ->
    hex_fraction(Cs, Pow*16, SoFar + (C - $a + 10)/Pow);
hex_fraction([C|Cs], Pow, SoFar) when C >= $A, C =< $F ->
    hex_fraction(Cs, Pow*16, SoFar + (C - $A + 10)/Pow);
hex_fraction([], _Pow, SoFar) -> SoFar.

%% string_token(InputChars, Length, Line) ->
%%      {token,{'LITERALSTRING',Line,Cs}} | {error,E}.
%% Convert an input string into the corresponding string
%% characters. We know that the input string is correct.

string_token(Cs0, Len, L) ->
    Cs1 = string:substr(Cs0, 2, Len - 2),	%Strip quotes
    case catch {ok,chars(Cs1)} of
	{ok,Cs2} ->
	    %% Strings are utf8 encoded.
	    %% io:format("before: ~w\n", [Cs2]),
	    Str = case is_valid_utf8(Cs2) of
		      true ->
			  list_to_binary(Cs2);
		      false ->
			  unicode:characters_to_binary(Cs2, unicode, unicode)
		  end,
	    %% io:format("after: ~w\n", [Str]),
	    {token,{'LITERALSTRING',L,Str}};
	error -> {error,"illegal string"}
    end.

%% Valid UTF-8 bytes and where they can occur.
%% ascii          0 - 7F    0 - 127
%% continutaion  80 - BF  128 - 191
%% first with 1  C2 - DF  194 - 223
%% first with 2  E0 - EF  224 - 239
%% first with 3  F0 - F4  240 - 244

is_valid_utf8([C|Cs]) when C >= 0, C =< 127 -> is_valid_utf8(Cs);
is_valid_utf8([F1|Cs]) when F1 >= 194, F1 =< 223 -> is_cont_utf8(1, Cs);
is_valid_utf8([F2|Cs]) when F2 >= 224, F2 =< 239 -> is_cont_utf8(2, Cs);
is_valid_utf8([F3|Cs]) when F3 >= 240, F3 =< 244 -> is_cont_utf8(3, Cs);
is_valid_utf8([]) -> true;
is_valid_utf8(_Cs) -> false.

is_cont_utf8(1, [C|Cs]) when C >= 128, C =< 191 -> is_valid_utf8(Cs);
is_cont_utf8(N, [C|Cs]) when C >= 128, C =< 191 -> is_cont_utf8(N-1, Cs);
is_cont_utf8(_N, _Cs) -> false.

chars([$\\,C1|Cs0]) when C1 >= $0, C1 =< $9 ->	%1-3 decimal digits
    I1 = C1 - $0,
    case Cs0 of
	[C2|Cs1] when C2 >= $0, C2 =< $9 ->
	    I2 = C2 - $0,
	    case Cs1 of
		[C3|Cs2] when C3 >= $0, C3 =< $9 ->
		    [100*I1 + 10*I2 + (C3-$0)|chars(Cs2)];
		_ -> [10*I1 + I2|chars(Cs1)]
	    end;
	_ -> [I1|chars(Cs0)]
    end;
chars([$\\,$x,C1,C2|Cs]) ->			%2 hex digits
    case hex_char(C1) and hex_char(C2) of
	true -> [hex_val(C1)*16+hex_val(C2)|chars(Cs)];
	false -> throw(error)
    end;
chars([$\\,$z|Cs]) -> chars(skip_space(Cs));	%Skip blanks
chars([$\\,C|Cs]) -> [escape_char(C)|chars(Cs)];
chars([$\n|_]) -> throw(error);
chars([C|Cs]) -> [C|chars(Cs)];
chars([]) -> [].

skip_space([C|Cs]) when C >= 0, C =< $\s -> skip_space(Cs);
skip_space(Cs) -> Cs.

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

hex_val(C) when C >= $0, C =< $9 -> C - $0;
hex_val(C) when C >= $a, C =< $f -> C - $a + 10;
hex_val(C) when C >= $A, C =< $F -> C - $A + 10.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

long_bracket(Line, [$\n|Cs]) ->
    S = list_to_binary(Cs),
    {token,{'LITERALSTRING',Line,S}};
long_bracket(Line, Cs) ->
    S = list_to_binary(Cs),
    {token,{'LITERALSTRING',Line,S}}.

%% is_keyword(Name) -> boolean().
%% Test if the name is a keyword.

is_keyword(<<"and">>) -> true;
is_keyword(<<"break">>) -> true;
is_keyword(<<"do">>) -> true;
is_keyword(<<"else">>) -> true;
is_keyword(<<"elseif">>) -> true;
is_keyword(<<"end">>) -> true;
is_keyword(<<"false">>) -> true;
is_keyword(<<"for">>) -> true;
is_keyword(<<"function">>) -> true;
is_keyword(<<"goto">>) -> true;
is_keyword(<<"if">>) -> true;
is_keyword(<<"in">>) -> true;
is_keyword(<<"local">>) -> true;
is_keyword(<<"nil">>) -> true;
is_keyword(<<"not">>) -> true;
is_keyword(<<"or">>) -> true;
is_keyword(<<"repeat">>) -> true;
is_keyword(<<"return">>) -> true;
is_keyword(<<"then">>) -> true;
is_keyword(<<"true">>) -> true;
is_keyword(<<"until">>) -> true;
is_keyword(<<"while">>) -> true;
is_keyword(_) -> false.
