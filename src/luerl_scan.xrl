%% Copyright (c) 2013-2025 Robert Virding
%% -*- mode: erlang; indent-tabs-mode: nil -*-
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
NAME = ({U}|{L}|_|{D})
SNAME = ({U}|{L}|_)

Rules.

%% Names/identifiers.
({U}|{L}|_)({U}|{L}|_|{D})* :
	name_token(TokenChars, TokenLine).

%% Numbers, we parse integers and floats in one go as they can
%% interact with each other.

%% Hexadecimal numbers, we have separate rule to ensure we don't have
%% just a '.'. NOTE THESE MUST COME FIRST TO CATCH 0[xX]!!!!
%%
%% 0[xX]{H}+\.?{H}+([pP][-+]?{D}+)?{NAME}* :
%% 0[xX]{H}+\.?{H}*([pP][-+]?{D}+)?{NAME}* :
%%
%% 0[xX]{H}*\.{H}*([pP][-+]?{D}+)?{NAME}* :

0[xX]{H}*\.?{H}*([pP][-+]?{D}*)?{NAME}* :
	%% io:format("h2 ~p\n", [TokenChars]),
	hex_number_token(TokenChars, TokenLine).

%% Decimal numbers, we separate rules to ensure we don't have just a '.'.
%% Both integers and floats are handled here.
%%
%% {D}*\.?{D}*([eE][-+]?{D}+)?{NAME}*
%% (({D}+\.?{D*})|(\.{D}+))([eE][-+]?{D}+)?{NAME}* :

\.{D}+([eE][-+]?{D}+)?{NAME}* :
	%% io:format("d1 ~p\n", [TokenChars]),
	decimal_number_token(TokenChars, TokenLine).
{D}+\.?{D}*([eE][-+]?{D}+)?{NAME}* :
	%% io:format("d2 ~p\n", [TokenChars]),
	decimal_number_token(TokenChars, TokenLine).

%% Strings.
%% Handle the illegal newlines in string_token.
\"(\\.|\\\n|[^"\\])*\" :
	string_token(TokenChars, TokenLen, TokenLine).
\'(\\.|\\\n|[^'\\])*\' :
	string_token(TokenChars, TokenLen, TokenLine).
%% Handle multi line strings, [[ ]], [=[ ]=], [==[ ]==]
%% This gets a bit tedious as we have to each case separately.
\[\[([^]]|\][^]])*\]\] :
	long_string_token(TokenChars, TokenLen, 2, TokenLine).
\[=\[([^]]|\](=[^]]|[^=]))*\]=\] :
	long_string_token(TokenChars, TokenLen, 3, TokenLine).
\[==\[([^]]|\](==[^]]|=[^=]|[^=]))*\]==\] :
	long_string_token(TokenChars, TokenLen, 4, TokenLine).
\[===\[([^]]|\](===[^]]|==[^=]|=[^=]|[^=]))*\]===\] :
	long_string_token(TokenChars, TokenLen, 5, TokenLine).

%% \[==\[([^]]|\]==[^]]|\]=[^=]|\][^=])*\]==\] :

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

%% Comment --ab ... yz  --ab([^y]|y[^z])*yz
--\[\[([^]]|\][^]])*\]\] : skip_token.
--\[\[([^]]|\][^]])* : {error,"unfinished long comment"}.

%% Catch other illegal tokens.

. :
	illegal_token(TokenChars, TokenLine).

Erlang code.

-include("luerl.hrl").

?MODULEDOC(false).

-export([is_keyword/1]).
-export([string_chars/1,chars/1]).

%% Luerl definitions of these types.
-define(WHITE_SPACE(C), (C >= $\000 andalso C =< $\s)).
-define(ASCII(C), (C >= 0 andalso C =< 127)).
-define(DIGIT(C), (C >= $0 andalso C =< $9)).
-define(HEX(C), (C >= $A andalso C =< $F orelse
                 C >= $a andalso C =< $f orelse
                 ?DIGIT(C))).
-define(CHAR(C), (C >= O andalso C < 16#110000)).
-define(UNICODE(C),
        (is_integer(C) andalso
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF))).

-define(UNI255(C), (is_integer(C) andalso 0 =< C andalso C =< 16#ff)).

%% illegal_token(Chars, Line) -> {error,E}.
%%  Generate a more Lua compatible error message.

illegal_token(Chars, _Line) ->
	{error,"syntax error near '" ++ Chars ++ "'"}.

%% name_token(Chars, Line) ->
%%     {token,{'NAME',Line,Symbol}} | {Name,Line} | {error,E}.
%%  Build a name from list of legal characters, else error.

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
    binary_to_atom(Name, latin1).               %Only latin1 in Lua

%% decimal_number_token(TokenChars, TokenLine)
%%     {token,{'NUMERAL',TokenLine,Float}} | {error,E}.
%%  Build either an integer or a float from a decimal number
%%  string. We first collect the specific number section, then we
%%  create the number. This makes it easier to keep track of which
%%  sections we need to make Lua compliant Luerl numbers, as Lua has
%%  some very "specific" handling.
%%
%%  \.{D}+([eE][-+]?{D}+)?{NAME}* :
%%  {D}+\.?{D}*([eE][-+]?{D}+)?{NAME}* :

decimal_number_token(TokenChars, TokenLine) ->
    %% io:format("dnt ~p\n", [dec_number_split(TokenChars)]),
    Result = case dec_number_split(TokenChars) of
                 %% If there is anything after the number sections then
                 %% it is an error!
                 {_,_,_,Rest} when Rest =/= [] -> error;
                 {[],[],[],_Rest} -> error;     %Nothing at all
                 {[],[],_Ecs,_Rest} -> error;   %No number data
                 {[],".",_Ecs,_Rest} -> error;  %Only "empty" fraction
                 {_,_,[_E],_Rest} -> error;     %Only "empty" exponent
                 {Hcs,Fcs,Ecs,_Rest} ->
                     DW = list_to_integer("0" ++ Hcs),
                     DF = dec_number_fraction(Fcs, DW),
                     Dnum = dec_number_exponent(Ecs, DF),
                     {ok,Dnum}
             end,
    case Result of
        {ok,Number} ->
            {token,{'NUMERAL',TokenLine,Number}};
        error ->
            number_token_error(TokenChars)
    end.

number_token_error(Tcs) ->
    {error,"malformed number near '" ++ Tcs ++ "'"}.

dec_number_split(Tcs0) ->
    Digit = fun (C) -> ?DIGIT(C) end,
    %% The whole number characters.
    {Hcs,Tcs1} = lists:splitwith(Digit, Tcs0),
    %% The fraction characters.
    {Fcs,Tcs2} = dec_number_split_fraction(Tcs1),
    %% The exponent characters.
    {Ecs,Rest} = dec_number_split_exponent(Tcs2),
    {Hcs,Fcs,Ecs,Rest}.

dec_number_split_fraction([$. | Fcs0]) ->
    {Fcs1,Frest} = lists:splitwith(fun (C) -> ?DIGIT(C) end, Fcs0),
    {[$.|Fcs1],Frest};
dec_number_split_fraction(Tcs) ->
    {[],Tcs}.

dec_number_split_exponent([P | Pcs0]) when P =:= $e ; P =:= $E ->
    Digit = fun (C) -> ?DIGIT(C) end,
    case Pcs0 of
        [S | Pcs1] when S =:= $+ ; S =:= $- ->
            {Pcs2,Rest} = lists:splitwith(Digit, Pcs1),
            {[P,S|Pcs2],Rest};
        Pcs1 ->
            {Pcs2,Rest} = lists:splitwith(Digit, Pcs1),
            {[P|Pcs2],Rest}
    end;
dec_number_split_exponent(Tcs) ->
    {[],Tcs}.

dec_number_fraction(".", DW) -> float(DW);
dec_number_fraction([$. | Fcs], DW) ->
    DW + list_to_float("0." ++ Fcs);
dec_number_fraction([], DW) -> DW.

dec_number_exponent([_E | Ecs], DF) ->
    DF * math:pow(10, list_to_integer(Ecs));
dec_number_exponent([], DF) -> DF.

%% hex_number_token(TokenChars, TokenLine)
%%     {token,{'NUMERAL',TokenLine,Float}} | {error,E}.
%%  Build either an integer or a float from a hexadecimal number
%%  string. We first collect the specific number section, then we
%%  create the number. This makes it easier to keep track of which
%%  sections we need to make Lua compliant Luerl numbers, as Lua has
%%  some very "specific" handling.
%%
%%  0[xX]\.{H}+([pP][-+]?{D}+)?{NAME}* :
%%  0[xX]{H}+\.?{H}*([pP][-+]?{D}+)?{NAME}*

hex_number_token([$0,X|TokenChars], TokenLine) ->
    %% io:format("hnt ~p\n", [hex_number_split(TokenChars)]),
    Result = case hex_number_split(TokenChars) of
                 %% If there is anything after the number sections then
                 %% it is an error!
                 {_,_,_,Rest} when Rest =/= [] -> error;
                 {[],[],[],_Rest} -> error;     %Nothing at all
                 {[],[],_Ecs,_Rest} -> error;   %No number data
                 {[],".",_Ecs,_Rest} -> error;  %Only "empty" fraction
                 {_,_,[_P],_Rest} -> error;     %Only "empty" exponent
                 {Hcs,Fcs,Ecs,_Rest} ->
                     HW = list_to_integer("0" ++ Hcs, 16),
                     HF = hex_number_fraction(Fcs, HW),
                     Hnum = hex_number_exponent(Ecs, HF),
                     {ok,Hnum}
             end,
    case Result of
        {ok,Number} ->
            {token,{'NUMERAL',TokenLine,Number}};
        error ->
            number_token_error([$0,X|TokenChars])
    end.

hex_number_split(Tcs0) ->
    Hex = fun (C) -> ?HEX(C) end,
    %% Digit = fun (C) -> ?DIGIT(C) end,
    %% The whole number characters.
    {Hcs,Tcs1} = lists:splitwith(Hex, Tcs0),
    %% The fraction characters.
    {Fcs,Tcs2} = hex_number_split_fraction(Tcs1),
    %% The exponent characters.
    {Ecs,Rest} = hex_number_split_exponent(Tcs2),
    {Hcs,Fcs,Ecs,Rest}.

hex_number_split_fraction([$. | Fcs0]) ->
    {Fcs1,Frest} = lists:splitwith(fun (C) -> ?HEX(C) end, Fcs0),
    {[$.|Fcs1],Frest};
hex_number_split_fraction(Tcs) ->
    {[],Tcs}.

hex_number_split_exponent([P | Pcs0]) when P =:= $p ; P =:= $P ->
    Digit = fun (C) -> ?DIGIT(C) end,
    case Pcs0 of
        [S | Pcs1] when S =:= $+ ; S =:= $- ->
            {Pcs2,Rest} = lists:splitwith(Digit, Pcs1),
            {[P,S|Pcs2],Rest};
        Pcs1 ->
            {Pcs2,Rest} = lists:splitwith(Digit, Pcs1),
            {[P|Pcs2],Rest}
    end;
hex_number_split_exponent(Tcs) ->
    {[],Tcs}.

hex_number_fraction([$. | Fcs], HW) ->
    {HF,_} = hex_number_fraction(Fcs, 16.0, HW + 0.0),
    HF;
hex_number_fraction([], HW) -> HW.

hex_number_exponent([_P | Ecs], HF) ->
    HF * math:pow(2, list_to_integer(Ecs));
hex_number_exponent([], HF) -> HF.

hex_number_fraction([C|Cs], Pow, SoFar) when C >= $0, C =< $9 ->
    hex_number_fraction(Cs, Pow*16.0, SoFar + (C - $0)/Pow);
hex_number_fraction([C|Cs], Pow, SoFar) when C >= $a, C =< $f ->
    hex_number_fraction(Cs, Pow*16.0, SoFar + (C - $a + 10)/Pow);
hex_number_fraction([C|Cs], Pow, SoFar) when C >= $A, C =< $F ->
    hex_number_fraction(Cs, Pow*16.0, SoFar + (C - $A + 10)/Pow);
hex_number_fraction(Cs, _Pow, SoFar) ->
    {SoFar,Cs}.

%% string_token(InputChars, Length, Line) ->
%%     {token,{'LITERALSTRING',Line,Cs}} | {error,Error}.
%%  Convert an input string into the corresponding string characters.
%%  We know that the input string is correct.

string_token([Qc|Cs0], _Len, L) ->
    Cs1 = lists:droplast(Cs0),                  %Strip trailing quote
    %% io:format("st1 ~w ~w\n", [length(Cs1),Cs1]),
    try
        Bytes = string_chars(Cs1),              %The bytes are encoded chars
        String = iolist_to_binary(Bytes),
        %% io:format("st2 ~w ~w\n", [byte_size(String),String]),
        {token,{'LITERALSTRING',L,String}}
    catch
        throw:{string_error,What} ->            %Specific error message
            string_token_error(What, Qc);
        _:_ ->                                  %General error message
            string_token_error("illegal string", Qc)
    end.

string_token_error(What, Qc) ->
    {error,What ++ " near '" ++ [Qc] ++ "'"}.

%% string_chars(Chars)
%% chars(Chars)
%%  Return a list of UTF-8 encoded binaries and one byte unencoded
%%  characters. chars/1 is for external backwards compatibilty.

chars(Cs) ->
    string_chars(Cs).

string_chars(Cs) ->
    string_chars(Cs, []).

string_chars([$\\ | Cs], Acc) ->
    string_bq_chars(Cs, Acc);
string_chars([$\n | _], _Acc) ->
    throw(string_error);
string_chars([C | Cs], Acc) ->
     string_chars(Cs, [C | Acc]);
%% string_chars([C | Cs], Acc) when ?ASCII(C) ->
%%      string_chars(Cs, [C | Acc]);
%% string_chars([C | Cs], Acc) ->
%%     case unicode:characters_to_binary([C]) of
%%         Bin when is_binary(Bin) ->
%%             string_chars(Cs, [Bin | Acc]);
%%         _Error ->
%%             throw(string_error)
%%     end;
string_chars([], Acc) ->
    lists:reverse(Acc).

%% string_bq_chars(Chars, Accumulator)
%%  Handle the backquotes characters.

string_bq_chars([C1|Cs0], Acc) when ?DIGIT(C1) -> %1-3 decimal digits
    I1 = C1 - $0,
    %% Note here we "export" Byte and Cs1 (this is Erlang).
    case Cs0 of
        [C2,C3|Cs1] when ?DIGIT(C2), ?DIGIT(C3) ->
            Byte = 100 * I1 + 10 * (C2 - $0) + (C3 - $0),
            (Byte =< 255) orelse throw(string_error);
        [C2|Cs1] when ?DIGIT(C2)  ->
            Byte = 10 * I1 + (C2 - $0);
        Cs1 ->
            Byte = I1
    end,
    string_chars(Cs1, [Byte | Acc]);
string_bq_chars([$x,C1,C2|Cs], Acc) ->          %2 hex digits
    case ?HEX(C1) andalso ?HEX(C2) of
        true ->
            Byte = hex_val(C1)*16 + hex_val(C2),
            string_chars(Cs, [Byte|Acc]);
        false -> throw({string_error,"hexadecimal digit expected"})
    end;
string_bq_chars([$u,${|Cs], Acc) ->             %Explicit utf-8 character
    string_bq_chars_utf8(Cs, 0, Acc);
string_bq_chars([$z|Cs], Acc) ->                %Skip whitespace
    string_chars(skip_space(Cs), Acc);
string_bq_chars([C|Cs], Acc) ->
    case escape_char(C) of
        error -> throw({string_error,"invalid escape sequence"});
        Esc -> string_chars(Cs, [Esc|Acc])
    end;
string_bq_chars([], Acc) ->
    Acc.

string_bq_chars_utf8([C|Cs], Uchar, Acc) when ?HEX(C) ->
    string_bq_chars_utf8(Cs, Uchar*16 + hex_val(C), Acc);
string_bq_chars_utf8([$}|Cs], Uchar, Acc) ->
    case unicode:characters_to_binary([Uchar]) of
        Bin when is_binary(Bin) ->
            string_chars(Cs, [Bin|Acc]);
        _Error ->
            throw({string_error,"UTF-8 value error"})
    end;
string_bq_chars_utf8(_Cs, _Uchar, _Acc) ->
    throw({string_error,"missing '}'"}).

skip_space([$\s|Cs]) -> skip_space(Cs);
skip_space(Cs) -> Cs.

%% long_string_token(InputChars, Length, BracketLength, Line) ->
%%     {token,{'LITERALSTRING',Line,Cs}} | {error,Error}.

long_string_token(Cs0, Len, BrLen, Line) ->
    %% Strip the brackets and remove first char if a newline.
    %% Note we "export" Cs1 here, (this is Erlang).
    case string:substr(Cs0, BrLen+1, Len - 2*BrLen) of
        [$\n | Cs1] -> Cs1;
        Cs1 -> Cs1
    end,
    %% io:format("lst1 ~w ~w\n", [length(Cs1),Cs1]),
    try
        Bytes = long_string_chars(Cs1, []),     %The bytes are encoded chars
        String = iolist_to_binary(Bytes),
        %% io:format("lst2 ~w ~w\n", [byte_size(String),String]),
        {token,{'LITERALSTRING',Line,String}}
    catch
        _:_ ->
            {error,"illegal long string"}
    end.

long_string_chars([C | Cs], Acc) ->
    long_string_chars(Cs, [C|Acc]);
%% long_string_chars([C | Cs], Acc) when ?ASCII(C) ->
%%     long_string_chars(Cs, [C|Acc]);
%% long_string_chars([C | Cs], Acc) ->             %This could be unicode
%%     case unicode:characters_to_binary([C]) of
%%         Bin when is_binary(Bin) ->
%%             long_string_chars(Cs, [Bin|Acc]);
%%         _Error ->
%%             throw(long_string_error)
%%     end;
long_string_chars([], Acc) ->
    lists:reverse(Acc).

hex_val(C) when C >= $0, C =< $9 -> C - $0;
hex_val(C) when C >= $a, C =< $f -> C - $a + 10;
hex_val(C) when C >= $A, C =< $F -> C - $A + 10.

escape_char($a) -> 7;                           %\a = BELL
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) -> $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char($\\) -> $\\;                        %\\ = BACKSLASH
escape_char($") -> $";                          %\" = STRING QUOTE
escape_char($') -> $';                          %\' = STRING QUOTE
escape_char($\n) -> $\n;                        %\LF = LF
escape_char($\r) -> $\n;                        %\RET = LF
escape_char(_C) -> error.                       %Illegal

%% is_keyword(Name) -> boolean().
%%  Test if the name is a keyword.

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
