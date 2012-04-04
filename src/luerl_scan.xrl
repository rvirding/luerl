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

%% File    : luerl_scan.xrl
%% Author  : Robert Virding
%% Purpose : Token definitions for LUA.

Definitions.

D = [0-9]
H = [0-9A-Za-z]
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
	    {ok,I} -> {token,{'NUMBER',TokenLine,float(I)}};
	    _ -> {error,"illegal number"}
	end.
0x{H}+ :
	base_token(string:substr(TokenChars, 3), 16, TokenLine).
%% Floats.
{D}+\.{D}+([eE][-+]?{D}+)? :
	case catch {ok,list_to_float(TokenChars)} of
	    {ok,F} -> {token,{'NUMBER',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.
{D}+[eE][-+]?{D}+ :
	[M,E] = string:tokens(TokenChars, "eE"),
	case catch {ok,list_to_float(M ++ ".0e" ++ E)} of
	    {ok,F} -> {token,{'NUMBER',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.
\.{D}+[eE][-+]?{D}+ :
	case catch {ok,list_to_float("0" ++ TokenChars)} of
	    {ok,F} -> {token,{'NUMBER',TokenLine,F}};
	    _ -> {error,"illegal number"}
	end.

%% Strings.
\"(\\.|[^"\n])*\" :
	%% Strip quotes.
	Cs = string:substr(TokenChars, 2, TokenLen - 2),
	S = list_to_binary(chars(Cs)),
	{token,{'STRING',TokenLine,S}}.
\'(\\.|[^'\n])*\' :
	%% Strip quotes.
	Cs = string:substr(TokenChars, 2, TokenLen - 2),
	S = list_to_binary(chars(Cs)),
	{token,{'STRING',TokenLine,S}}.
\[\[([^]]|\][^]])*\]+\] :
	%% Strip quotes.
	Cs = string:substr(TokenChars, 3, TokenLen - 4),
 	long_bracket(TokenLine, Cs).

%% Other known tokens.
\+ : {token,{'+',TokenLine}}.
\- : {token,{'-',TokenLine}}.
\* : {token,{'*',TokenLine}}.
\/ : {token,{'/',TokenLine}}.
\% : {token,{'%',TokenLine}}.
\^ : {token,{'^',TokenLine}}.
\# : {token,{'#',TokenLine}}.
== : {token,{'==',TokenLine}}.
~= : {token,{'~=',TokenLine}}.
<= : {token,{'<=',TokenLine}}.
>= : {token,{'>=',TokenLine}}.
< :  {token,{'<',TokenLine}}.
> :  {token,{'>',TokenLine}}.
= :  {token,{'=',TokenLine}}.
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

[\000-\s]+ : skip_token.

%% Comments.
--(\n|[^[].*) : skip_token.
%% --aa([^b]|b[^b])*b+b
--\[\[([^]]|\][^]])*\]+\] : skip_token.

Erlang code.

-export([is_keyword/1]).

%% name_token(Chars, Line) ->
%%     {token,{'NAME',Line,Symbol}} | {Name,Line} | {error,E}.
%% Build a name from list of legal characters, else error.

name_token(Cs, L) ->
    case catch {ok,list_to_atom(Cs)} of
	{ok,Name} ->
	    case is_keyword(Name) of
		true -> {token,{Name,L}};
		false -> {token,{'NAME',L,Name}}
	    end;
	_ -> {error,"illegal name"}
    end.

%% base_token(Chars, Base, Line) -> Integer.
%% Convert a string of Base characters into a number. We know that
%% the strings only contain the correct character.

base_token(Cs, B, L) ->
    case base1(Cs, B, 0) of
	{I,[]} -> {token,{'NUMBER',L,float(I)}};
	{_,_} -> {error,"illegal based number"}
    end.

base1([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base1(Cs, Base, Next);
base1([C|Cs], _Base, SoFar) -> {SoFar,[C|Cs]};
base1([], _Base, N) -> {N,[]}.

%% chars(InputChars) -> Chars.
%% Convert an input string into the corresponding string
%% characters. We know that the input string is correct.

chars([$\\,$x,C|Cs0]) ->
    case hex_char(C) of
	true ->
	    case base1([C|Cs0], 16, 0) of
		{N,[$;|Cs1]} -> [N|chars(Cs1)];
		_Other -> [escape_char($x)|chars([C|Cs0])]
	    end;
	false -> [escape_char($x)|chars([C|Cs0])]
    end;
chars([$\\,C|Cs]) -> [escape_char(C)|chars(Cs)];
chars([C|Cs]) -> [C|chars(Cs)];
chars([]) -> [].

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

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
    S = list_to_binary(chars(Cs)),
     {token,{'STRING',Line,S}};
long_bracket(Line, Cs) ->
    S = list_to_binary(chars(Cs)),
     {token,{'STRING',Line,S}}.

%% is_keyword(Name) -> boolean().
%% Test if the name is a keyword.

is_keyword('and') -> true;
is_keyword('break') -> true;
is_keyword('do') -> true;
is_keyword('else') -> true;
is_keyword('elseif') -> true;
is_keyword('end') -> true;
is_keyword('false') -> true;
is_keyword('for') -> true;
is_keyword('function') -> true;
is_keyword('goto') -> true;
is_keyword('if') -> true;
is_keyword('in') -> true;
is_keyword('local') -> true;
is_keyword('nil') -> true;
is_keyword('not') -> true;
is_keyword('or') -> true;
is_keyword('repeat') -> true;
is_keyword('return') -> true;
is_keyword('then') -> true;
is_keyword('true') -> true;
is_keyword('until') -> true;
is_keyword('while') -> true;
is_keyword(_) -> false.
