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

%% File    : luerl_string.erl
%% Author  : Robert Virding
%% Purpose : The string library for Luerl.

-module(luerl_string).

-export([table/0,do_match/3,pat/1]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

table() ->
    [{<<"byte">>,{function,fun byte/2}},
     {<<"char">>,{function,fun char/2}},
     {<<"format">>,{function, fun format/2}},
     {<<"len">>,{function,fun len/2}},
     {<<"lower">>,{function,fun lower/2}},
     {<<"match">>,{function,fun match/2}},
     {<<"rep">>,{function,fun rep/2}},
     {<<"reverse">>,{function,fun reverse/2}},
     {<<"sub">>,{function,fun sub/2}},
     {<<"upper">>,{function,fun upper/2}}
    ].

byte([A], St) -> byte(A, 1, 1, St);
byte([A1,A2], St) -> byte(A1, A2, A2, St);
byte([A1,A2,A3|_], St) -> byte(A1, A2, A3, St);
byte(As, _) -> lua_error({badarg,byte,As}).

byte(A1, A2, A3, St) when is_binary(A1), is_number(A2), is_number(A3) ->
    F = round(A2),				%First and last positions
    L = round(A3),
    if F >= 1, L >= F, L =< byte_size(A1) ->
	    {binary_to_list(A1, F, L),St};
       true -> {[],St}
    end;
byte(_, _, _, St) -> {[],St}.

char(Cs, St) -> {list_to_binary(Cs),St}.

%% format([Format,Arg|_], State) -> {String,State}.
% A VERY primitive format function, barely works.

format([F|As], St) ->
    L = format_loop(F, As),
    {[iolist_to_binary(L)],St};
format(As, _) -> lua_error({badarg,format,As}).

format_loop(<<$%,F0/binary>>, As0) ->
    {Fo,F1} = collect(F0),
    {Out,As1} = build(Fo, As0),
    [Out|format_loop(F1, As1)];
format_loop(<<$\\,C,Rest/binary>>, As) ->
    [C|format_loop(Rest, As)];
format_loop(<<C,Rest/binary>>, As) ->
    [C|format_loop(Rest, As)];
format_loop(<<>>, _) -> [].

%% collect(Format) -> {{C,F,Ad,P},Format}.

collect(F0) ->
    {C,F1} = collect_loop(F0),
    {{C,0,0,0},F1}.

collect_loop(<<D,F/binary>>) when D >= $0, D =< $9 ->
    collect_loop(F);
collect_loop(<<$.,F/binary>>) ->
    collect_loop(F);
collect_loop(<<C,F/binary>>) -> {C,F}.

%% build({C,F,Ad,P}, Args) -> {Out,Args}.

build({$s,_,_,_}, [A|As]) ->
    S = luerl_lib:tostring(A),
    {io_lib:fwrite("~s", [S]),As};
build({$q,_,_,_}, [A|As]) ->
    S = luerl_lib:tostring(A),
    {io_lib:fwrite("\"~s\"", [S]),As};
build({$d,_,_,_}, [A|As]) ->
    I = luerl_lib:tointeger(A),
    {io_lib:write(I),As};
build({$e,_,_,_}, [A|As]) ->
    F = luerl_lib:tonumber(A),
    {io_lib:format("~e", [F]),As};
build({$f,_,_,_}, [A|As]) ->
    F = luerl_lib:tonumber(A),
    {io_lib:format("~f", [F]),As};
build({$g,_,_,_}, [A|As]) ->
    F = luerl_lib:tonumber(A),
    {io_lib:format("~g", [F]),As}.

len([A|_], St) when is_binary(A) -> {byte_size(A),St};
len([A|_], St) when is_number(A) ->
    {[length(luerl_lib:number_to_list(A))],St};
len(As, _) -> lua_error({badarg,len,As}).

lower([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(string:to_lower(S))],St};
lower(As, _) -> lua_error({badarg,lower,As}).

match([A1,A2], St) -> match([A1,A2,1.0], St);
match(As, St) ->
    case luerl_lib:conv_list(As, [string,string,integer]) of
	[S,P,I] -> {match(S, length(S), P, I),St};
	_ -> lua_error({badarg,match,As})
    end.

match(_, L, _, I) when I > L -> [nil];		%Shuffle values
match(_, L, _, I) when I < -L -> [nil];
match(S, L, P, I) when I < 0 ->
    match(S, L, P, L+I+1);
match(S, _, P, I) ->
    {ok,{Pat,_},[]} = pat(P),			%"Compile" the pattern
    match_loop(lists:nthtail(I-1, S), Pat, I).

match_loop([], _, _) -> [nil];
match_loop(S, Pat, I) ->
    case do_match(S, Pat, I) of
	nomatch -> match_loop(tl(S), Pat, I+1);
	{match,[Ca],_} -> [list_to_binary(Ca)];	%Only top level match
	{match,[_|Cas],_} ->			%Have sub matches
	    [ if is_integer(Ca) -> float(Ca);	%String position
		 true -> list_to_binary(Ca) end
	      || Ca <- Cas ]
    end.

rep([A1,A2], St) -> rep([A1,A2,<<>>], St);
rep([_,_,_|_]=As, St) ->
    case luerl_lib:conv_list(As, [list,integer,list]) of
	[S,I,Sep] ->
	    if I > 0 ->
		    {[iolist_to_binary([S|lists:duplicate(I-1, [Sep,S])])],St};
	       true -> {[<<>>],St}
	    end;
	nil ->					%Error or bad values
	    lua_error({badarg,rep,As})
    end;
rep(As, _) -> lua_error({badarg,rep,As}).

reverse([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(lists:reverse(S))],St};
reverse(As, _) -> lua_error({badarg,reverse,As}).

sub([A1|As], St) ->
    case luerl_lib:conv_list([A1|As], [lstring,integer,integer]) of
	[S|Is] ->
	    Len = byte_size(S),
	    Sub = case Is of
		      [I] -> do_sub(S, Len, I);		%Just an I
		      [I,J] -> do_sub(S, Len, I, J)	%Both an I and a J
		  end,
	    {[Sub],St}
    end;
sub(As, _) -> lua_error({badarg,sub,As}).

do_sub(S, _, 0) -> S;
do_sub(S, Len, I) when I < 1 -> do_sub(S, Len, Len+I+1, Len);
do_sub(S, Len, I) -> do_sub(S, Len, I, Len).

do_sub(S, Len, I, J) when I < 1 -> do_sub(S, Len, 1, J);
do_sub(_, Len, _, J) when J < -Len -> <<>>;
do_sub(S, Len, I, J) when J < 0 -> do_sub(S, Len, I, Len+J+1);
do_sub(S, Len, I, J) when J > Len -> do_sub(S, Len, I, Len);
do_sub(_, Len, I, _) when I > Len -> <<>>;
do_sub(_, _, I, J) when J < I -> <<>>;
do_sub(S, _, I, J) -> binary:part(S, I-1, J-I+1). %Zero-based, yuch!

upper([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(string:to_upper(S))],St};
upper(As, _) -> lua_error({badarg,upper,As}).

%% Patterns.
%% %a
%% %d
%% *
%% ^$
%% ()
%% []

%% This is the pattern grammar used.
%%
%% pat -> "^" pat1 "$" : '$1'.
%% pat1 -> pat1 pat2 : {concat,'$1','$2'}.
%% pat1 -> pat2 : '$1'.
%% pat2 -> pat2 "*" : {kclosure,'$1'}.
%% pat2 -> pat2 "+" : {pclosure,'$1'}.
%% pat2 -> pat2 "?" : {optional,'$1'}.
%% pat2 -> pat3 : '$1'.
%% pat3 -> "(" pat ")" : '$2'.
%% pat3 -> "%" char : '$2'.
%% pat3 -> "." : char.
%% pat3 -> "[" class "]" : {char_class,char_class('$2')}
%% pat3 -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% pat3 -> char : '$1'.
%% pat3 -> empty : epsilon.
%%  The actual parser is a recursive descent implementation of the grammar.

pat(Cs0) ->
    case catch pat1(Cs0, 0) of
	{P,Cs1,Sc} -> {ok,{P,Sc},Cs1};
	{error,E} -> {error,E}
    end.

%% pat1(Chars, SubNumber) -> {Pattern,SubNumber,Chars}.
%% Allow the empty sequence.

pat1(Cs0, Sn0) -> pat1p(Cs0, Sn0).

pat1p([C|_]=Cs0, Sn0) when C /= $) ->
    {P,Cs1,Sn1} = pat2(Cs0, Sn0),
    {Ps,Cs2,Sn2} = pat1p(Cs1, Sn1),
    {[P|Ps],Cs2,Sn2};
pat1p(Cs, Sn) -> {[],Cs,Sn}.

pat2(Cs0, Sn0) ->
    {L,Cs1,Sn1} = pat3(Cs0, Sn0),
    pat2p(Cs1, Sn1, L).

pat2p([$*|Cs], Sn, L) -> pat2p(Cs, Sn, {kclosure,L});
pat2p([$+|Cs], Sn, L) -> pat2p(Cs, Sn, {pclosure,L});
pat2p([$-|Cs], Sn, L) -> pat2p(Cs, Sn, {mclosure,L});
pat2p([$?|Cs], Sn, L) -> pat2p(Cs, Sn, {optional,L});
pat2p(Cs, Sn, L) -> {L,Cs,Sn}.

pat3([$(|Cs0], Sn0) ->
    Sn1 = Sn0 + 1,
    case pat1(Cs0, Sn1) of
	{P,[$)|Cs1],Sn2} -> {{sub,Sn1,P},Cs1,Sn2};
	X -> throw({error,{unterminated,"(",X}})
    end;
pat3([$[,$^|Cs0], Sn) ->
    case char_class(Cs0, []) of
	{Cc,[$]|Cs1]} -> {{comp_class,Cc},Cs1,Sn};
	_ -> throw({error,{unterminated,"["}})
    end;
pat3([$[|Cs0], Sn) ->
    case char_class(Cs0, []) of
	{Cc,[$]|Cs1]} -> {{char_class,Cc},Cs1,Sn};
	_ -> throw({error,{unterminated,"["}})
    end;
pat3([$%,C|Cs], Sn) -> {char_class(C),Cs,Sn};
pat3([$.|Cs], Sn) -> {'.',Cs,Sn};
pat3([C|Cs], Sn) when C /= $*, C /= $+, C /= $?, C /= $-, C /= $] -> {C,Cs,Sn};
pat3([], Sn) -> {epsilon,[],Sn}.

char_class($a) -> 'a';
char_class($A) -> 'A';
char_class($c) -> 'c';
char_class($C) -> 'C';
char_class($d) -> 'd';
char_class($D) -> 'D';
char_class($l) -> 'l';
char_class($L) -> 'L';
char_class($u) -> 'u';
char_class($U) -> 'U';
char_class($w) -> 'w';
char_class($W) -> 'W';
char_class($x) -> 'x';
char_class($X) -> 'X';
char_class(C) -> C.

char_class([$%,C|Cs], Cc) -> char_class(Cs, [char_class(C)|Cc]);
char_class([C1,$-,C2|Cs], Cc) ->
    if C1 < C2 -> char_class(Cs, [{C1,C2}|Cc]);
       true -> throw({error,char_class})
    end;
char_class([C|Cs], Cc) when C /= $] -> char_class(Cs, [C|Cc]);
char_class(Cs, Cc) -> {Cc,Cs}.

%% do_match(String, Pattern, Index) -> {match,[Capture],Rest} | nomatch.

do_match(S0, P0, I0) ->
    case do_match(P0, S0, I0, [], []) of
	{match,S1,_,Ca,Cas} ->			%Ca top level match
	    {match,[lists:reverse(Ca)|Cas],S1};
	{nomatch,_,_,_,_,_} -> nomatch
    end.

do_match([$$], [], I, Ca, Cas) -> {match,[],I,Ca,Cas};
do_match([$^|Ps], Cs, 1, Ca, Cas) ->
    do_match(Ps, Cs, 2, Ca, Cas);
%%do_match([{kclosure,P}|Ps]
do_match([{pclosure,P}|Ps], Cs, I, Ca, Cas) ->
    do_match([P,{kclosure,P}|Ps], Cs, I, Ca, Cas);
do_match([{optional,P}|Ps], Cs, I, Ca, Cas) ->
    case do_match([P|Ps], Cs, I, Ca, Cas) of
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} -> do_match(Ps, Cs, I, Ca, Cas)
    end;
do_match([{sub,_,[]}|Ps], Cs, I, Ca, Cas) ->
    do_match(Ps, Cs, I, Ca, Cas ++ [I]);
do_match([{sub,_,P1}|Ps], Cs0, I0, Ca0, Cas0) ->
    case do_match(P1, Cs0, I0, [], []) of
	{match,Cs1,I1,Ca1,Cas1} ->
	    Ca2 = lists:reverse(Ca1),
	    do_match(Ps, Cs1, I1, Ca1 ++ Ca0, Cas0 ++ [Ca2] ++ Cas1);
	{nomatch,_,_,_,_,_}=NM -> NM
    end;
do_match([{char_class,CC}|Ps]=Ps0, [C|Cs]=Cs0, I, Ca, Cas) ->
    case match_char_class(CC, C) of
	true -> do_match(Ps, Cs, I+1, [C|Ca], Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
do_match([{comp_class,CC}|Ps]=Ps0, [C|Cs]=Cs0, I, Ca, Cas) ->
    case match_char_class(CC, C) of
	true -> {nomatch,Ps0,Cs0,I,Ca,Cas};
	false -> do_match(Ps, Cs, I+1, [C|Ca], Cas)
    end;
do_match(['.'|Ps], [C|Cs], I, Ca, Cas) ->
    do_match(Ps, Cs, I+1, [C|Ca], Cas);
do_match([A|Ps]=Ps0, [C|Cs]=Cs0, I, Ca, Cas) when is_atom(A) ->
    case match_class(A, C) of
	true -> do_match(Ps, Cs, I+1, [C|Ca], Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
do_match([C|Ps], [C|Cs], I, Ca, Cas) ->
    do_match(Ps, Cs, I+1, [C|Ca], Cas);
do_match([], Cs, I, Ca, Cas) ->
    {match,Cs,I,Ca,Cas};
do_match(Ps, Cs, I, Ca, Cas) ->
    {nomatch,Ps,Cs,I,Ca,Cas}.

match_class('a', C) -> is_a_char(C);
match_class('A', C) -> not is_a_char(C);
match_class('c', C) -> is_c_char(C);
match_class('C', C) -> not is_c_char(C);
match_class('d', C) -> is_d_char(C);
match_class('D', C) -> not is_d_char(C);
match_class('l', C) -> is_l_char(C);
match_class('L', C) -> not is_l_char(C);
match_class('u', C) -> is_u_char(C);
match_class('U', C) -> not is_u_char(C);
match_class('w', C) -> is_w_char(C);
match_class('W', C) -> not is_w_char(C);
match_class('x', C) -> is_x_char(C);
match_class('X', C) -> not is_x_char(C).

match_char_class([{C1,C2}|_], C) when C >= C1, C=< C2 -> true;
match_char_class([A|P], C) when is_atom(A) ->
    case match_class(A, C) of
	true -> true;
	false -> match_char_class(P, C)
    end;
match_char_class([C|_], C) -> true;
match_char_class([_|Cc], C) -> match_char_class(Cc, C); 
match_char_class([], _) -> false.

%% Test for various character types.

is_a_char($_) -> true;				%All letters
is_a_char(C) ->
    is_l_char(C) orelse is_u_char(C).

is_c_char(C) when C >= 0, C =< 31 -> true;	%All control characters
is_c_char(C) when C >= 128, C =< 159 -> true;
is_c_char(_) -> false.

is_d_char(C) -> (C >= $0) and (C =< $9).	%All digits

is_l_char(C) when C >= $a, C =< $z -> true;	%All lowercase letters
is_l_char(C) when C >= 224, C =< 246 -> true;
is_l_char(C) when C >= 248, C =< 255 -> true;
is_l_char(_) -> false.

is_u_char(C) when C >= $A, C =< $Z -> true;	%All uppercase letters
is_u_char(C) when C >= 192, C =< 214 -> true;
is_u_char(C) when C >= 216, C =< 223 -> true;
is_u_char(_) -> false.

is_w_char(C) ->					%All alphanumeric characters
    is_a_char(C) orelse is_d_char(C).

is_x_char(C) when C >= $a, C =< $f -> true;	%All hexadecimal characters
is_x_char(C) when C >= $A, C =< $F -> true;
is_x_char(C) -> is_d_char(C).

%% match_class('a', C) -> (char_table(C) band ?_A) =/= 0;
%% match_class('A', C) ->  (char_table(C) band ?_A) =:= 0.

%% char_table(C) when C >= 0, C =< 31 -> ?_C;
%% char_table(C) when C >= 65, C =< 91 -> ?_U bor ?_A;
%% char_table(C) when C >= 97, C =< 123 -> ?_L;
