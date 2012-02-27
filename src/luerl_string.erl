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

-export([table/0,do_match/2]).

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

match(S, Len, P, I) when I > Len -> [nil];
match(S, Len, P, I) when I < -Len -> [nil];
match(S, Len, P, I) when I < 0 ->
    match(S, Len, P, Len+I+1);
match(S, Len, P, I) ->
    match_loop(lists:nthtail(I-1, S), P).

match_loop([], _) -> [nil];
match_loop(S, P) ->
    case do_match(S, P) of
	nomatch -> match_loop(tl(S), P);
	{match,[Ca],_} -> [list_to_binary(Ca)];
	{match,[_|Cas],_} ->
	    [ list_to_binary(Ca) || Ca <- Cas ]
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

%% do_match(String, Pattern) -> {match,[Capture],Rest} | nomatch.

do_match(S0, P0) ->
    case do_match(P0, S0, [], []) of
	{match,_,[$)|S],Ca,Cas} -> {match,[lists:reverse(Ca)|Cas],S};
	{match,_,S,Ca,Cas} -> {match,[lists:reverse(Ca)|Cas],S};
	{nomatch,_,_,_,_} -> nomatch
    end.

%% do_match(Pattern, String, Cap, [Cap]) ->
%%     {match,Pat,Str,Cap,[Cap]} | {nomatch,Pat,Str,Cap,[Cap]}.

do_match([$%,$a|P]=P0, [C|S]=S0, Ca, Cas) ->
    case is_a_char(C) of
	true -> do_match(P, S, [C|Ca], Cas);
	false -> {nomatch,P0,S0,Ca,Cas}
    end;
do_match([$%,$c|P]=P0, [C|S]=S0, Ca, Cas) ->
    case is_c_char(C) of
	true -> do_match(P, S, [C|Ca], Cas);
	false -> {nomatch,P0,S0,Ca,Cas}
    end;
do_match([$%,$d|P]=P0, [C|S]=S0, Ca, Cas) ->
    case is_d_char(C) of
	true -> do_match(P, S, [C|Ca], Cas);
	false -> {nomatch,P0,S0,Ca,Cas}
    end;
do_match([$%,$l|P]=P0, [C|S]=S0, Ca, Cas) ->
    case is_l_char(C) of
	true -> do_match(P, S, [C|Ca], Cas);
	false -> {nomatch,P0,S0,Ca,Cas}
    end;
do_match([$%,$u|P]=P0, [C|S]=S0, Ca, Cas) ->
    case is_u_char(C) of
	true -> do_match(P, S, [C|Ca], Cas);
	false -> {nomatch,P0,S0,Ca,Cas}
    end;
do_match([$%,$w|P]=P0, [C|S]=S0, Ca, Cas) ->
    case is_w_char(C) of
	true -> do_match(P, S, [C|Ca], Cas);
	false -> {nomatch,P0,S0,Ca,Cas}
    end;
do_match([$%,$x|P]=P0, [C|S]=S0, Ca, Cas) ->
    do_match_type(is_x_char(C), P0, P, S0, Ca, Cas);
do_match([$(|P], S, Ca, Cas) ->			%Enter new capture
    do_match_cap(P, S, Ca, Cas);
do_match([$)|_]=P, S, Ca, Cas) ->		%Exit this capture
    {match,P, S, Ca, Cas};
do_match([$.|P], [C|S], Ca, Cas) ->
    do_match(P, S, [C|Ca], Cas);
do_match([C|P], [C|S], Ca, Cas) ->
    do_match(P, S, [C|Ca], Cas);
do_match([$$], [], Ca, Cas) -> {match,[],[],Ca,Cas};
do_match([_|_]=P0, S, Ca, Cas) -> {nomatch,P0,S,Ca,Cas};
do_match([], S, [], Cas) -> {match,[],S,[],Cas};
do_match([], S, Ca, Cas) -> {match,[],S,Ca,Cas}.

do_match_type(true, _, P, [C|S], Ca, Cas) ->
    do_match(P, S, [C|Ca], Cas);
do_match_type(false, P0, _, S, Ca, Cas) ->
    {nomatch,P0,S,Ca,Cas}.

do_match_cap(P0, S0, Ca0, Cas0) ->
    case do_match(P0, S0, [], Cas0) of
	{match,[$)|P1],S1,Ca1,Cas1} ->		%Correctly terminated
	    Ca2 = lists:reverse(Ca1),
	    do_match(P1, S1, Ca1 ++ Ca0, [Ca2|Cas1]);
	{match,P1,S1,Ca1,Cas1} ->		%Missing ')', ignore
	    Ca2 = lists:reverse(Ca1),
	    do_match(P1, S1, Ca1 ++ Ca0, [Ca2|Cas1]);
	{nomatch,_P1,_S1,_Ca1,_Cas1}=NM -> NM
    end.

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
