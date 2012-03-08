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

-include("luerl.hrl").

-export([install/1]).

-export([test_gsub/3,test_do_match/3,test_pat/1]).	%Test functions

-import(luerl_lib, [lua_error/1]).		%Shorten this

-compile([bin_opt_info]).			%For when we are optimising

install(St0) ->
    {T,St1} = luerl_eval:alloc_table(table(), St0),
    {M,St2} = luerl_eval:alloc_table(metatable(T), St1),
    Meta0 = St2#luerl.meta,
    Meta1 = Meta0#meta{string=M},
    {T,St2#luerl{meta=Meta1}}.

metatable(T) ->					%String type metatable
    [{<<"__index">>,T}].

table() ->					%String table
    [{<<"byte">>,{function,fun byte/2}},
     {<<"char">>,{function,fun char/2}},
     {<<"find">>,{function,fun find/2}},
     {<<"format">>,{function, fun format/2}},
     {<<"gmatch">>,{function,fun gmatch/2}},
     {<<"gsub">>,{function,fun gsub/2}},
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

char(As, St) ->
    case catch list_to_binary(luerl_lib:tointegers(As)) of
	{'EXIT',_} -> lua_error({badarg,char,As});
	B -> {[B],St}
    end.

find([A1,A2], St) -> find([A1,A2,1.0], St);
find([A1,A2,A3], St) -> find([A1,A2,A3,nil], St);
find(As, St) ->
    case luerl_lib:conv_list(As, [lstring,lstring,integer,lbool]) of
	nil -> lua_error({badarg,find,As});
	[S,P,I,Pl] -> {find(S, byte_size(S), P, I, Pl),St}
    end.

%% find(String, Length, Pattern, Start, Plain) -> [Return].
%% Adjust the starting index and find the string.

find(_, L, _, I, _) when I > L -> [nil];
find(S, L, P, I, Pl) when I < -L -> find(S, L, P, 1, Pl);
find(S, L, P, I, Pl) when I < 0 -> find(S, L, P, L+I+1, Pl);
find(S, L, P, 0, Pl) ->  find(S, L, P, 1, Pl);
find(S, L, P, I, true) ->			%Plain text search string
    case binary:match(S, P, [{scope,{I-1,L-I+1}}]) of
	{Fs,Fl} -> [Fs+1,Fs+Fl];
	nomatch -> [nil]
    end;
find(S, L, P, I, false) ->			%Pattern search string
    case pat(binary_to_list(P)) of
	{ok,{Pat,_},_} ->
	    case find_loop(S, L, Pat, I) of
		[{_,F,Len}|Cas] ->
		    [float(F),float(F+Len-1)|match_cas(S, Cas)];
		[] -> [nil]
	    end;
	{error,E} -> lua_error(E)
    end.

find_loop(_, L, _, I) when I > L -> [];
find_loop(S0, L, Pat, I0) ->
    case do_match(S0, Pat, I0) of
	{match,Cas,_,_} -> Cas;
	nomatch ->
	    S1 = binary_part(S0, 1, L-I0),
	    find_loop(S1, L, Pat, I0+1)
    end.

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
collect_loop(<<$.,F/binary>>) -> collect_loop(F);
collect_loop(<<$-,F/binary>>) -> collect_loop(F);
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

gmatch(As, _) -> lua_error({badarg,gmatch,As}).

gsub(As, St) ->
    case luerl_lib:conv_list(As, [lstring,lstring,lany,integer]) of
	[S,P,R,N] when N > 0 ->
	    gsub(S, byte_size(S), P, R, N, St);
	[S,P,R] ->				%'all' bigger than any number
	    gsub(S, byte_size(S), P, R, all, St);
	_ -> lua_error({badarg,gsub,As})
    end.

test_gsub(S, P, N) ->
    {ok,{Pat,_},_} = pat(binary_to_list(P)),
    gsub_loop(S, byte_size(S), Pat, 1, 1, N).

gsub(S, L, P, R, N, St0) ->
    case pat(binary_to_list(P)) of
	{ok,{Pat,_},_} ->
	    Fs = gsub_loop(S, L, Pat, 1, 1, N),
	    {Ps,St1} = gsub_repl_loop(Fs, S, 1, L, R, St0),
	    {[iolist_to_binary(Ps),float(length(Fs))],St1};
	{error,E} -> lua_error(E)
    end.

%% gsub_loop(S, L, Pat, I, C, N) -> [Cas].
%%  Return the list of Cas's for each match.

gsub_loop(_, _, _, _, C, N) when C > N -> [];
gsub_loop(<<>>, _, _, _, _, _) -> [];
gsub_loop(S0, L, Pat, I0, C, N) ->
    case do_match(S0, Pat, I0) of
	{match,Cas,S1,I1} ->
	    [Cas|gsub_loop(S1, L, Pat, I1, C+1, N)];
	nomatch ->
	    S1 = binary_part(S0, 1, L-I0),
	    gsub_loop(S1, L, Pat, I0+1, C, N)
    end.

%% gsub_repl_loop([Cas], String, Index, Length, Reply, State) ->
%%     {iolist,State}.
%%  Build the return string as an iolist processing each match and
%%  filling in with the original string.

gsub_repl_loop([[{_,F,Len}|_]=Cas|Fs], S, I, L, R, St0) ->
    {Rep,St1} = gsub_repl(Cas, S, R, St0),
    {Ps,St2} = gsub_repl_loop(Fs, S, F+Len, L, R, St1),
    {[binary_part(S, I-1, F-I),Rep|Ps],St2};
gsub_repl_loop([], S, I, L, _, St) ->
    {[binary_part(S, I-1, L-I+1)],St}.

gsub_repl(Cas, S, {table,N}, #luerl{tabs=Ts}=St) ->
    Ca = case Cas of				%Get the right capture
	     [_Ca] -> _Ca;
	     [_,_Ca|_] -> _Ca
	 end,
    Key = match_ca(S, Ca),
    {Tab,_} = ?GET_TABLE(N, Ts),
    V = case orddict:find(Key, Tab) of
	    {ok,Val} -> Val;
	    error -> nil
	end,
    {[gsub_repl_val(S, V, Ca)],St};
gsub_repl(Cas0, S, Repl, St0) when element(1, Repl) =:= function ->
    Args = case Cas0 of
	       [Ca] -> [match_ca(S, Ca)];
	       [Ca|Cas] -> match_cas(S, Cas)
	   end,
    {Rs,St1} = luerl_eval:functioncall(Repl, Args, St0),
    {[gsub_repl_val(S, luerl_lib:first_value(Rs), Ca)],St1};
gsub_repl(Cas, S, Repl, St) ->
    case luerl_lib:to_list(Repl) of
	nil -> {[],St};
	R -> {gsub_repl_str(Cas, S, R),St}
    end.

gsub_repl_str(Cas, S, [$%,$%|R]) ->
    [$%|gsub_repl_str(Cas, S, R)];
gsub_repl_str(Cas, S, [$%,$0|R]) ->
    [match_ca(S, hd(Cas))|gsub_repl_str(Cas, S, R)];
gsub_repl_str(Cas, S, [$%,C|R]) when C >= $1, C =< $9 ->
    case lists:keysearch(C-$0, 1, Cas) of
	{value,Ca} -> [match_ca(S, Ca)|gsub_repl_str(Cas, S, R)];
	false -> lua_error({illegal_index,capture,C-$0})
    end;
gsub_repl_str(Cas, S, [C|R]) ->
    [C|gsub_repl_str(Cas, S, R)];
gsub_repl_str(_, _, []) -> [].

%% Return string or original match.

gsub_repl_val(S, Val, Ca) ->
    case luerl_lib:tostring(Val) of
	nil -> match_ca(S, Ca);			%Use original match
	Str -> Str
    end.

len([A|_], St) when is_binary(A) -> {[float(byte_size(A))],St};
len([A|_], St) when is_number(A) ->
    {[length(luerl_lib:number_to_list(A))],St};
len(As, _) -> lua_error({badarg,len,As}).

lower(As, St) ->
    case luerl_lib:conv_list(As, [list]) of
	[S] -> {[list_to_binary(string:to_lower(S))],St};
	nil -> lua_error({badarg,lower,As})
    end.

match([A1,A2], St) -> match([A1,A2,1.0], St);
match(As, St) ->
    case luerl_lib:conv_list(As, [lstring,lstring,integer]) of
	[S,P,I] -> {match(S, byte_size(S), P, I),St};
	_ -> lua_error({badarg,match,As})
    end.

%% match(String, Length, Pattern, Start) -> [Return].
%% Adjust the starting index and find the match.

match(_, L, _, I) when I > L -> [nil];		%Shuffle values
match(S, L, P, I) when I < -L -> match(S, L, P, 1);
match(S, L, P, I) when I < 0 -> match(S, L, P, L+I+1);
match(S, L, P, 0) -> match(S, L, P, 1);
match(S, L, P, I) ->
    case pat(binary_to_list(P)) of		%"Compile" the pattern
	{ok,{Pat,_},_} -> match_loop(S, L, Pat, I);
	{error,E} -> lua_error(E)
    end.

match_loop(_, L, _, I) when I > L -> [nil];	%No more to check
match_loop(S, L, Pat, I) ->
    Rest = binary_part(S, I-1, L-I+1),		%The bit we are interested in
    case do_match(Rest, Pat, I) of
	nomatch -> match_loop(S, L, Pat, I+1);
	{match,[{_,F,Len}],_,_} ->		%Only top level match
	    [binary_part(S, F-1, Len)];
	{match,[_|Cas],_,_} ->			%Have sub matches
	    match_cas(S, Cas)
    end.

match_ca(_, {_,F,Len}) when Len < 0 ->		%Capture position
    float(F);
match_ca(S, {_,F,Len}) ->			%Capture
    binary_part(S, F-1, Len).

match_cas(S, Cas) -> [ match_ca(S, Ca) || Ca <- Cas ].
    
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

%% This is the pattern grammar used. It may actually be overkill to
%% first parse the pattern as the pattern is relativey simple and we
%% should be able to do it in one pass.
%%
%% pat -> seq : '$1'.
%% seq -> single seq : ['$1'|'$2'].
%% seq -> single : '$1'.
%% single -> "(" seq ")" .
%% single -> "[" class "]" : {char_class,char_class('$2')}
%% single -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% single -> char "*" .
%% single -> char "+" .
%% single -> char "-" .
%% single -> char "?" .
%% single -> char .
%% char -> "%" class .
%% char -> "." .
%% char -> char .
%%  The actual parser is a recursive descent implementation of the
%%  grammar. We leave ^ $ as normal characters and handle them
%%  specially in matching.

pat(Cs0) ->
    case catch seq(Cs0, 0, 1, []) of
	{error,E} -> {error,E};
	{P,0,Sn} -> {ok,{P,0},Sn};
	{_,_,_} -> {error,invalid_capture}
    end.

test_pat(P) -> pat(P).

seq([_|_]=Cs, Sd, Sn, P) -> single(Cs, Sd, Sn, P);
seq([], Sd, Sn, P) -> {lists:reverse(P),Sd,Sn}.

single([$(|Cs], Sd, Sn, P) -> single(Cs, Sd+1, Sn+1, [{'(',Sn}|P]);
single([$)|_], 0, _, _) -> throw({error,invalid_capture});
single([$)|Cs], Sd, Sn, P) -> single(Cs, Sd-1, Sn, [')'|P]);
single([$[|Cs], Sd, Sn, P) -> char_class(Cs, Sd, Sn, P);
single([$.|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, ['.'|P]);
single([$%,C|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [char_class(C)|P]); 
single([C|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [C|P]);
single([], Sd, Sn, P) -> {lists:reverse(P),Sd,Sn}.

singlep([$*|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{kclosure,Char}|P]);
singlep([$+|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{pclosure,Char}|P]);
singlep([$-|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{mclosure,Char}|P]);
singlep([$?|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{optional,Char}|P]);
singlep(Cs, Sd, Sn, P) -> single(Cs, Sd, Sn, P).

char_class([$^|Cs], Sd, Sn, P) -> char_class(Cs, Sd, Sn, P, comp_class);
char_class(Cs, Sd, Sn, P) -> char_class(Cs, Sd, Sn, P, char_class).

char_class(Cs0, Sd, Sn, P, Tag) ->
    case char_class(Cs0, []) of
	{Cc,[$]|Cs1]} -> singlep(Cs1, Sd, Sn, [{Tag,Cc}|P]);
	_ -> throw({error,invalid_class})
    end.

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
char_class([C1,$-,C2|Cs], Cc) -> char_class(Cs, [{C1,C2}|Cc]);
char_class([C|Cs], Cc) when C /= $] -> char_class(Cs, [C|Cc]);
char_class(Cs, Cc) -> {Cc,Cs}.

test_do_match(S, P, I) ->
    {ok,{Pat,_},_} = pat(P),
    do_match(S, Pat, I).

%% do_match(String, Pattern, Index) -> {match,[Capture],Rest,Index} | nomatch.
%%  Try and match the pattern with the string *at the current
%%  position*. No searching.

do_match(S0, P0, I0) ->
    case do_match(P0, S0, I0, [{0,I0}], []) of
	{match,S1,I1,_,Cas} ->{match,Cas,S1,I1};
	{nomatch,_,_,_,_,_} -> nomatch
    end.

do_match([$$], <<>>, I, Ca, Cas) ->		%Match end of string
    {match,[],I,Ca,Cas};
do_match([$^|Ps], Cs, 1, Ca, Cas) ->		%Match beginning of string
    do_match(Ps, Cs, 1, Ca, Cas);
do_match([{'(',Sn},')'|P], Cs, I, Ca, Cas) ->
    do_match(P, Cs, I, Ca, save_cap(Sn, I, -1, Cas));
do_match([{'(',Sn}|P], Cs, I, Ca, Cas) ->
    do_match(P, Cs, I, [{Sn,I}|Ca], Cas);
do_match([')'|P], Cs, I, [{Sn,S}|Ca], Cas) ->
    do_match(P, Cs, I, Ca, save_cap(Sn, S, I-S, Cas));
do_match([{kclosure,P}=K|Ps], Cs, I, Ca, Cas) ->
    case do_match([P,K|Ps], Cs, I, Ca, Cas) of	%First try with it
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} ->			%Else try without it
	    do_match(Ps, Cs, I, Ca, Cas)
    end;
do_match([{pclosure,P}|Ps], Cs, I, Ca, Cas) ->	%The easy way
    do_match([P,{kclosure,P}|Ps], Cs, I, Ca, Cas);
do_match([{mclosure,P}=K|Ps], Cs, I, Ca, Cas) ->
    case do_match(Ps, Cs, I, Ca, Cas) of	%First try without it
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} ->			%Else try with it
	    do_match([P,K|Ps], Cs, I, Ca, Cas)
    end;
do_match([{optional,P}|Ps], Cs, I, Ca, Cas) ->
    case do_match([P|Ps], Cs, I, Ca, Cas) of	%First try with it
	{match,_,_,_,_}=M -> M;
	{nomatch,_,_,_,_,_} ->			%Else try without it
	    do_match(Ps, Cs, I, Ca, Cas)
    end;
do_match([{char_class,CC}|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) ->
    case match_char_class(CC, C) of
	true -> do_match(Ps, Cs, I+1, Ca, Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
do_match([{comp_class,CC}|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) ->
    case match_char_class(CC, C) of
	true -> {nomatch,Ps0,Cs0,I,Ca,Cas};
	false -> do_match(Ps, Cs, I+1, Ca, Cas)
    end;
do_match(['.'|Ps], <<_,Cs/binary>>, I, Ca, Cas) ->	%Matches anything
    do_match(Ps, Cs, I+1, Ca, Cas);
do_match([A|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) when is_atom(A) ->
    case match_class(A, C) of
	true -> do_match(Ps, Cs, I+1, Ca, Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
do_match([C|Ps], <<C,Cs/binary>>, I, Ca, Cas) ->
    do_match(Ps, Cs, I+1, Ca, Cas);
do_match([], Cs, I, [{Sn,S}|Ca], Cas) ->
    {match,Cs,I,Ca,[{Sn,S,I-S}|Cas]};
do_match(Ps, Cs, I, Ca, Cas) ->
    {nomatch,Ps,Cs,I,Ca,Cas}.

save_cap(N, F, L, [{N1,_,_}=Ca|Cas]) when N > N1 ->
    [Ca|save_cap(N, F, L, Cas)];
save_cap(N, F, L, Cas) -> [{N,F,L}|Cas].

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
