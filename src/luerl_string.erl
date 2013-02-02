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

-export([test_gsub/3,test_do_match/3,test_pat/1,	%Test functions
	 test_byte/3,test_find/4,test_sub/2,test_sub/3]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

%%-compile([bin_opt_info]).			%For when we are optimising

install(St0) ->
    {T,St1} = luerl_emul:alloc_table(table(), St0),
    {M,St2} = luerl_emul:alloc_table(metatable(T), St1),
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

byte(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,integer,integer]) of
	[S|Is] ->
	    Bs = do_byte(S, byte_size(S), Is),
	    {Bs,St};
	_ -> lua_error({badarg,byte,As})	%nil or []
    end.

test_byte(S, I, J) ->
    do_byte(S, byte_size(S), I, J).

do_byte(_, 0, _) -> [nil];
do_byte(S, Len, []) -> do_byte(S, Len, 1, 1);
do_byte(S, Len, [I]) -> do_byte(S, Len, I, I);
do_byte(S, Len, [I,J]) -> do_byte(S, Len, I, J).

do_byte(S, Len, I0, J0) ->			%The same as for sub
    I1 = do_sub_m(Len, I0),
    J1 = do_sub_m(Len, J0),
    do_byte_ij(S, Len, I1, J1).

do_byte_ij(S, Len, I, J) when I < 1 -> do_byte_ij(S, Len, 1, J);
do_byte_ij(S, Len, I, J) when J > Len -> do_byte_ij(S, Len, I, Len);
do_byte_ij(_, _, I, J) when I > J -> [nil];
do_byte_ij(S, _, I, J) ->
    [ float(N) || N <- binary_to_list(S, I, J) ].

char([nil], St) -> {[<<>>],St};
char(As, St) ->
    case catch list_to_binary(luerl_lib:to_ints(As)) of
	{'EXIT',_} -> lua_error({badarg,char,As});
	B -> {[B],St}
    end.

find([A1,A2], St) -> find([A1,A2,1.0], St);
find([A1,A2,A3], St) -> find([A1,A2,A3,nil], St);
find(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,lua_string,integer,lua_bool]) of
	[S,P,I,Pl] -> {find(S, byte_size(S), P, I, Pl),St};
	_ -> lua_error({badarg,find,As})	%nil, [_] or []
    end.

test_find(S, P, I, Pl) -> find(S, byte_size(S), P, I, Pl).

%% find(String, Length, Pattern, Start, Plain) -> [Return].
%% Adjust the starting index and find the string.

find(_, L, _, I, _) when I > L+1 -> [nil];
find(S, L, P, I, Pl) when I < -L -> find(S, L, P, 1, Pl);
find(S, L, P, I, Pl) when I < 0 -> find(S, L, P, L+I+1, Pl);
find(S, L, P, 0, Pl) ->  find(S, L, P, 1, Pl);
find(S, L, P, I, true) ->			%Plain text search string
    case binary:match(S, P, [{scope,{I-1,L-I+1}}]) of
	{Fs,Fl} -> [float(Fs+1),float(Fs+Fl)];
	nomatch -> [nil]
    end;
find(S, L, P, I, false) ->			%Pattern search string
    case pat(binary_to_list(P)) of
	{ok,{Pat,_},_} ->
	    S1 = binary_part(S, I-1, L-I+1),	%Start searching from I
	    case match_loop(S1, L, Pat, I) of
		[{_,F,Len}|Cas] ->		%Matches
		    [float(F),float(F+Len-1)|match_cas(Cas, S)];
		[] -> [nil]			%No match
	    end;
	{error,E} -> lua_error(E)
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
build({$i,_,_,_}, [A|As]) ->
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
    case luerl_lib:conv_list(As, [lua_string,lua_string,lua_any,integer]) of
	[S,P,R,N] when N > 0 ->
	    gsub(S, byte_size(S), P, R, N, St);
	[S,P,R] ->				%'all' bigger than any number
	    gsub(S, byte_size(S), P, R, all, St);
	_ -> lua_error({badarg,gsub,As})
    end.

test_gsub(S, P, N) ->
    {ok,{Pat,_},_} = pat(binary_to_list(P)),
    gsub_match_loop(S, byte_size(S), Pat, 1, 1, N).

gsub(S, L, P, R, N, St0) ->
    case pat(binary_to_list(P)) of
	{ok,{Pat,_},_} ->
	    %% io:fwrite("g: ~p\n", [{S,L,Pat,R,N}]),
	    Fs = gsub_match_loop(S, L, Pat, 1, 1, N),
	    %% io:fwrite("g->~p\n", [Fs]),
	    {Ps,St1} = gsub_repl_loop(Fs, S, 1, L, R, St0),
	    {[iolist_to_binary(Ps),float(length(Fs))],St1};
	{error,E} -> lua_error(E)
    end.

%% gsub_match_loop(S, L, Pat, I, C, N) -> [Cas].
%%  Return the list of Cas's for each match.

gsub_match_loop(_, _, _, _, C, N) when C > N -> [];
gsub_match_loop(<<>>, _, Pat, I, _, _) ->	%It can still match at end!
    case do_match(<<>>, Pat, I) of
	{match,Cas,_,_} -> [Cas];
	nomatch -> []
    end;
gsub_match_loop(S0, L, Pat, I0, C, N) ->
    case do_match(S0, Pat, I0) of
	{match,Cas,_,I0} ->			%Zero length match
	    S1 = binary_part(S0, 1, L-I0),
	    [Cas|gsub_match_loop(S1, L, Pat, I0+1, C+1, N)];
	{match,Cas,S1,I1} ->
	    [Cas|gsub_match_loop(S1, L, Pat, I1, C+1, N)];
	nomatch ->
	    S1 = binary_part(S0, 1, L-I0),
	    gsub_match_loop(S1, L, Pat, I0+1, C, N)
    end.

%% gsub_repl_loop([Cas], String, Index, Length, Reply, State) ->
%%     {iolist,State}.
%%  Build the return string as an iolist processing each match and
%%  filling in with the original string.

gsub_repl_loop([[{_,F,Len}|_]=Cas|Fs], S, I, L, R, St0) ->
    %% io:fwrite("grl: ~p\n", [{Cas,S,R}]),
    {Rep,St1} = gsub_repl(Cas, S, R, St0),
    %% io:fwrite("grl->~p\n", [{Rep}]),
    {Ps,St2} = gsub_repl_loop(Fs, S, F+Len, L, R, St1),
    {[binary_part(S, I-1, F-I),Rep|Ps],St2};
gsub_repl_loop([], S, I, L, _, St) ->
    {[binary_part(S, I-1, L-I+1)],St}.

gsub_repl(Cas, S, #tref{}=T, St0) ->
    case Cas of					%Export both Ca and Key
	[Ca] -> Key = match_ca(Ca, S);
	[Ca,Ca1|_] -> Key = match_ca(Ca1, S)
    end,
    {R,St1} = luerl_emul:get_table_key(T, Key, St0),
    {[gsub_repl_val(S, R, Ca)],St1};
gsub_repl(Cas0, S, Repl, St0) when element(1, Repl) =:= function ->
    case Cas0 of				%Export both Ca and Args
	[Ca] -> Args = [match_ca(Ca, S)];
	[Ca|Cas] -> Args = match_cas(Cas, S)
    end,
    {Rs,St1} = luerl_emul:functioncall(Repl, Args, St0),
    {[gsub_repl_val(S, luerl_lib:first_value(Rs), Ca)],St1};
gsub_repl(Cas, S, Repl, St) ->			%Replace string
    case luerl_lib:to_list(Repl) of
	nil -> {[],St};
	R -> {gsub_repl_str(Cas, S, R),St}
    end.

gsub_repl_str(Cas, S, [$%,$%|R]) ->
    [$%|gsub_repl_str(Cas, S, R)];
gsub_repl_str(Cas, S, [$%,$0|R]) ->
    Cstr = luerl_lib:tostring(match_ca(hd(Cas), S)), %Force to string
    [Cstr|gsub_repl_str(Cas, S, R)];
gsub_repl_str(Cas, S, [$%,C|R]) when C >= $1, C =< $9 ->
    case lists:keysearch(C-$0, 1, Cas) of
	{value,Ca} ->
	    Cstr = luerl_lib:tostring(match_ca(Ca, S)),	%Force to string!
	    [Cstr|gsub_repl_str(Cas, S, R)];
	false -> lua_error({illegal_index,capture,C-$0})
    end;
gsub_repl_str(Cas, S, [C|R]) ->
    [C|gsub_repl_str(Cas, S, R)];
gsub_repl_str(_, _, []) -> [].

%% Return string or original match.

gsub_repl_val(S, Val, Ca) ->
    case luerl_lib:tostring(Val) of
	nil -> match_ca(Ca, S);			%Use original match
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

%% match(Args, State) -> {[Match],State}.

match([A1,A2], St) -> match([A1,A2,1.0], St);
match(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,lua_string,integer]) of
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
	{ok,{Pat,_},_} ->
	    S1 = binary_part(S, I-1, L-I+1),	%Start searching from I
	    case match_loop(S1, L, Pat, I) of
		[{_,F,Len}] ->			%Only top level match
		    [binary_part(S, F-1, Len)];
		[_|Cas] ->			%Have sub matches
		    match_cas(Cas, S);
		[] -> [nil]			%No match
	    end;
	{error,E} -> lua_error(E)
    end.

%% match_loop(String, Length, Pattern, Index) -> Cas | [].
%% Step down the string trying to find a match.

match_loop(S, L, Pat, I) when I > L ->		%It can still match at end!
    case do_match(S, Pat, I) of
	{match,Cas,_,_} -> Cas;
	nomatch -> []				%Now we haven't found it
    end;
match_loop(S0, L, Pat, I) ->
    case do_match(S0, Pat, I) of
	{match,Cas,_,_} -> Cas;
	nomatch ->
	    S1 = binary_part(S0, 1, L-I),
	    match_loop(S1, L, Pat, I+1)
    end.

match_ca({_,F,Len}, _) when Len < 0 ->		%Capture position
    float(F);
match_ca({_,F,Len}, S) ->			%Capture
    binary_part(S, F-1, Len).

match_cas(Cas, S) -> [ match_ca(Ca, S) || Ca <- Cas ].

rep([A1,A2], St) -> rep([A1,A2,<<>>], St);
rep([_,_,_|_]=As, St) ->
    case luerl_lib:conv_list(As, [lua_string,integer,lua_string]) of
	[S,I,Sep] ->
	    if I > 0 ->
		    {[iolist_to_binary([S|lists:duplicate(I-1, [Sep,S])])],St};
	       true -> {[<<>>],St}
	    end;
	nil ->					%Error or bad values
	    lua_error({badarg,rep,As})
    end;
rep(As, _) -> lua_error({badarg,rep,As}).

%% reverse(Args, State) -> {[Res],St}.

reverse([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(lists:reverse(S))],St};
reverse(As, _) -> lua_error({badarg,reverse,As}).

%% sub(Args, State) -> {[Res],State}.

sub(As, St) ->
    case luerl_lib:conv_list(As, [lua_string,integer,integer]) of
	[S,I|Js] ->
	    Len = byte_size(S),
	    Sub = do_sub(S, Len, I, Js),	%Just I, or both I and J
	    {[Sub],St};
	_ -> lua_error({badarg,sub,As})		%nil, [_] or []
    end.

test_sub(S, I) -> do_sub(S, byte_size(S), I, []).
test_sub(S, I, J) -> do_sub(S, byte_size(S), I, [J]).

do_sub(S, _, 0, []) -> S;			%Special case this
do_sub(S, Len, I, []) -> do_sub_1(S, Len, I, Len);
do_sub(S, Len, I, [J]) -> do_sub_1(S, Len, I, J).

do_sub_1(S, Len, I0, J0) ->
    I1 = do_sub_m(Len, I0),
    J1 = do_sub_m(Len, J0),
    do_sub_ij(S, Len, I1, J1).

do_sub_m(Len, I) when I < 0 -> Len+I+1;		%Negative count from end
do_sub_m(_, I) -> I.

do_sub_ij(S, Len, I, J) when I < 1 -> do_sub_ij(S, Len, 1, J);
do_sub_ij(S, Len, I, J) when J > Len -> do_sub_ij(S, Len, I, Len);
do_sub_ij(_, _, I, J) when I > J -> <<>>;
do_sub_ij(S, _, I, J) ->
    binary:part(S, I-1, J-I+1).			%Zero-based, yuch!

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

seq([$^|Cs], Sd, Sn, P) -> single(Cs, Sd, Sn, ['^'|P]);
seq([_|_]=Cs, Sd, Sn, P) -> single(Cs, Sd, Sn, P);
seq([], Sd, Sn, P) -> {lists:reverse(P),Sd,Sn}.

single([$(|Cs], Sd, Sn, P) -> single(Cs, Sd+1, Sn+1, [{'(',Sn}|P]);
single([$)|_], 0, _, _) -> throw({error,invalid_capture});
single([$)|Cs], Sd, Sn, P) -> single(Cs, Sd-1, Sn, [')'|P]);
single([$[|Cs], Sd, Sn, P) -> char_set(Cs, Sd, Sn, P);
single([$.|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, ['.'|P]);
single([$%|Cs], Sd, Sn, P) -> char_class(Cs, Sd, Sn, P);
single([$$], Sd, Sn, P) -> {lists:reverse(P, ['\$']),Sd,Sn};
single([C|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [C|P]);
single([], Sd, Sn, P) -> {lists:reverse(P),Sd,Sn}.

singlep([$*|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{kclosure,Char}|P]);
singlep([$+|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{pclosure,Char}|P]);
singlep([$-|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{mclosure,Char}|P]);
singlep([$?|Cs], Sd, Sn, [Char|P]) -> single(Cs, Sd, Sn, [{optional,Char}|P]);
singlep(Cs, Sd, Sn, P) -> single(Cs, Sd, Sn, P).

char_set([$^|Cs], Sd, Sn, P) -> char_set(Cs, Sd, Sn, P, comp_set);
char_set(Cs, Sd, Sn, P) -> char_set(Cs, Sd, Sn, P, char_set).

char_set(Cs0, Sd, Sn, P, Tag) ->
    case char_set(Cs0) of
	{Set,[$]|Cs1]} -> singlep(Cs1, Sd, Sn, [{Tag,Set}|P]);
	{_,_} -> throw({error,invalid_char_set})
    end.

char_set([$]|Cs]) -> char_set(Cs, [$]]);	%Must special case this
char_set(Cs) -> char_set(Cs, []).

char_set([$%,C|Cs], Set) -> char_set(Cs, [char_class(C)|Set]);
char_set([C1,$-,C2|Cs], Set) when C2 =/= $] -> char_set(Cs, [{C1,C2}|Set]);
char_set([C|Cs], Set) when C =/= $] -> char_set(Cs, [C|Set]);
char_set(Cs, Set) -> {Set,Cs}.

%% char_class([$f,$[|Cs], Sd, Sn, P) ->
%%     char_set(Cs, Sd, Sn, [frontier|P]);
char_class([$f|_], _, _, _) -> throw({error,invalid_pattern});
char_class([$b,L,R|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [{balance,L,R}|P]);
char_class([C|Cs], Sd, Sn, P) -> singlep(Cs, Sd, Sn, [char_class(C)|P]);
char_class([], _, _, _) -> throw({error,invalid_pattern}).

char_class($a) -> 'a';
char_class($A) -> 'A';
char_class($c) -> 'c';
char_class($C) -> 'C';
char_class($d) -> 'd';
char_class($D) -> 'D';
char_class($g) -> 'g';
char_class($G) -> 'G';
char_class($l) -> 'l';
char_class($L) -> 'L';
char_class($p) -> 'p';
char_class($P) -> 'P';
char_class($s) -> 's';
char_class($S) -> 'S';
char_class($u) -> 'u';
char_class($U) -> 'U';
char_class($w) -> 'w';
char_class($W) -> 'W';
char_class($x) -> 'x';
char_class($X) -> 'X';
char_class($z) -> 'z';				%Deprecated
char_class($Z) -> 'Z';
char_class(C) ->				%Only non-alphanum allowed
    case is_w_char(C) of
	true -> throw({error,{invalid_char_class,C}});
	false -> C
    end.

test_do_match(S, P, I) ->
    {ok,{Pat,_},_} = pat(P),
    io:fwrite("tdm: ~p\n", [{Pat}]),
    do_match(S, Pat, I).

%% do_match(String, Pattern, Index) -> {match,[Capture],Rest,Index} | nomatch.
%%  Try and match the pattern with the string *at the current
%%  position*. No searching.

do_match(S0, P0, I0) ->
    case do_match(P0, S0, I0, [{0,I0}], []) of
	{match,S1,I1,_,Cas} ->{match,Cas,S1,I1};
	{nomatch,_,_,_,_,_} -> nomatch
    end.

do_match(['\$']=Ps, Cs, I, Ca, Cas) ->		%Match only end of string
    case Cs of
	<<>> -> do_match([], <<>>, I, Ca, Cas);
	_ -> {nomatch,Ps,Cs,I,Ca,Cas}
    end;
do_match(['^'|Ps]=Ps0, Cs, I, Ca, Cas) ->	%Match beginning of string
    if I =:= 1 -> do_match(Ps, Cs, 1, Ca, Cas);
       true -> {nomatch,Ps0,Cs,I,Cs,Cas}
    end;
do_match([{'(',Sn},')'|P], Cs, I, Ca, Cas) ->
    do_match(P, Cs, I, Ca, save_cap(Sn, I, -1, Cas));
do_match([{'(',Sn}|P], Cs, I, Ca, Cas) ->
    do_match(P, Cs, I, [{Sn,I}|Ca], Cas);
do_match([')'|P], Cs, I, [{Sn,S}|Ca], Cas) ->
    do_match(P, Cs, I, Ca, save_cap(Sn, S, I-S, Cas));
do_match([{kclosure,P}=K|Ps], Cs, I, Ca, Cas) ->
    %%io:fwrite("dm: ~p\n", [{[P,K|Ps],Cs,I,Ca,Cas}]),
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
do_match([{char_set,Set}|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) ->
    case match_char_set(Set, C) of
	true -> do_match(Ps, Cs, I+1, Ca, Cas);
	false -> {nomatch,Ps0,Cs0,I,Ca,Cas}
    end;
do_match([{comp_set,Set}|Ps]=Ps0, <<C,Cs/binary>>=Cs0, I, Ca, Cas) ->
    case match_char_set(Set, C) of
	true -> {nomatch,Ps0,Cs0,I,Ca,Cas};
	false -> do_match(Ps, Cs, I+1, Ca, Cas)
    end;
do_match([{balance,L,R}|Ps]=Ps0, <<L,Cs1/binary>>=Cs0, I0, Ca, Cas) ->
    case balance(Cs1, I0+1, L, R, 1) of
	{ok,Cs2,I1} -> do_match(Ps, Cs2, I1, Ca, Cas);
	error -> {nomatch,Ps0,Cs0,I0,Ca,Cas}
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

%% MUST first check for right char, this in case of L == R!
balance(<<R,Cs/binary>>, I, L, R, D) ->
    if D =:= 1 -> {ok,Cs,I+1};
       true -> balance(Cs, I+1, L, R, D-1)
    end;
balance(<<L,Cs/binary>>, I, L, R, D) -> balance(Cs, I+1, L, R, D+1);
balance(<<_,Cs/binary>>, I, L, R, D) -> balance(Cs, I+1, L, R, D);
balance(<<>>, _, _, _, _) -> error.

match_class('a', C) -> is_a_char(C);
match_class('A', C) -> not is_a_char(C);
match_class('c', C) -> is_c_char(C);
match_class('C', C) -> not is_c_char(C);
match_class('d', C) -> is_d_char(C);
match_class('D', C) -> not is_d_char(C);
match_class('g', C) -> is_g_char(C);
match_class('G', C) -> not is_g_char(C);
match_class('l', C) -> is_l_char(C);
match_class('L', C) -> not is_l_char(C);
match_class('p', C) -> is_p_char(C);
match_class('P', C) -> not is_p_char(C);
match_class('s', C) -> is_s_char(C);
match_class('S', C) -> not is_s_char(C);
match_class('u', C) -> is_u_char(C);
match_class('U', C) -> not is_u_char(C);
match_class('w', C) -> is_w_char(C);
match_class('W', C) -> not is_w_char(C);
match_class('x', C) -> is_x_char(C);
match_class('X', C) -> not is_x_char(C);
match_class('z', C) -> is_z_char(C);		%Deprecated
match_class('Z', C) -> not is_z_char(C).

match_char_set([{C1,C2}|_], C) when C >= C1, C=< C2 -> true;
match_char_set([A|Set], C) when is_atom(A) ->
    match_class(A, C) orelse match_char_set(Set, C);
match_char_set([C|_], C) -> true;
match_char_set([_|Set], C) -> match_char_set(Set, C);
match_char_set([], _) -> false.

%% Test for various character types.

is_a_char(C) ->					%All letters
    is_l_char(C) orelse is_u_char(C).

is_c_char(C) when C >= 0, C =< 31 -> true;	%All control characters
is_c_char(C) when C >= 128, C =< 159 -> true;
is_c_char(_) -> false.

is_d_char(C) -> (C >= $0) and (C =< $9).	%All digits

is_g_char(C) when C >= 33, C =< 126 -> true;	%All printable characters
is_g_char(C) when C >= 161, C =< 255 -> true;
is_g_char(_) -> false.

is_l_char(C) when C >= $a, C =< $z -> true;	%All lowercase letters
is_l_char(C) when C >= 224, C =< 246 -> true;
is_l_char(C) when C >= 248, C =< 255 -> true;
is_l_char(_) -> false.

is_p_char(C) when C >= 33, C =< 47 -> true;	%All punctutation characters
is_p_char(C) when C >= 58, C =< 63 -> true;
is_p_char(C) when C >= 91, C =< 96 -> true;
is_p_char(126) -> true;
is_p_char(C) when C >= 161, C =< 191 -> true;
is_p_char(215) -> true;
is_p_char(247) -> true;
is_p_char(_) -> false.

is_s_char(C) when C >= 9, C =< 13 -> true;	%Space characters
is_s_char(32) -> true;
is_s_char(160) -> true;
is_s_char(_) -> false.

is_u_char(C) when C >= $A, C =< $Z -> true;	%All uppercase letters
is_u_char(C) when C >= 192, C =< 214 -> true;
is_u_char(C) when C >= 216, C =< 223 -> true;
is_u_char(_) -> false.

is_w_char(C) ->					%All alphanumeric characters
    is_a_char(C) orelse is_d_char(C).

is_x_char(C) when C >= $a, C =< $f -> true;	%All hexadecimal characters
is_x_char(C) when C >= $A, C =< $F -> true;
is_x_char(C) -> is_d_char(C).

is_z_char(C) -> C =:= 0.			%The zero character, deprecated

%% match_class('a', C) -> (char_table(C) band ?_A) =/= 0;
%% match_class('A', C) ->  (char_table(C) band ?_A) =:= 0.

%% char_table(C) when C >= 0, C =< 31 -> ?_C;
%% char_table(C) when C >= 65, C =< 91 -> ?_U bor ?_A;
%% char_table(C) when C >= 97, C =< 123 -> ?_L;
