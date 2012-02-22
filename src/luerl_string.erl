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

-export([table/0]).

table() ->
    [{<<"byte">>,{function,fun byte/2}},
     {<<"char">>,{function,fun char/2}},
     {<<"format">>,{function, fun format/2}},
     {<<"len">>,{function,fun len/2}},
     {<<"lower">>,{function,fun lower/2}},
     {<<"rep">>,{function,fun rep/2}},
     {<<"reverse">>,{function,fun reverse/2}},
     {<<"sub">>,{function,fun sub/2}},
     {<<"upper">>,{function,fun upper/2}}
    ].

byte([A], St) -> byte(A, 1, 1, St);
byte([A1,A2], St) -> byte(A1, A2, A2, St);
byte([A1,A2,A3|_], St) -> byte(A1, A2, A3, St);
byte(As, _) -> error({badarg,byte,As}).

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
format(As, _) -> error({badarg,format,As}).

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
len(As, _) -> error({badarg,len,As}).

lower([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(string:to_lower(S))],St};
lower(As, _) -> error({badarg,lower,As}).

rep([A1,A2], St) -> rep([A1,A2,<<>>], St);
rep([_,_,_|_]=As, St) ->
    case luerl_lib:conv_list(As, [list,int,list]) of
	[S,I,Sep] ->
	    if I > 0 ->
		    {[iolist_to_binary([S|lists:duplicate(I-1, [Sep,S])])],St};
	       true -> {[<<>>],St}
	    end;
	nil ->					%Error or bad values
	    error({badarg,rep,As})
    end;
rep(As, _) -> error({badarg,rep,As}).

reverse([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(lists:reverse(S))],St};
reverse(As, _) -> error({badarg,reverse,As}).

sub([A1|As], St) ->
    do_sub(luerl_lib:conv_list([A1|As], [string,int,int]), St);
sub(As, _) -> error({badarg,sub,As}).

do_sub(nil, _) -> error({badarg,sub,nil});
do_sub([S|Is], St) ->
    Len = byte_size(S),
    Sub = case Is of
	      %% The cases where we just have an I.
	      [I] -> do_sub(S, Len, I);
	      [I,J] -> do_sub(S, Len, I, J)
	  end,
    {[Sub],St}.

do_sub(S, _, 0) -> S;
do_sub(S, Len, I) when I < 1 -> do_sub(S, Len, Len+I+1, Len);
do_sub(S, Len, I) -> do_sub(S, Len, I, Len).

do_sub(S, Len, I, J) when I < 1 -> do_sub(S, Len, 1, J);
do_sub(S, Len, I, J) when J < 0 -> do_sub(S, Len, I, Len+J+1);
do_sub(S, Len, I, J) when J > Len -> do_sub(S, Len, I, Len);
do_sub(_, Len, I, _) when I > Len -> <<>>;
do_sub(_, _, I, J) when J < I -> <<>>;
do_sub(S, _, I, J) -> binary:part(S, I-1, J-I+1). %Zero-based, yuch!

upper([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:to_list(A),
    {[list_to_binary(string:to_upper(S))],St};
upper(As, _) -> error({badarg,upper,As}).
