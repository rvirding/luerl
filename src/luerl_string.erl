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
     {<<"len">>,{function,fun len/2}},
     {<<"lower">>,{function,fun lower/2}},
     {<<"rep">>,{function,fun rep/2}},
     {<<"reverse">>,{function,fun reverse/2}},
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

len([A|_], St) when is_binary(A) -> {byte_size(A),St};
len([A|_], St) when is_number(A) ->
    {[length(luerl_lib:number_to_list(A))],St};
len(As, _) -> error({badarg,len,As}).

lower([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:tolist(A),
    {[list_to_binary(string:to_lower(S))],St};
lower(As, _) -> error({badarg,lower,As}).

rep([A1,A2], St) -> rep([A1,A2,<<>>], St);
rep([A1,A2,A3|_], St) ->
    case catch {luerl_lib:tolist(A1),
		luerl_lib:tonumber(A2),
		luerl_lib:tolist(A3)} of
	{S,N,Sep} when S =/= nil, N =/= nil, Sep =/= nil ->
	    case round(N) of
		I when I > 0 ->
		    {[iolist_to_binary([S|lists:duplicate(I-1, [Sep,S])])],St};
		_ -> {[<<>>],St}
	    end;
	_ ->					%Error or bad values
	    error({badarg,rep,[A1,A2,A3]})
    end;
rep(As, _) -> error({badarg,rep,As}).

reverse([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:tolist(A),
    {[list_to_binary(lists:reverse(S))],St};
reverse(As, _) -> error({badarg,reverse,As}).

upper([A|_], St) when is_binary(A) ; is_number(A) ->
    S = luerl_lib:tolist(A),
    {[list_to_binary(string:to_upper(S))],St};
upper(As, _) -> error({badarg,upper,As}).
