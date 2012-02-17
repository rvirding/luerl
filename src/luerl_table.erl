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

%% File    : luerl_table.erl
%% Author  : Robert Virding
%% Purpose : The table library for Luerl.

%% These functions sometimes behave strangely in the Lua 5.2
%% libraries, but we try to follow them. Most of these functions KNOW
%% that a table is an orddict!

-module(luerl_table).

-include("luerl.hrl").

-export([table/0]).

%% table() -> [{FuncName,Function}].
%% Caller will convert this list to the correct format.

table() ->
    [{<<"concat">>,{function,fun concat/2}},
     {<<"pack">>,{function,fun pack/2}},
     {<<"unpack">>,{function,fun unpack/2}}
    ].

concat([{table,_}=T], St) -> concat(T, <<>>, [1.0], St);
concat([{table,_}=T,A2], St) -> concat(T, A2, [1.0], St);
concat([{table,_}=T,A2|As], St) -> concat(T, A2, As, St);
concat(As, _) -> error({badarg,concat,As}).

concat({table,N}=T, A2, As, St) ->
    {Tab,_} = ?GET_TABLE(N, St#luerl.tabs),
    case {luerl_lib:tostring(A2),luerl_lib:tointegers(As)} of
	{Sep,Is} when Sep =/= nil, Is =/= nil ->
	    {[concat(Tab, Sep, Is)],St};
	_ -> error({badarg,concat,[T,A2|As]})
    end.

concat(_, _, [I|_]) when I < 1.0 ->
    error({illegal_val,concat,I});
concat(Tab, Sep, [I]) ->
    Rest = skip_until(Tab, I),
    Conc = concat_loop(Rest, I),
    concat_join(Conc, Sep);
concat(Tab, Sep, [I,J|_]) ->
    Rest = skip_until(Tab, I),
    Conc = concat_loop(Rest, I, J),
    concat_join(Conc, Sep).

concat_loop([{N,V}|Tab], N) ->			%An interesting element
    case luerl_lib:tolist(V) of			%Check if right type
	nil -> error({illegal_val,concat,V});
	S -> [S|concat_loop(Tab, N+1)]
    end;
concat_loop([{K,_}|Tab], N) when K < N ->	%Skip intermediates
    concat_loop(Tab, N);
concat_loop(_, _) -> [].			%No more interesting elements

concat_loop(_, N, J) when N > J -> [];		%Done
concat_loop([{N,V}|Tab], N, J) ->		%An interesting element
    case luerl_lib:tolist(V) of			%Check if right type
	nil -> error({illegal_val,concat,V});
	S -> [S|concat_loop(Tab, N+1, J)]
    end;
concat_loop([{K,_}|Tab], N, J) when K < N ->	%Skip intermediates
    concat_loop(Tab, N, J);
concat_loop(_, _, J) ->				%No more interesting elements
    error({illegal_val,concat,J}).

concat_join([E], _) -> list_to_binary(E);
concat_join([E1|Es], Sep) ->
    iolist_to_binary([E1|[ [Sep,E] || E <- Es ]]);
concat_join([], _) -> <<>>.


pack(As, St0) ->
    T = pack_loop(As, 0),
    {Tab,St1} = luerl_eval:alloc_table(T, St0),
    {[Tab],St1}.

pack_loop([E|Es], N) ->				%In order for an orddict!
    [{N+1,E}|pack_loop(Es, N+1)];
pack_loop([], N) -> [{<<"n">>,N}].

unpack([{table,N}=T|As], St) ->
    {Tab,_} = ?GET_TABLE(N, St#luerl.tabs),
    case luerl_lib:tointegers(As) of
	[I] ->
	    Start = skip_until(Tab, I),
	    {unpack_loop(Start, I),St};
	[I,J|_] ->
	    Start = skip_until(Tab, I),
	    {unpack_loop(Start, I, J),St};
	_ -> error({badarg,unpack,[T|As]})
    end.

skip_until([{K,_}|_]=Tab, I) when K >= I -> Tab;
skip_until([_|Tab], I) -> skip_until(Tab, I);
skip_until([], _) -> [].

%% Unpack until we reach th end of the list.

unpack_loop([{N,V}|Tab], N) -> [V|unpack_loop(Tab, N+1)];
unpack_loop([{K,_}|Tab], N) when K < N ->	%Skip inbetween keys
    unpack_loop(Tab, N);
unpack_loop(_, _) -> [].			%Next key bigger than next N

unpack_loop(_, N, J) when N > J -> [];			%Done
unpack_loop([{N,V}|Tab], N, J) -> [V|unpack_loop(Tab, N+1, J)];
unpack_loop([{K,_}|_]=Tab, N, J) when K > N -> [nil|unpack_loop(Tab, N+1, J)];
unpack_loop([{K,_}|Tab], N, J) when K < N -> unpack_loop(Tab, N, J);
unpack_loop([], N, J) -> [nil|unpack_loop([], N+1, J)].
