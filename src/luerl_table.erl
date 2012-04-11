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

-export([install/1,length/2]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

install(St) ->
    luerl_eval:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"concat">>,{function,fun concat/2}},
     {<<"pack">>,{function,fun pack/2}},
     {<<"sort">>,{function,fun sort/2}},
     {<<"unpack">>,{function,fun unpack/2}}
    ].

concat([#tref{}=T], St) -> concat(T, <<>>, [1.0], St);
concat([#tref{}=T,A2], St) -> concat(T, A2, [1.0], St);
concat([#tref{}=T,A2|As], St) -> concat(T, A2, As, St);
concat(As, _) -> lua_error({badarg,concat,As}).

concat(#tref{i=N}=T, A2, As, St) ->
    #table{t=Tab} = ?GET_TABLE(N, St#luerl.tabs),
    case luerl_lib:tostrings([A2], luerl_lib:tointegers(As)) of
	[Sep|Is] -> {[concat(Tab, Sep, Is)],St};
	_ -> lua_error({badarg,concat,[T,A2|As]})
    end.

concat(Tab, Sep, [I]) ->
    Rest = skip_until(Tab, I),
    Conc = concat_loop(Rest, I),
    concat_join(Conc, Sep);
concat(Tab, Sep, [I,J|_]) ->
    Rest = skip_until(Tab, I),
    Conc = concat_loop(Rest, I, J),
    concat_join(Conc, Sep).

concat_loop([{N,V}|Tab], N) ->			%An interesting element
    case luerl_lib:to_list(V) of		%Check if right type
	nil -> lua_error({illegal_val,concat,V});
	S -> [S|concat_loop(Tab, N+1)]
    end;
concat_loop([{K,_}|Tab], N) when K < N ->	%Skip intermediates
    concat_loop(Tab, N);
concat_loop(_, _) -> [].			%No more interesting elements

concat_loop(_, N, J) when N > J -> [];		%Done
concat_loop([{N,V}|Tab], N, J) ->		%An interesting element
    case luerl_lib:to_list(V) of		%Check if right type
	nil -> lua_error({illegal_val,concat,V});
	S -> [S|concat_loop(Tab, N+1, J)]
    end;
concat_loop([{K,_}|Tab], N, J) when K < N ->	%Skip intermediates
    concat_loop(Tab, N, J);
concat_loop(_, _, J) ->				%No more interesting elements
    lua_error({illegal_val,concat,J}).

concat_join([E], _) -> list_to_binary(E);
concat_join([E1|Es], Sep) ->
    iolist_to_binary([E1|[ [Sep,E] || E <- Es ]]);
concat_join([], _) -> <<>>.


pack(As, St0) ->
    T = pack_loop(As, 0.0),			%Indexes are floats!
    {Tab,St1} = luerl_eval:alloc_table(T, St0),
    {[Tab],St1}.

pack_loop([E|Es], N) ->				%In order for an orddict!
    [{N+1,E}|pack_loop(Es, N+1)];
pack_loop([], N) -> [{<<"n">>,N}].

unpack([#tref{i=N}=T|As], St) ->
    #table{t=Tab} = ?GET_TABLE(N, St#luerl.tabs),
    case luerl_lib:tointegers(unpack_args(As)) of
	[I] ->
	    Start = skip_until(Tab, I),
	    {unpack_loop(Start, I),St};
	[I,J] ->
	    Start = skip_until(Tab, I),
	    {unpack_loop(Start, I, J),St};
	_ -> lua_error({badarg,unpack,[T|As]})
    end;
unpack([], _) -> lua_error({badarg,unpack,[]}).

unpack_args([]) -> unpack_args([1.0]);		%Just start from the beginning
unpack_args([nil|As]) -> unpack_args([1.0|As]);
unpack_args([I]) -> [I];			%Only one argument
unpack_args([I,nil|_]) -> [I];			%Goto the default end
unpack_args([I,J|_]) -> [I,J].			%Two arguments

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

sort([#tref{i=N}], St0) ->
    Comp = fun ([{_,A},{_,B}], St) -> lt_comp(A, B, St) end,
    St1 = do_sort(Comp, St0, N),
    {[],St1};
sort([#tref{i=N},Func|_], St0) ->
    Comp = fun ([{_,A},{_,B}], St) ->
		   luerl_eval:functioncall(Func, [A,B], St)
	   end,
    St1 = do_sort(Comp, St0, N),
    {[],St1};
sort(As, _) -> lua_error({badarg,sort,As}).

do_sort(Sort, St0, N) ->
    #table{t=Tab0}=T = ?GET_TABLE(N, St0#luerl.tabs),
    {Tab1,St1} = merge_sort(Sort, St0, Tab0),
    Tab2 = renumber(Tab1),
    Ts0 = St1#luerl.tabs,
    Ts1 = ?SET_TABLE(N, T#table{t=Tab2}, Ts0),
    St1#luerl{tabs=Ts1}.

%% lt_comp(O1, O2, State) -> {[Bool],State}.
%%  Proper Lua '<' comparison.

lt_comp(O1, O2, St) when is_number(O1), is_number(O2) -> {[O1 =< O2],St};
lt_comp(O1, O2, St) when is_binary(O1), is_binary(O2) -> {[O1 =< O2],St};
lt_comp(O1, O2, St0) ->
    case luerl_eval:getmetamethod(O1, O2, <<"__lt">>, St0) of
	nil -> lua_error({illegal_comp,sort});
	Meta ->
	    {Ret,St1} = luerl_eval:functioncall(Meta, [O1,O2], St0),
	    {[is_true(Ret)],St1}
    end.

renumber(Tab0) ->
    Fun = fun ({_,V}, I) -> {{I,V},I+1} end,
    {Tab1,_} = lists:mapfoldl(Fun, 1.0, Tab0),
    Tab1.

%% length(Stable, State) -> {Length,State}.
%%  The length of a table is the number of numeric keys in sequence
%%  from 1.0.

length(#tref{i=N}=T, St) ->
    Meta = luerl_eval:getmetamethod(T, <<"__len">>, St),
    if ?IS_TRUE(Meta) -> luerl_eval:functioncall(Meta, [T], St);
       true ->
	    #table{t=Tab} = ?GET_TABLE(N, St#luerl.tabs),
	    {[length_loop(Tab)],St}
    end.

length_loop([{1.0,_}|T]) -> length_loop(T, 2.0);
length_loop([_|T]) -> length_loop(T);
length_loop([]) -> 0.0.

length_loop([{K,_}|T], K) -> length_loop(T, K+1);
length_loop(_, N) -> N-1.

%% sort(A,B,C) -> sort_up(A,B,C).

%% sort_up(A,B,[X,Y|L]) ->
%%     case X =< Y of
%% 	true -> merge_dn([Y,X], sort_dn(A, B, L), []);
%% 	false -> merge_dn([X,Y], sort_dn(A, B, L), [])
%%     end;
%% sort_up(A,B,[X]) -> [X];
%% sort_up(A,B,[]) -> [].

%% sort_dn(A,B,[X,Y|L]) ->
%%     case X =< Y of
%% 	true -> merge_up([X,Y], sort_up(A, B, L), []);
%% 	false ->  merge_up([Y,X], sort_up(A, B, L), [])
%%     end;
%% sort_dn(A,B,[X]) -> [X];
%% sort_dn(A,B,[]) -> [].

%% merge(A,B,C) ->
%%     merge_dn(A,B,C).

%% %% merge_up(L1, L2, Acc)
%% %%  L1, L2 increasing, Acc will be decreasing

%% merge_up([X|Xs]=Xs0, [Y|Ys]=Ys0, Acc) ->
%%     case X =< Y of
%% 	true -> merge_up(Xs, Ys0, [X|Acc]);
%% 	false -> merge_up(Xs0, Ys, [Y|Acc])
%%     end;
%% merge_up([X|Xs], [], Acc) -> merge_up(Xs, [], [X|Acc]);
%% merge_up([], [Y|Ys], Acc) -> merge_up([], Ys, [Y|Acc]);
%% merge_up([], [], Acc) -> Acc.

%% %% merge_dn(L1, L2, Acc)
%% %%  L1, L2 decreasing, Acc will be increasing

%% merge_dn([X|Xs]=Xs0, [Y|Ys]=Ys0, Acc) ->
%%     case X =< Y of
%% 	true -> merge_dn(Xs0, Ys, [Y|Acc]);
%% 	false -> merge_dn(Xs, Ys0, [X|Acc])
%%     end;
%% merge_dn([X|Xs], [], Acc) -> merge_dn(Xs, [], [X|Acc]);
%% merge_dn([], [Y|Ys], Acc) -> merge_dn([], Ys, [Y|Acc]);
%% merge_dn([], [], Acc) -> Acc.

%% merge_sort(CompFun, State, List) -> {SortedList,State}.
%%  The code here has been taken from the sort/2 code in lists.erl and
%%  converted to chain State through all calls to the comparison
%%  function.

merge_sort(_, St, []) -> {[],St};
merge_sort(_, St, [_] = L) -> {L,St};
merge_sort(Fun, St0, [X, Y|T]) ->
    {Ret,St1} = Fun([X,Y], St0),
    case is_true(Ret) of
	true ->
	    fsplit_1(Y, X, Fun, St1, T, [], []);
	false ->
	    fsplit_2(Y, X, Fun, St1, T, [], [])
    end.

%% Ascending.
fsplit_1(Y, X, Fun, St0, [Z|L], R, Rs) ->
    {Ret1,St1} = Fun([Y,Z], St0),
    case is_true(Ret1) of
        true ->
            fsplit_1(Z, Y, Fun, St1, L, [X|R], Rs);
        false ->
	    {Ret2,St2} = Fun([X,Z], St1),
            case is_true(Ret2) of
                true ->
                    fsplit_1(Y, Z, Fun, St2, L, [X|R], Rs);
                false when R == [] ->
                    fsplit_1(Y, X, Fun, St2, L, [Z], Rs);
                false ->
                    fsplit_1_1(Y, X, Fun, St2, L, R, Rs, Z)
            end
    end;
fsplit_1(Y, X, Fun, St, [], R, Rs) ->
    rfmergel([[Y, X|R]|Rs], [], Fun, St, asc).

fsplit_1_1(Y, X, Fun, St0, [Z|L], R, Rs, S) ->
    {Ret1,St1} = Fun([Y,Z], St0),
    case is_true(Ret1) of
        true ->
            fsplit_1_1(Z, Y, Fun, St1, L, [X|R], Rs, S);
        false ->
	    {Ret2,St2} = Fun([X,Z], St1),
            case is_true(Ret2) of
                true ->
                    fsplit_1_1(Y, Z, Fun, St2, L, [X|R], Rs, S);
                false ->
		    {Ret3,St3} = Fun([S,Z], St2),
                    case is_true(Ret3) of
                        true ->
                            fsplit_1(Z, S, Fun, St3, L, [], [[Y, X|R]|Rs]);
                        false ->
                            fsplit_1(S, Z, Fun, St3, L, [], [[Y, X|R]|Rs])
                    end
            end
    end;
fsplit_1_1(Y, X, Fun, St, [], R, Rs, S) ->
    rfmergel([[S], [Y, X|R]|Rs], [], Fun, St, asc).

%% Descending.
fsplit_2(Y, X, Fun, St0, [Z|L], R, Rs) ->
    {Ret1,St1} = Fun([Y,Z], St0),
    case is_true(Ret1) of
        false ->
            fsplit_2(Z, Y, Fun, St1, L, [X|R], Rs);
        true ->
	    {Ret2,St2} = Fun([X,Z], St1),
            case is_true(Ret2) of
                false ->
                    fsplit_2(Y, Z, Fun, St2, L, [X|R], Rs);
                true when R == [] ->
                    fsplit_2(Y, X, Fun, St2, L, [Z], Rs);
                true ->
                    fsplit_2_1(Y, X, Fun, St2, L, R, Rs, Z)
            end
    end;
fsplit_2(Y, X, Fun, St, [], R, Rs) ->
    fmergel([[Y, X|R]|Rs], [], Fun, St, desc).

fsplit_2_1(Y, X, Fun, St0, [Z|L], R, Rs, S) ->
    {Ret1,St1} = Fun([Y,Z], St0),
    case is_true(Ret1) of
        false ->
            fsplit_2_1(Z, Y, Fun, St1, L, [X|R], Rs, S);
        true ->
	    {Ret2,St2} = Fun([X,Z], St1),
            case is_true(Ret2) of
                false ->
                    fsplit_2_1(Y, Z, Fun, St2, L, [X|R], Rs, S);
                true ->
		    {Ret3,St3} = Fun([S,Z], St2),
                    case is_true(Ret3) of
                        false ->
                            fsplit_2(Z, S, Fun, St3, L, [], [[Y, X|R]|Rs]);
                        true ->
                            fsplit_2(S, Z, Fun, St3, L, [], [[Y, X|R]|Rs])
                    end
            end
    end;
fsplit_2_1(Y, X, Fun, St, [], R, Rs, S) ->
    fmergel([[S], [Y, X|R]|Rs], [], Fun, St, desc).

fmergel([T1, [H2|T2]|L], Acc, Fun, St0, asc) ->
    {L1,St1} = fmerge2_1(T1, H2, Fun, St0, T2, []),
    fmergel(L, [L1|Acc], Fun, St1, asc);
fmergel([[H2|T2], T1|L], Acc, Fun, St0, desc) ->
    {L1,St1} = fmerge2_1(T1, H2, Fun, St0, T2, []),
    fmergel(L, [L1|Acc], Fun, St1, desc);
fmergel([L], [], _Fun, St, _O) -> {L,St};
fmergel([L], Acc, Fun, St, O) ->
    rfmergel([lists:reverse(L, [])|Acc], [], Fun, St, O);
fmergel([], Acc, Fun, St, O) ->
    rfmergel(Acc, [], Fun, St, O).

rfmergel([[H2|T2], T1|L], Acc, Fun, St0, asc) ->
    {L1,St1} = rfmerge2_1(T1, H2, Fun, St0, T2, []),
    rfmergel(L, [L1|Acc], Fun, St1, asc);
rfmergel([T1, [H2|T2]|L], Acc, Fun, St0, desc) ->
    {L1,St1} = rfmerge2_1(T1, H2, Fun, St0, T2, []),
    rfmergel(L, [L1|Acc], Fun, St1, desc);
rfmergel([L], Acc, Fun, St, O) ->
    fmergel([lists:reverse(L, [])|Acc], [], Fun, St, O);
rfmergel([], Acc, Fun, St, O) ->
    fmergel(Acc, [], Fun, St, O).

%% Elements from the first list are prioritized.
fmerge2_1([H1|T1], H2, Fun, St0, T2, M) ->
    {Ret,St1} = Fun([H1,H2], St0),
    case is_true(Ret) of
        true ->
            fmerge2_1(T1, H2, Fun, St1, T2, [H1|M]);
        false ->
            fmerge2_2(H1, T1, Fun, St1, T2, [H2|M])
    end;
fmerge2_1([], H2, _Fun, St, T2, M) ->
    {lists:reverse(T2, [H2|M]),St}.

fmerge2_2(H1, T1, Fun, St0, [H2|T2], M) ->
    {Ret,St1} = Fun([H1,H2], St0),
    case is_true(Ret) of
        true ->
            fmerge2_1(T1, H2, Fun, St1, T2, [H1|M]);
        false ->
            fmerge2_2(H1, T1, Fun, St1, T2, [H2|M])
    end;
fmerge2_2(H1, T1, _Fun, St, [], M) ->
    {lists:reverse(T1, [H1|M]),St}.

%% rmerge/3

rfmerge2_1([H1|T1], H2, Fun, St0, T2, M) ->
    {Ret,St1} = Fun([H1,H2], St0),
    case is_true(Ret) of
        true ->
            rfmerge2_2(H1, T1, Fun, St1, T2, [H2|M]);
        false ->
            rfmerge2_1(T1, H2, Fun, St1, T2, [H1|M])
    end;
rfmerge2_1([], H2, _Fun, St, T2, M) ->
    {lists:reverse(T2, [H2|M]),St}.

rfmerge2_2(H1, T1, Fun, St0, [H2|T2], M) ->
    {Ret,St1} = Fun([H1,H2], St0),
    case is_true(Ret) of
        true ->
            rfmerge2_2(H1, T1, Fun, St1, T2, [H2|M]);
        false ->
            rfmerge2_1(T1, H2, Fun, St1, T2, [H1|M])
    end;
rfmerge2_2(H1, T1, _Fun, St, [], M) ->
    {lists:reverse(T1, [H1|M]),St}.

%% is_true(Rets) -> boolean().

is_true([nil|_]) -> false;
is_true([false|_]) -> false;
is_true([_|_]) -> true;
is_true([]) -> false.
