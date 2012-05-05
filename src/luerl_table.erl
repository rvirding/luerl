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

-export([install/1,length/2,test_insert/2,test_insert/3]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

install(St) ->
    luerl_eval:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"concat">>,{function,fun concat/2}},
     {<<"insert">>,{function,fun insert/2}},
     {<<"pack">>,{function,fun pack/2}},
     {<<"remove">>,{function,fun remove/2}},
     {<<"sort">>,{function,fun sort/2}},
     {<<"unpack">>,{function,fun unpack/2}}
    ].

%% concat - concat the elements of a list into a string.

concat([#tref{}=T], St) -> concat(T, <<>>, [1.0], St);
concat([#tref{}=T,A2], St) -> concat(T, A2, [1.0], St);
concat([#tref{}=T,A2|As], St) -> concat(T, A2, As, St);
concat(As, _) -> lua_error({badarg,concat,As}).

concat(#tref{i=N}=T, A2, As, St) ->
    #table{a=Arr} = ?GET_TABLE(N, St#luerl.tabs),
    case luerl_lib:tostrings([A2], luerl_lib:to_ints(As)) of
	[Sep|Is] -> {[concat(Arr, Sep, Is)],St};
	_ -> lua_error({badarg,concat,[T,A2|As]})
    end.

concat(Arr, Sep, [I]) ->
    Conc = concat_loop(Arr, I, length_loop(Arr, 1)),
    concat_join(Conc, Sep);
concat(Arr, Sep, [I,J|_]) ->
    Conc = concat_loop(Arr, I, J),
    concat_join(Conc, Sep).

%% This and unpack_loop are very similar.
concat_loop(_, N, J) when N > J -> [];		%Done
concat_loop([{N,V}|Tab], N, J) ->
    case luerl_lib:to_list(V) of
	nil -> lua_error({illegal_val,concat,V});
	S -> [S|concat_loop(Tab, N+1, J)]
    end;
concat_loop([{K,_}|_], N, _) when K > N ->	%Gap
    lua_error({illegal_val,concat,nil});
concat_loop([{K,_}|Tab], N, J) when K < N ->
    concat_loop(Tab, N, J);
concat_loop([], _, _) -> lua_error({illegal_val,concat,nil}).

concat_join([E], _) -> list_to_binary(E);
concat_join([E1|Es], Sep) ->
    iolist_to_binary([E1|[ [Sep,E] || E <- Es ]]);
concat_join([], _) -> <<>>.

%% insert - insert an element into a list shifting following elements.

insert([#tref{i=N},V], St0) ->
    Ts0 = St0#luerl.tabs,
    #table{a=Arr0}=T = ?GET_TABLE(N, Ts0),
    Arr1 = do_insert_last(Arr0, V),
    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
    {[],St0#luerl{tabs=Ts1}};
insert([#tref{i=N},P0,V]=As, St0) ->
    Ts0 = St0#luerl.tabs,
    #table{a=Arr0}=T = ?GET_TABLE(N, Ts0),
    case luerl_lib:to_int(P0) of
	nil -> lua_error({badarg,insert,As});
	P1 ->
	    Arr1 = do_insert(Arr0, P1, V),
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
	    {[],St0#luerl{tabs=Ts1}}
    end;
insert(As, _) -> lua_error({badarg,insert,As}).

%% Facit
%% t={} t={'aa','bb','cc',[6]='zz'} table.insert(t, 8, 'E')
%%  print(table.unpack(t,0,10))
%% -2  -1  0   1   2   3   4   5   6   7   8   9   10
%% nil nil nil aa  bb  cc  nil nil zz  nil E   nil nil
%% nil nil nil aa  bb  cc  nil nil zz  E   nil nil nil
%% nil nil nil aa  bb  cc  nil nil E   nil nil nil nil
%% nil nil nil aa  bb  cc  nil E   zz  nil nil nil nil
%% nil nil nil aa  bb  cc  E   nil zz  nil nil nil nil
%% nil nil nil aa  bb  E   cc  nil zz  nil nil nil nil
%% nil nil nil aa  E   bb  cc  nil zz  nil nil nil nil
%% nil nil nil E   aa  bb  cc  nil zz  nil nil nil nil
%% nil nil E   nil aa  bb  cc  nil zz  nil nil nil nil
%% nil E   nil nil aa  bb  cc  nil zz  nil nil nil nil

test_insert(T, V) -> do_insert_last(T, V).
test_insert(T, N, V) -> do_insert(T, N, V).

%% do_insert(Arr, N, V) -> Arr.
%% Don't ask, it tries to emulate the "real" Lua.

do_insert([{K,nil}|_], _, _) ->			%Shouldn't be a nil
    error({boom,K,nil});
do_insert([{N,_}|_]=Arr, N, V) ->		%Push it in here
    [{N,V}|insert_renum(Arr, N)];
do_insert([{K,_}|_]=Arr, N, V) when K > N ->	%Gap
    [{N,V}|insert_renum(Arr, N)];
do_insert([P|Arr], N, V) -> [P|do_insert(Arr, N, V)];
do_insert([], N, V) -> [{N,V}].

insert_renum([{K,nil}|_], _) ->			%Shouldn't be a nil
    error({boom,K,nil});
insert_renum([{N,V}|Arr], N) -> [{N+1,V}|insert_renum(Arr, N+1)];
insert_renum(Arr, _) -> Arr.			%Gap or end of list

do_insert_last(Arr, V) -> do_insert_last(Arr, 1, V).

do_insert_last([{K,nil}|_], _, _) ->		%Shouldn't be a nil
    error({boom,K,nil});
do_insert_last([{N,_}=P|Arr], N, V) ->
    [P|do_insert_last(Arr, N+1, V)];
do_insert_last(Arr, N, V) -> [{N,V}|Arr].	%Gap or end of list

%% pack - pack arguments in to a table.

pack(As, St0) ->
    T = pack_loop(As, 0.0),			%Indexes are floats!
    {Tab,St1} = luerl_eval:alloc_table(T, St0),
    {[Tab],St1}.

pack_loop([E|Es], N) ->				%In order for an orddict!
    [{N+1,E}|pack_loop(Es, N+1)];
pack_loop([], N) -> [{<<"n">>,N}].

%% remove - Remove an element from a list shifting following elements.

remove([#tref{i=N}], St0) ->
    Ts0 = St0#luerl.tabs,
    #table{a=Arr0}=T = ?GET_TABLE(N, Ts0),
    {Val,Arr1} = do_remove_last(Arr0),
    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
    {[Val],St0#luerl{tabs=Ts1}};
remove([#tref{i=N},P0]=As, St0) ->
    Ts0 = St0#luerl.tabs,
    #table{a=Arr0}=T = ?GET_TABLE(N, Ts0),
    case luerl_lib:to_int(P0) of
	nil -> lua_error({badarg,remove,As});
	P1 ->
	    {Val,Arr1} = do_remove(Arr0, P1),
	    Ts1 = ?SET_TABLE(N, T#table{a=Arr1}, Ts0),
	    {[Val],St0#luerl{tabs=Ts1}}
    end;
remove(As, _) -> lua_error({badarg,remove,As}).

do_remove_last(Arr) -> do_remove_last(Arr, 1, nil, []).

do_remove_last([{K,nil}|_], _, _, _) ->		%Shouldn't be a nil
    error({boom,K,nil});
do_remove_last([{N,V}=P|Arr], N, _, Acc) ->
    do_remove_last(Arr, N+1, V, [P|Acc]);
do_remove_last(Arr, _, V, [_|Acc]) ->		%Gap or end of list
    {V,lists:reverse(Acc, Arr)};
do_remove_last(Arr, _, V, []) -> {V,Arr}.

do_remove(Arr, N) when N < 1 -> {nil,Arr};
do_remove(Arr, N) -> do_remove(Arr, N, []).

do_remove([{N,V}|Arr], N, Acc) ->
    {V,lists:reverse(Acc, remove_renum(Arr, N))};
do_remove([{K,_}=P|Arr], N, Acc) when K < N ->
    do_remove(Arr, N, [P|Acc]);
do_remove(Arr, _, Acc) ->			%Gap or end of list
    {nil,lists:reverse(Acc, Arr)}.

remove_renum([{K,V}|Arr], N) when K =:= N+1 ->
    [{N,V}|remove_renum(Arr, N+1)];
remove_renum(Arr, _) -> Arr.			%Gap or end of list

%% unpack - unpack table into return values.

unpack([#tref{i=N}=T|As], St) ->
    #table{a=Arr} = ?GET_TABLE(N, St#luerl.tabs),
    case luerl_lib:to_ints(unpack_args(As)) of
	[I] ->
	    Unp = unpack_loop(Arr, I, length_loop(Arr, 1)),
	    %% io:fwrite("unp: ~p\n", [{Arr,I,Start,Unp}]),
	    {Unp,St};
	[I,J] ->
	    Unp = unpack_loop(Arr, I, J),
	    %% io:fwrite("unp: ~p\n", [{Arr,I,J,Start,Unp}]),
	    {Unp,St};
	_ -> lua_error({badarg,unpack,[T|As]})
    end;
unpack([], _) -> lua_error({badarg,unpack,[]}).

unpack_args([]) -> unpack_args([1.0]);		%Just start from the beginning
unpack_args([nil|As]) -> unpack_args([1.0|As]);
unpack_args([I]) -> [I];			%Only one argument
unpack_args([I,nil|_]) -> [I];			%Goto the default end
unpack_args([I,J|_]) -> [I,J].			%Two arguments

%% This and concat_loop are very similar.
unpack_loop(_, N, J) when N > J -> [];		%Done
unpack_loop([{N,V}|Tab], N, J) ->
    [V|unpack_loop(Tab, N+1, J)];
unpack_loop([{K,_}|_]=Tab, N, J) when K > N ->	%Gap
    [nil|unpack_loop(Tab, N+1, J)];
unpack_loop([{K,_}|Tab], N, J) when K < N ->
    unpack_loop(Tab, N, J);
unpack_loop([], N, J) -> [nil|unpack_loop([], N+1, J)].

%% sort - sort the elements of the list after their values.

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
    #table{a=Arr0}=T = ?GET_TABLE(N, St0#luerl.tabs),
    {Arr1,St1} = merge_sort(Sort, St0, Arr0),
    Arr2 = renumber(Arr1),
    %% io:fwrite("so: ~p\n", [{Arr0,Arr1,Arr2}]),
    Ts0 = St1#luerl.tabs,
    Ts1 = ?SET_TABLE(N, T#table{a=Arr2}, Ts0),
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
    {Tab1,_} = lists:mapfoldl(Fun, 1, Tab0),
    Tab1.

%% length(Stable, State) -> {Length,State}.
%%  The length of a table is the number of numeric keys in sequence
%%  from 1. Except if 1 is nil followed by non-nil. Don't ask!

length(#tref{i=N}=T, St) ->
    Meta = luerl_eval:getmetamethod(T, <<"__len">>, St),
    if ?IS_TRUE(Meta) -> luerl_eval:functioncall(Meta, [T], St);
       true ->
	    #table{a=Arr} = ?GET_TABLE(N, St#luerl.tabs),
	    {[float(length_loop(Arr))],St}
    end.

length_loop([{2,_}|Arr]) -> length_loop(Arr, 2);
length_loop(Arr) -> length_loop(Arr, 1).

length_loop([{K,_}|Arr], N) when K < N -> length_loop(Arr, N);
length_loop([{N,V}|Arr], N) when V =/= nil -> length_loop(Arr, N+1);
length_loop(_, N) -> N-1.			%Hit a nil or gap

%% drop_until([{K,_}|Tab], N) when K < N ->
%%     drop_until(Tab, N);
%% drop_until(Tab, _) -> Tab.

%% is_true(Rets) -> boolean().

is_true([nil|_]) -> false;
is_true([false|_]) -> false;
is_true([_|_]) -> true;
is_true([]) -> false.

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

%% merge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
%%     lists:reverse(fmerge2_1(T1, H2, Fun, T2, []), []);
%% merge(Fun, T1, []) when is_function(Fun, 2) ->
%%     T1.

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

%% rmerge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
%%     lists:reverse(rfmerge2_1(T1, H2, Fun, T2, []), []);
%% rmerge(Fun, T1, []) when is_function(Fun, 2) ->
%%     T1.

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
