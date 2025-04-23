%% Copyright (c) 2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : ttsets.erl
%% Author  : Robert Virding
%% Purpose : Set as a 2-3 tree.

%% This implementation uses 2-3 trees. The description of the tree
%% restructuring which is used comes from Prof. Lyn Turbak's notes for
%% CS230 Data Structures at Wellesley College.

-module(ttsets).

-include("luerl.hrl").

?MODULEDOC(false).

%% Standard interface.
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2,subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

%% Extended interface.
-export([foreach/2]).

-compile({no_auto_import,[size/1]}).		%We mean our own size/1

-ifdef(DEBUG).
-export([check_depth/1]).
-endif.

%% Data structure:
%% - {Left,Element,Right}
%% - {Left,Element,Middle,Element,Right}
%% - empty
%%
%% The term order is an arithmetic total order, so we should not
%% test exact equality for the keys. (If we do, then it becomes
%% possible that neither `>', `<', nor `=:=' matches.) Testing '<'
%% and '>' first is statistically better than testing for
%% equality, and also allows us to skip the test completely in the
%% remaining case.

-type ttset() :: empty |
		 {ttset(),any(),ttset()} |
		 {ttset(),any(),ttset(),any(),ttset()}.

-export_type([ttset/0]).

-spec new() -> Set::ttset().
%% Return a new empty set.

new() -> empty.					%The empty set

-spec is_set(Set::ttset()) -> boolean().
%% Return 'true' if Set is a set, else 'false'.

is_set(empty) -> true;
is_set({A,_,B}) ->
    is_set(A) andalso is_set(B);
is_set({A,_,B,_,C}) ->
    is_set(A) andalso is_set(B) andalso is_set(C);
is_set(_) -> false.

-spec size(Set::ttset()) -> non_neg_integer().
%% Return the number of elements in Set.

size(empty) -> 0;
size({A,_,B}) ->
    size(A) + size(B) + 1;
size({A,_,B,_,C}) ->
    size(A) + size(B) + size(C) + 2.

-spec to_list(Set::ttset()) -> [Element::any()].
%% Return the elements in Set as a list.

to_list(D) -> to_list(D, []).

to_list(empty, Tail) -> Tail;
to_list({A,X,B}, Tail) ->
    to_list(A, [X|to_list(B, Tail)]);
to_list({A,X,B,Y,C}, Tail) ->
    to_list(A, [X|to_list(B, [Y|to_list(C, Tail)])]).

-spec from_list([Element::any()]) -> Dict::ttset().
%% Build a set from the elements in list.

from_list(List) ->
    lists:foldl(fun (E, S) -> add_element(E, S) end, new(), List).

-spec is_element(Element::any(), Set::ttset()) -> boolean().
%% Return 'true' if Element is an element of Set, else 'false'.

is_element(_, empty) -> false;
is_element(E, {A,X,_}) when E < X -> is_element(E, A);
is_element(E, {_,X,B}) when E > X -> is_element(E, B);
is_element(_, {_,_,_}) -> true;
is_element(E, {A,X,_,_,_}) when E < X -> is_element(E, A);
is_element(E, {_,X,B,Y,C}) when E > X ->
    if E < Y -> is_element(E, B);		%Middle
       E > Y -> is_element(E, C);		%Right
       true -> true
    end;
is_element(_, {_,_,_,_,_}) -> true.

-spec add_element(Element::any(), Set::ttset()) -> Set::ttset().
%% Return Set with Element inserted in it.

add_element(E, T) ->
    %% Store and check for a returned "Up" node.
    case add_aux(E, T) of
	{up,Lu,Eu,Ru} -> {Lu,Eu,Ru};
	Node -> Node
    end.

add_aux(E, empty) -> {up,empty,E,empty};	%"Up" node
add_aux(E, {empty,X,empty}=N) ->
    %% Special case to avoid creating temporary "up" nodes.
    %% It helps a little bit, but not much.
    if E < X -> {empty,E,empty,X,empty};
       E > X -> {empty,X,empty,E,empty};
       true -> N
    end;
add_aux(E, {A,X,B}=N) ->
    if E < X ->					%Down the left
	    add_up2_l(add_aux(E, A), X, B);
       E > X ->					%Down the right
	    add_up2_r(A, X, add_aux(E, B));
       true -> N				%Replace current value
    end;
add_aux(E, {A,X,B,Y,C}) when E < X ->
    add_up3_l(add_aux(E, A), X, B, Y, C);
add_aux(E, {A,X,B,Y,C}=N) when E > X ->
    if E < Y ->					%Down the middle
	    add_up3_m(A, X, add_aux(E, B), Y, C);
       E > Y ->					%Down the right
	    add_up3_r(A, X, B, Y, add_aux(E, C));
       true -> N
    end;
add_aux(_, {_,_,_,_,_}=N) -> N.

%% add_up2_l/r(L, X, R) -> {L,X,M,X,R} | {L,X,R}.

add_up2_l({up,Lu,X,Ru}, Y, R) ->
    {Lu,X,Ru,Y,R};
add_up2_l(L, X, R) -> {L,X,R}.

add_up2_r(L, X, {up,Lu,Y,Ru}) ->
    {L,X,Lu,Y,Ru};
add_up2_r(L, X, R) -> {L,X,R}.

%% add_up3_l/m/r(L, X, M, Y, R) ->
%%     {up,L,X,R} | {L,X,M,Y,R}.

add_up3_l({up,Lu,X,Ru}, Y, M, Z, R) ->
    {up,{Lu,X,Ru},Y,{M,Z,R}};
add_up3_l(L, X, M, Y, R) -> {L,X,M,Y,R}.

add_up3_m(L, X, {up,Lu,Y,Ru}, Z, R) ->
    {up,{L,X,Lu},Y,{Ru,Z,R}};
add_up3_m(L, X, M, Y, R) -> {L,X,M,Y,R}.

add_up3_r(L, X, M, Y, {up,Lu,Z,Ru}) ->
    {up,{L,X,M},Y,{Lu,Z,Ru}};
add_up3_r(L, X, M, Y, R) -> {L,X,M,Y,R}.

-spec del_element(Element::any(), Set::ttset()) -> Set::ttset().
%% Return Set but with Element removed.

del_element(E, T) ->
    case del_aux(E, T) of
	{up,T1} -> T1;
	T1 -> T1
    end.

del_aux(_, empty) -> empty;			%No element
del_aux(E, {empty,X,empty}=N) ->
    if E < X; E > X -> N;			%No element
       true -> {up,empty}
    end;
del_aux(E, {A,X,B}) ->
    if E < X ->					%Down the left
	    del_up2_l(del_aux(E, A), X, B);
       E > X ->					%Down the right
	    del_up2_r(A, X, del_aux(E, B));
       true ->
	    {Bm,B1}= del_min(B),
	    del_up2_r(A, Bm, B1)
    end;
del_aux(E, {empty,X,empty,Y,empty}=N) ->
    if E < X -> N;				%No element
       E > X ->
	    if E < Y -> N;			%No element
	       E > Y -> N;
	       true -> {empty,X,empty}
	    end;
       true -> {empty,Y,empty}
    end;
del_aux(E, {A,X,B,Y,C}) when E < X ->
    del_up3_l(del_aux(E, A), X, B, Y, C);
del_aux(E, {A,X,B,Y,C}) when E > X ->
    if E < Y ->
	    del_up3_m(A, X, del_aux(E, B), Y, C);
       E > Y ->
	    del_up3_r(A, X, B, Y, del_aux(E, C));
       true ->
	    {Cm,C1} = del_min(C),
	    del_up3_r(A, X, B, Cm, C1)
    end;
del_aux(_, {A,_,B,Y,C}) ->
    {Bm,B1} = del_min(B),
    del_up3_m(A, Bm, B1, Y, C).

del_min(T) ->
    %%io:format("em: ~p\n->  ~p\n", [T,T1]),
    del_min1(T).

del_min1({empty,X,empty}) -> {X,{up,empty}};
del_min1({A,X,B}) ->
    {Min,A1} = del_min1(A),
    {Min,del_up2_l(A1, X, B)};
del_min1({empty,X,empty,Y,empty}) ->
    {X,{empty,Y,empty}};
del_min1({A,X,B,Y,C}) ->
    {Min,A1} = del_min1(A),
    {Min,del_up3_l(A1, X, B, Y, C)}.

%% del_up2_l/r(L, X, R) -> Node | {up,Node}.
%% We use the same naming of nodes and keys as in the text. It makes
%% checking the rules easier.

del_up2_l({up,L}, X, {M,Y,R}) ->		%1.1
    {up,{L,X,M,Y,R}};
del_up2_l({up,A}, X, {B,Y,C,Z,D}) ->		%2.1
    {{A,X,B},Y,{C,Z,D}};
del_up2_l(L, X, R) -> {L,X,R}.

del_up2_r({L,X,M}, Y, {up,R}) ->		%1.2
    {up,{L,X,M,Y,R}};
del_up2_r({A,X,B,Y,C}, Z, {up,D}) ->		%2.2
    {{A,X,B},Y,{C,Z,D}};
del_up2_r(L, X, R) -> {L,X,R}.

%% del_up2_r(L, X, {up,R}) -> del_up2_r1(L, X, R);
%% del_up2_r(L, X, R) -> {L,K,V,R}.

%% del_up2_r1({L,X,M}, Y, R) ->			%1.2
%%     {up,{L,X,M,Y,R}};
%% del_up2_r1({A,X,B,Y,C}, Z, D) ->		%2.2
%%     {{A,X,B},Y,{C,Z,D}}.

%% del_up3_l/m/r(L, X, M, Y, R) -> Node | {up,Node}.
%%  We use the same naming of nodes and keys as in the text. It makes
%%  checking the rules easier. N.B. there are alternate valid choices
%%  for the middle case!

del_up3_l({up,A}, X, {B,Y,C}, Z, D) ->		%3a.1
    {{A,X,B,Y,C},Z,D};
del_up3_l({up,A}, W, {B,X,C,Y,D}, Z, E) ->	%4a.1
    {{A,W,B},X,{C,Y,D},Z,E};
del_up3_l(A, X, B, Y, C) -> {A,X,B,Y,C}.

del_up3_m({A,X,B}, Y, {up,C}, Z, D) ->		%3a.2
    {{A,X,B,Y,C},Z,D};
del_up3_m(A, X, {up,B}, Y, {C,Z,D}) ->		%3b.1
    {A,X,{B,Y,C,Z,D}};
del_up3_m({A,W,B,X,C}, Y, {up,D}, Z, E) ->	%4a.2
    {{A,W,B},X,{C,Y,D},Z,E};
del_up3_m(A, W, {up,B}, X, {C,Y,D,Z,E}) ->	%4b.1
    {A,W,{B,X,C},Y,{D,Z,E}};
del_up3_m(A, X, B, Y, C) -> {A,X,B,Y,C}.

del_up3_r(A, X, {B,Y,C}, Z, {up,D}) ->		%3b.2
    {A,X,{B,Y,C,Z,D}};
del_up3_r(A, W, {B,X,C,Y,D}, Z, {up,E}) ->	%4b.2
    {A,W,{B,X,C},Y,{D,Z,E}};
del_up3_r(A, X, B, Y, C) -> {A,X,B,Y,C}.

-spec union(Set1::ttset(), Set2::ttset()) -> Set::ttset().
%% Return the union of Set1 and Set2.

union(S1, S2) ->
    fold(fun (E, S) -> add_element(E, S) end, S2, S1).

-spec union(Sets::[ttset()]) -> Set::ttset().
%% Return the union of the list of sets.

union([S1,S2|Ss]) ->
    %% Do our own unions here to try and fold over smaller set.
    U0 = union(Ss),
    U1 = fold(fun (E, S) -> add_element(E, S) end, U0, S2),
    fold(fun (E, S) -> add_element(E, S) end, U1, S1);
union([S]) -> S;
union([]) -> empty.

-spec intersection(Set1::ttset(), Set2::ttset()) -> Set::ttset().
%% Return the intersection of Set1 and Set2.

intersection(S1, S2) ->
    filter(fun (E) -> is_element(E, S1) end, S2).

-spec intersection(Sets::[ttset()]) -> Set::ttset().
%% Return the intersection of the list of sets.

intersection([S]) -> S;
intersection([S|Ss]) ->
    lists:foldl(fun (S1, I) -> intersection(S1, I) end, S, Ss).

-spec is_disjoint(Set1::ttset(), Set2::ttset()) -> boolean().
%% Check whether Set1 and Set2 are disjoint.

is_disjoint(S1, S2) ->
    fold(fun (E, Dis) -> Dis andalso (not is_element(E, S2)) end, true, S1).

-spec subtract(Set1::ttset(), Set2::ttset()) -> Set::ttset().
%% Return all and only the elements in Set1 which are not elements of Set2.

subtract(S1, S2) ->
    filter(fun (E) -> not is_element(E, S2) end, S1).

-spec is_subset(Set1::ttset(), Set2::ttset()) -> boolean().
%% Return 'true' when every element of Set1 is also an element of
%% Set2, else 'false'.

is_subset(S1, S2) ->
    fold(fun (E, Sub) -> Sub andalso is_element(E, S2) end, true, S1).

-spec fold(Fun::fun(), Acc::any(), Set::ttset()) -> any().
%%  Apply Fun to each element in Set. Do it left to right, even if
%%  this is not specified.

fold(_, Acc, empty) -> Acc;
fold(F, Acc0, {A,X,B}) ->
    Acc1 = F(X, fold(F, Acc0, A)),
    fold(F, Acc1, B);
fold(F, Acc0, {A,X,B,Y,C}) ->
    Acc1 = F(X, fold(F, Acc0, A)),
    Acc2 = F(Y, fold(F, Acc1, B)),
    fold(F, Acc2, C).

-spec filter(Fun::fun(), Set::ttset()) -> Set::ttset().
%%  Apply Fun to each element in Dict. Do it left to right, even if
%%  this is not specified.

filter(F, S) -> filter(F, S, new()).

filter(_, empty, New) -> New;
filter(F, {A,X,B}, New0) ->
    New1 = filter(F, A, New0),
    New2 = case F(X) of
	       true -> add_element(X, New1);
	       false -> New1
	   end,
    filter(F, B, New2);
filter(F, {A,X,B,Y,C}, New0) ->
    New1 = filter(F, A, New0),
    New2 = case F(X) of
	       true -> add_element(X, New1);
	       false -> New1
	   end,
    New3 = filter(F, B, New2),
    New4 = case F(Y) of
	       true -> add_element(Y, New3);
	       false -> New3
	   end,
    filter(F, C, New4).

%% Extended interface.

-spec foreach(Fun::fun(), Set::ttset()) -> ok.
%%  Apply Fun to each element in Set. Do it left to right, even if
%%  this is not specified.

foreach(_, empty) -> ok;
foreach(F, {A,X,B}) ->
    foreach(F, A),
    F(X),
    foreach(F, B);
foreach(F, {A,X,B,Y,C}) ->
    foreach(F, A),
    F(X),
    foreach(F, B),
    F(Y),
    foreach(F, C).

-ifdef(DEBUG).

%% Check the depth of all the leaves, should all be the same.
check_depth(T) -> check_depth(T, 1, orddict:new()).

check_depth(empty, D, Dd) ->
    orddict:update_counter(D, 1, Dd);
check_depth({L,_,R}, D, Dd0) ->
    Dd1 = orddict:update_counter(two, 1, Dd0),
    Dd2 = check_depth(L, D+1, Dd1),
    check_depth(R, D+1, Dd2);
check_depth({L,_,M,_,R}, D, Dd0) ->
    Dd1 = orddict:update_counter(three, 1, Dd0),
    Dd2 = check_depth(L, D+1, Dd1),
    Dd3 = check_depth(M, D+1, Dd2),
    check_depth(R, D+1, Dd3).

-endif.
