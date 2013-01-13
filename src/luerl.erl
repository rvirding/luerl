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

%% File    : luerl.erl
%% Authors : Robert Virding, Henning Diedrich
%% Purpose : Basic LUA 5.2 interface.

-module(luerl).

-include("luerl.hrl").

-export([eval/1,eval/2,evalfile/1,evalfile/2,
	 do/1,do/2,dofile/1,dofile/2,
	 load/1,loadfile/1,
	 call/2,call/3,call1/3,call_list/2,
	 method/2,method/3,method1/3,method_list/2,
	 init/0,stop/1,gc/1,
	 encode/2,encode_list/2,decode/2,decode_list/2]).

%% luerl:eval(String|Binary|Form[, State]) -> Result.
eval(Chunk) ->
    eval(Chunk, init()).

eval(Chunk, St0) ->
    try do(Chunk, St0) of
        {Ret,St1} -> {ok, decode_list(Ret, St1)}
    catch
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.

%% luerl:evalfile(Path[, State]) -> {ok, Result} | {error,Reason}.
evalfile(Path) ->
    evalfile(Path, init()).

evalfile(Path, St0) ->
    try dofile(Path, St0) of
        {Ret,St1} -> {ok, decode_list(Ret, St1)}
    catch
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.

%% luerl:do(String|Binary|Form[, State]) -> {Result, NewState}
do(SBC) ->
    do(SBC, init()).

do(B, St) when is_binary(B) ->
    do(binary_to_list(B), St);
do(S, St) when is_list(S) ->
    {ok,C} = load(S),
    luerl_eval:chunk(C, [], St);
do(C, St) ->					%Pre-parsed/compiled chunk
    luerl_eval:chunk(C, [], St).

%% luerl:dofile(Path[, State]) -> {Result, NewState}.
dofile(Path) ->
    dofile(Path, init()).

dofile(Path, St) ->
    {ok,C} = loadfile(Path),
    luerl_eval:chunk(C, [], St).

%% load(String|Binary) -> {ok,Form}.
load(Chunk) when is_binary(Chunk) ->
    load(binary_to_list(Chunk));
load(Chunk) when is_list(Chunk) ->
    {ok,Ts,_} = luerl_scan:string(Chunk),
    luerl_parse:chunk(Ts).

%% loadfile(Path) -> {ok,Form}.
loadfile(Path) ->
    {ok,Bin} = file:read_file(Path),
    {ok,Ts,_} = luerl_scan:string(binary_to_list(Bin)),
    luerl_parse:chunk(Ts).

%% init() -> State.
init() -> luerl_eval:init().

%% call(FuncPath, Args, State) -> {Result,State}.
%% call1(FuncPath | Func, LuaArgs, State) -> {LuaResult,State}.

call(Fp, As) -> call(Fp, As, init()).		%Use default state

call(Fp, As, St0) ->
    %% Encode the input arguments.
    {Lfp,St1} = encode_list(Fp, St0),
    {Las,St2} = encode_list(As, St1),
    %% Find the function definition and call function.
    {F,St3} = call_list(Lfp, St2),
    {Lrs,St4} = luerl_eval:functioncall(F, Las, St3),
    Rs = decode_list(Lrs, St4),
    {Rs,St4}.

call1(Fp, Las, St0) when is_list(Fp) ->
    {F,St1} = call_list(Fp, St0),
    luerl_eval:functioncall(F, Las, St1);
call1(F, Las, St) ->
    luerl_eval:functioncall(F, Las, St).

%% call_list(Keys, State) -> {V,State}.
%%  Go down a list of keys and return final value.

call_list([G|Kl], St0) ->
    First = luerl_eval:get_env_key(G, St0),	%Start at global env
    Fun = fun (K, {T,Sta}) ->
		  {Vs,Stb} = luerl_eval:get_table_key(T, K, Sta),
		  {luerl_lib:first_value(Vs),Stb}
	  end,
    lists:foldl(Fun, {First,St0}, Kl);
call_list(_, _) -> error(badarg).

%% method(FuncPath, Args, State) -> {Result,State}.
%% method1(FuncPath | FuncPath, Args, State) -> {Result,State}.

method(Fp, As) -> method(Fp, As, init()).

method(Fp, As, St0) ->
    %% Encode the input arguments.
    {Lfp,St1} = encode_list(Fp, St0),
    {Las,St2} = encode_list(As, St1),
    %% Find the object and method definition and call method.
    {O,M,St3} = method_list(Lfp, St2),
    {Lrs,St4} = luerl_eval:functioncall(M, [O|Las], St3),
    Rs = decode_list(Lrs, St4),
    {Rs,St4}.

method1(Fp, Las, St0) ->
    %% Find the object and method definition and call method.
    {O,M,St1} = method_list(Fp, St0),
    luerl_eval:functioncall(M, [O|Las], St1).

method_list([G|Ks], St) ->
    First = luerl_eval:get_env_key(G, St),
    method_list(Ks, First, St).

method_list([K], SoFar, St0) ->
    {Func,St1} = luerl_eval:get_table_key(SoFar, K, St0),
    {SoFar,luerl_lib:first_value(Func),St1};
method_list([K|Ks], SoFar, St0) ->
    {Next,St1} = luerl_eval:get_table_key(SoFar, K, St0),
    method_list(Ks, luerl_lib:first_value(Next), St1);
method_list(_, _, _) -> error(badarg).

%% stop(State) -> GCedState.
stop(St) ->
    luerl_eval:gc(St).

%% gc(State) -> State.
gc(St) -> luerl_eval:gc(St).

%% encode_list([Term], State) -> {[LuerlTerm],State}.
%% encode(Term, State) -> {LuerlTerm,State}.

encode_list(Ts, St) ->
    lists:mapfoldl(fun encode/2, St, Ts).

encode(B, St) when is_binary(B) -> {B,St};
encode(A, St) when is_atom(A) -> {atom_to_binary(A, latin1),St};
encode(I, St) when is_integer(I) -> {float(I),St};
encode(F, St) when is_float(F) -> {F,St};
encode(B, St) when is_boolean(B) -> {B,St};
encode(nil, St) -> {nil,St};
encode(L, St0) when is_list(L) ->
    {Es,{_,St1}} = lists:mapfoldl(fun ({K0,V0}, {I,S0}) ->
					  {K1,S1} = encode(K0, S0),
					  {V1,S2} = encode(V0, S1),
					  {{K1,V1},{I,S2}};
				      (V0, {I,S0}) ->
					  {V1,S1} = encode(V0, S0),
					  {{I,V1},{I+1,S1}}
			      end, {1.0,St0}, L),
    {T,St2} = luerl_eval:alloc_table(Es, St1),
    {T,St2};					%No more to do for now
encode(_, _) -> error(badarg).			%Can't encode anything else

%% decode_list([LuerlTerm], State) -> [Term].
%% decode(LuerlTerm, State) -> Term.

decode_list(Lts, St) ->
    lists:map(fun (Lt) -> decode(Lt, St) end, Lts).

decode(B, _) when is_binary(B) -> B;
decode(N, _) when is_number(N) -> N;
decode(B, _) when is_boolean(B) -> B;
decode(nil, _) -> nil;
decode(#tref{i=N}, St) ->
    case ?GET_TABLE(N, St#luerl.tabs) of
	#table{a=Arr,t=Tab} ->
	    Fun = fun (K, V, Acc) -> [{decode(K, St),decode(V, St)}|Acc] end,
	    Ts = ttdict:fold(Fun, [], Tab),
	    array:sparse_foldr(Fun, Ts, Arr);
	_Undefined -> error(badarg)
    end;
decode({function,Fun}, _) -> {function,Fun};
decode(_, _) -> error(badarg).			%Shouldn't have anything else
