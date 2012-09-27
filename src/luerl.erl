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
        call/2,call/3,
        init/0,stop/1,gc/1,decode/2,encode/2]).

%% luerl:eval(String|Binary|Form[, State]) -> Result.
eval(Chunk) ->
    eval(Chunk, init()).

eval(Chunk, St) ->
    try do(Chunk, St) of
        {Ret,_} -> {ok,Ret}
    catch 
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.
    
%% luerl:evalfile(Path[, State]) -> {ok, Result} | {error,Reason}.
evalfile(Path) ->
    evalfile(Path, init()).

evalfile(Path, St) ->
    try dofile(Path, St) of
        {Ret,_} -> {ok,Ret}
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
    luerl_emul:chunk(C, [], St);
do(C, St) ->					%Pre-parsed/compiled chunk
    luerl_emul:chunk(C, [], St).

%% luerl:dofile(Path[, State]) -> {Result, NewState}.
dofile(Path) ->
    dofile(Path, init()).

dofile(Path, St) ->
    {ok,C} = loadfile(Path),
    luerl_emul:chunk(C, [], St).

%% load(String|Binary) -> {ok,Form}.
load(Bin) when is_binary(Bin) ->
    load(binary_to_list(Bin));
load(Str) when is_list(Str) ->
    do_passes(comp_passes(), Str).

%% loadfile(Path) -> {ok,Form}.
loadfile(Path) ->
    {ok,Bin} = file:read_file(Path),
    do_passes(comp_passes(), binary_to_list(Bin)).

do_passes([Fun|Funs], St0) ->
    case Fun(St0) of
	{ok,St1} -> do_passes(Funs, St1);
	Error -> Error
    end;
do_passes([], St) -> {ok,St}.

comp_passes() ->
    [fun (S) ->
	     %% Make return values "conformant".
	     case luerl_scan:string(S) of
		 {ok,Ts,_} -> {ok,Ts};
		 {error,Error,_} -> {error,Error}
	     end
     end,
     fun (Ts) -> luerl_parse:chunk(Ts) end,
     fun (Chunk) -> luerl_comp:chunk(Chunk) end].

%% init() -> State.
init() -> luerl_emul:init().

%% call(Form, Terms, State) -> {Result,State}

call(C, Ts) -> call(C, Ts, init()).

call(C, Ts, St0) ->
    {Lts,St1} = encode_list(Ts, St0),
    {Lrs,St2} = luerl_emul:chunk(C, Lts, St1),
    Rs = decode_list(Lrs, St2),
    {Rs,St2}.

%% stop(State) -> GCedState.
stop(St) -> 
    luerl_emul:gc(St).

%% gc(State) -> State.
gc(St) -> luerl_emul:gc(St).

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
    {T,St2} = luerl_emul:alloc_table(Es, St1),
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
	    Kfun = fun (K, V) -> {decode(K, St),decode(V, St)} end,
	    Afun = fun (K, V, Acc) -> [Kfun(K, V)|Acc] end,
	    [ Kfun(K, V) || {K,V} <- Tab ] ++
		array:sparse_foldr(Afun, [], Arr);
	_Undefined -> error(badarg)
    end;
decode({function,Fun}, _) -> {function,Fun};
decode(_, _) -> error(badarg).			%Shouldn't have anything else
