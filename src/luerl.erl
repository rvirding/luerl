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

-export([eval/1,eval/2,evalfile/1,evalfile/2,
        do/1,do/2,dofile/1,dofile/2,
        load/1,loadfile/1,
        compile/1,compilefile/1,
        call/1,call/2,
        start/0,stop/1,gc/1]).

%% luerl:eval(String|Binary|Form[, State]) -> Result.
eval(Chunk) ->
    eval(Chunk, luerl_eval:init()).

eval(Chunk, St) ->
    try do(Chunk, St) of
        {Ret,_} -> {ok,Ret}
    catch 
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.
    
%% luerl:evalfile(Path[, State]) -> {ok, Result} | {error,Reason}.
evalfile(Path) ->
    evalfile(Path, luerl_eval:init()).

evalfile(Path, St) ->
    try dofile(Path, St) of
        {Ret,_} -> {ok,Ret}
    catch 
         _E:R -> {error, R} % {error, {E, R}} ? <- todo: decide
    end.

%% luerl:do(String|Binary|Form[, State]) -> {Result, NewState} 
do(SBC) ->
    do(SBC, luerl_eval:init()).

do({functiondef,_,_,_}=C, St) ->
    luerl_eval:funchunk(C, St);

do({functiondef,_,_,_,_}=C, St) ->
    luerl_eval:funchunk(C, St);

do(B, St) when is_binary(B) ->
    do(binary_to_list(B), St);

do(S, St) when is_list(S) ->
    {ok,Ts,_} = luerl_scan:string(S),
    {ok,C} = luerl_parse:chunk(Ts),
    luerl_eval:funchunk(C, St).

%% luerl:dofile(Path[, State]) -> {Result, NewState}.
dofile(Path) ->
    dofile(Path, luerl_eval:init()).

dofile(Path, St) ->
    {ok,Bin} = file:read_file(Path),
    {ok,Ts,_} = luerl_scan:string(binary_to_list(Bin)),
    {ok,C} = luerl_parse:chunk(Ts),
    luerl_eval:funchunk(C, St).

%% load(String|Binary) -> {ok,Form}.
load(Chunk) when is_binary(Chunk) ->
    load(binary_to_list(Chunk));

load(Chunk) when is_list(Chunk) ->
    {ok,Ts,_} = luerl_scan:string(Chunk),
    luerl_parse:chunk(Ts).

%% compilefile(Path) -> {ok,Form}.
loadfile(Path) ->
    {ok,Bin} = file:read_file(Path),
    {ok,Ts,_} = luerl_scan:string(binary_to_list(Bin)),
    luerl_parse:chunk(Ts).

%% compile(String|Binary) -> {ok,Form} | {error,Reason}.
compile(Chunk) when is_binary(Chunk) ->
    compile(binary_to_list(Chunk));

compile(Chunk) when is_list(Chunk) ->
    {ok,Ts,_} = luerl_scan:string(Chunk),
    luerl_parse:chunk(Ts).

%% compilefile(Path) -> {ok,Form} | {error,Reason}.
compilefile(Path) ->
    {ok,Bin} = file:read_file(Path),
    {ok,Ts,_} = luerl_scan:string(binary_to_list(Bin)),
    luerl_parse:chunk(Ts).

%% start() -> State.
start() -> 
    luerl_eval:init().

%% call(Form[, State][, ErlParam]) -> {Result,State}
call(C) ->
    call(C, luerl_eval:init(), []).

call(C, St) ->
    call(C, St, []).

call({functiondef,_,_,_}=C, St, P) ->
    luerl_eval:funchunk(C, St, P);

call({functiondef,_,_,_,_}=C, St, P) ->
    luerl_eval:funchunk(C, St, P).

%% stop(State) -> GCedState.
stop(St) -> 
    luerl_eval:gc(St).

%% gc(State) -> State.
gc(St) -> luerl_eval:gc(St).

%% luerl:encode(list()) -> LuerlTermsList().
% luerl:encode(list()) -> LuerlTermsList().

%% luerl:decode(LuerlTermsList()) -> list().
% decode(LuerlTermsList) ->
%    list().