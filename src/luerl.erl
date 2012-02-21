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
%% Author  : Robert Virding
%% Purpose : Basic LUA 5.2 interface.

-module(luerl).

-export([eval/1,dofile/1,do/2,dochunk/2,ps/1,init/0,gc/1]).

eval(S) ->
    {ok,Ts,_} = luerl_scan:string(S),
    {ok,C} = luerl_parse:chunk(Ts),
    {Ret,_} = luerl_eval:chunk(C, luerl_eval:init()),
    Ret.

dofile(File) ->
    {ok,Bin} = file:read_file(File),
    {ok,Ts,_} = luerl_scan:string(binary_to_list(Bin)),
    {ok,C} = luerl_parse:chunk(Ts),
    {Ret,_} = luerl_eval:chunk(C, luerl_eval:init()),
    Ret.

%% ps(String) -> {ok,ChunkCode}.
%% init() -> State.
%% do(String, State) -> {Res,State}.
%% dochunk(Chunk, State) -> {Res,State}.
%% gc(State) -> State.
%% Some testing utilities.

ps(S) ->
    {ok,Ts,_} = luerl_scan:string(S),
    luerl_parse:chunk(Ts).

init() -> luerl_eval:init().

do(S, St) ->
    {ok,C} = ps(S),
    luerl_eval:chunk(C, St).

dochunk(C, St) ->
    luerl_eval:chunk(C, St).

gc(St) -> luerl_eval:gc(St).
