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

%% File    : luerl_math.erl
%% Author  : Robert Virding
%% Purpose : The math library for Luerl.

-module(luerl_math).

-export([table/0]).

table() ->
    [{<<"abs">>,{function,fun abs/2}},
     {<<"acos">>,{function,fun acos/2}},
     {<<"asin">>,{function,fun asin/2}},
     {<<"atan">>,{function,fun atan/2}},
     {<<"atan2">>,{function,fun atan2/2}},
     {<<"cos">>,{function,fun cos/2}},
     {<<"cosh">>,{function,fun cosh/2}},
     {<<"deg">>,{function,fun deg/2}},
     {<<"exp">>,{function,fun exp/2}},
     {<<"floor">>,{function,fun floor/2}},
     {<<"log">>,{function,fun log/2}},
     {<<"pi">>,math:pi()},
     {<<"pow">>,{function,fun pow/2}},
     {<<"rad">>,{function,fun rad/2}},
     {<<"sin">>,{function,fun sin/2}},
     {<<"sinh">>,{function,fun sinh/2}},
     {<<"sqrt">>,{function,fun sqrt/2}},
     {<<"tan">>,{function,fun tan/2}},
     {<<"tanh">>,{function,fun tanh/2}}
    ].

%% abs(Args, State) -> {[Ret],State}.

abs([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[abs(N)],St}.

acos([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:acos(N)],St}.

asin([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:asin(N)],St}.

atan([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:atan(N)],St}.

atan2([A1,A2|_], St) ->
    N1 = luerl_lib:tonumber(A1),
    N2 = luerl_lib:tonumber(A2),
    {[math:atan2(N1, N2)],St}.

cos([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:cos(N)],St}.

cosh([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:cosh(N)],St}.

deg([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[180.0*N/math:pi()],St}.

exp([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:exp(N)],St}.

floor([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[float(round(N - 0.5))],St}.

log([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:log(N)],St}.

pow([A1,A2|_], St) ->
    N1 = luerl_lib:tonumber(A1),
    N2 = luerl_lib:tonumber(A2),
    {[math:pow(N1, N2)],St}.

rad([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:pi()*N/180.0],St}.

sin([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:sin(N)],St}.

sinh([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:sinh(N)],St}.

sqrt([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:sqrt(N)],St}.

tan([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:tan(N)],St}.

tanh([A|_], St) ->
    N = luerl_lib:tonumber(A),
    {[math:tanh(N)],St}.
