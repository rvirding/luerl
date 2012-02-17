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
     {<<"ceil">>,{function,fun ceil/2}},
     {<<"cos">>,{function,fun cos/2}},
     {<<"cosh">>,{function,fun cosh/2}},
     {<<"deg">>,{function,fun deg/2}},
     {<<"exp">>,{function,fun exp/2}},
     {<<"floor">>,{function,fun floor/2}},
     {<<"log">>,{function,fun log/2}},
     {<<"max">>,{function,fun max/2}},
     {<<"min">>,{function,fun min/2}},
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

abs(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[abs(N)],St};
	_ -> error({badarg,abs,As})
    end.

acos(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:acos(N)],St};
	nil -> error({badarg,acos,As})
    end.

asin(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:asin(N)],St};
	_ -> error({badarg,asin,As})
    end.

atan(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:atan(N)],St};
	_ -> error({badarg,atan,As})
    end.

atan2(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N1,N2|_] -> {[math:atan2(N1, N2)],St};
	_ -> error({badarg,atan2,As})
    end.

ceil(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[float(round(N + 0.5))],St};
	_ -> error({badarg,ceil,As})
    end.

cos(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:cos(N)],St};
	_ -> error({badarg,cos,As})
    end.

cosh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:cosh(N)],St};
	_ -> error({badarg,cosh,As})
    end.

deg(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[180.0*N/math:pi()],St};
	_ -> error({badarg,deg,As})
    end.

exp(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:exp(N)],St};
	_ -> error({badarg,exp,As})
    end.

floor(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[float(round(N - 0.5))],St};
	_ -> error({badarg,floor,As})
    end.

log(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N] -> {[math:log(N)],St};
	[N1,N2|_] ->
	    {[math:log(N1)/math:log(N2)],St};
	_ -> error({badarg,log,As})
    end.

max(As, St) ->
    case luerl_lib:tonumbers(As) of
	[_|_]=Ns -> {[lists:max(Ns)],St};
	_ -> error({badarg,max,As})
    end.

min(As, St) ->
    case luerl_lib:tonumbers(As) of
	[_|_]=Ns -> {[lists:min(Ns)],St};
	_ -> error({badarg,min,As})
    end.

pow(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N1,N2|_] -> {[math:pow(N1, N2)],St};
	_ -> error({badarg,pow,As})
    end.

rad(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:pi()*N/180.0],St};
	_ -> error({badarg,sinh,As})
    end.

sin(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sin(N)],St};
	_ -> error({badarg,sin,As})
    end.

sinh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sinh(N)],St};
	_ -> error({badarg,sinh,As})
    end.

sqrt(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sqrt(N)],St};
	_ -> error({badarg,sqrt,As})
    end.

tan(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:tan(N)],St};
	_ -> error({badarg,tan,As})
    end.

tanh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:tanh(N)],St};
	_ -> error({badarg,tanh,As})
    end.
