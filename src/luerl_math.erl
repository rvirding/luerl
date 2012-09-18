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

-export([install/1,fmod/2,frexp/2]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

install(St) ->
    luerl_eval:alloc_table(table(), St).

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
     {<<"fmod">>,{function,fun fmod/2}},
     {<<"frexp">>,{function,fun frexp/2}},
     {<<"huge">>,1.7976931348623157e308},	%From the specs
     {<<"ldexp">>,{function,fun ldexp/2}},
     {<<"log">>,{function,fun log/2}},
     {<<"log10">>,{function,fun log10/2}},	%For 5.1 backwards compatibility
     {<<"max">>,{function,fun max/2}},
     {<<"min">>,{function,fun min/2}},
     {<<"modf">>,{function,fun modf/2}},
     {<<"pi">>,math:pi()},
     {<<"pow">>,{function,fun pow/2}},
     {<<"rad">>,{function,fun rad/2}},
     {<<"random">>,{function,fun random/2}},
     {<<"randomseed">>,{function,fun randomseed/2}},
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
	_ -> lua_error({badarg,abs,As})
    end.

acos(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:acos(N)],St};
	nil -> lua_error({badarg,acos,As})
    end.

asin(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:asin(N)],St};
	_ -> lua_error({badarg,asin,As})
    end.

atan(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:atan(N)],St};
	_ -> lua_error({badarg,atan,As})
    end.

atan2(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N1,N2|_] -> {[math:atan2(N1, N2)],St};
	_ -> lua_error({badarg,atan2,As})
    end.

ceil(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[float(round(N + 0.5))],St};
	_ -> lua_error({badarg,ceil,As})
    end.

cos(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:cos(N)],St};
	_ -> lua_error({badarg,cos,As})
    end.

cosh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:cosh(N)],St};
	_ -> lua_error({badarg,cosh,As})
    end.

deg(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[180.0*N/math:pi()],St};
	_ -> lua_error({badarg,deg,As})
    end.

exp(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:exp(N)],St};
	_ -> lua_error({badarg,exp,As})
    end.

floor(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[float(round(N - 0.5))],St};
	_ -> lua_error({badarg,floor,As})
    end.

fmod(As, St) ->
    case luerl_lib:tonumbers(As) of
	[X,Y|_] ->
	    Div = float(trunc(X/Y)),
	    Rem = X - Div*Y,
	    {[Rem],St};
	_ -> lua_error({badarg,fmod,As})
    end.

frexp(As, St) ->				%M,E such that X = M*2^E
    case luerl_lib:tonumbers(As) of
	[X|_] ->
	    <<_:1,E0:11,M0:52>> = <<X/float>>,	%The sneaky bit!
	    Two52 = 1 bsl 52,
	    M1 = (M0 bor Two52)/Two52,
	    if M1 >= 1.0 -> M2 = M1/2, E1 = E0 - 1022; %Export M2, E1
	       M1 < 0.5 -> M2 = M1*2.0, E1 = E0 - 1024;
	       true -> M2 = M1, E1 = E0 - 1023
	    end,
	    {[float(M2),float(E1)],St};
	_ -> lua_error({badarg,frexp,As})
    end.

ldexp(As, St) ->
    case luerl_lib:conv_list(As, [lua_number,lua_integer]) of
	[M,E] ->
	    {[M*math:pow(2, E)],St};
%% 	    <<X/float>> = <<0:1,E:11,M:52>>,
%% 	    {[X],St};
	_ -> lua_error({badarg,ldexp,As})
    end.

log(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N] -> {[math:log(N)],St};
	[N,10.0|_] -> {[math:log10(N)],St};	%Seeing it is builtin
	[N1,N2|_] ->
	    {[math:log(N1)/math:log(N2)],St};
	_ -> lua_error({badarg,log,As})
    end.

log10(As, St) ->				%For 5.1 backwards compatibility
    case luerl_lib:tonumbers(As) of
	[0.0|_] -> {[-500.0],St};		%Bit hacky
	[N|_] -> {[math:log10(N)],St};
	_ -> lua_error({badarg,log10,As})
    end.

max(As, St) ->
    case luerl_lib:tonumbers(As) of
	[_|_]=Ns -> {[lists:max(Ns)],St};
	_ -> lua_error({badarg,max,As})
    end.

min(As, St) ->
    case luerl_lib:tonumbers(As) of
	[_|_]=Ns -> {[lists:min(Ns)],St};
	_ -> lua_error({badarg,min,As})
    end.

modf(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] ->
	    I = float(trunc(N)),		%Integral part
	    {[I,N-I],St};
	_ -> lua_error({badarg,modf,As})
    end.

pow(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N1,N2|_] -> {[math:pow(N1, N2)],St};
	_ -> lua_error({badarg,pow,As})
    end.

rad(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:pi()*N/180.0],St};
	_ -> lua_error({badarg,sinh,As})
    end.

random(As, St) ->
    case luerl_lib:to_ints(As) of
	[] -> {[random:uniform()],St};		%0-1.0
	[M] when M > 1 ->
	    R = random:uniform(M),
	    {[float(R)],St};
	[M,N] when N > M ->
	    R = random:uniform(N - M),
	    {[float(R + M)],St};
	_ -> lua_error({badarg,random,As})
    end.

randomseed(As, St) ->
    case luerl_lib:tonumbers(As) of
	[S|_] ->
	    %% Split float-64 into three integers.
	    <<A1:24,A2:24,A3:16>> = <<S/float>>,
	    random:seed(A1, A2, A3),
	    {[],St};
	_ -> lua_error({badarg,randomseed,As})
    end.

sin(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sin(N)],St};
	_ -> lua_error({badarg,sin,As})
    end.

sinh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sinh(N)],St};
	_ -> lua_error({badarg,sinh,As})
    end.

sqrt(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:sqrt(N)],St};
	_ -> lua_error({badarg,sqrt,As})
    end.

tan(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:tan(N)],St};
	_ -> lua_error({badarg,tan,As})
    end.

tanh(As, St) ->
    case luerl_lib:tonumbers(As) of
	[N|_] -> {[math:tanh(N)],St};
	_ -> lua_error({badarg,tanh,As})
    end.
