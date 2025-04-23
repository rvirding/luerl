%% Copyright (c) 2013-2020 Robert Virding
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

%% File    : luerl_lib_math.erl
%% Author  : Robert Virding
%% Purpose : The math library for Luerl.

%% We try to mirror the handling of arguments which occurs in the Lua
%% math module. Many functions allow extra arguments but only look at
%% the first required ones of the right type and completely ignore the
%% rest.
%%
%% We keep atan2, cosh, sinh tanh, pow, frexp and ldexp even though
%% have been deprecated.

-module(luerl_lib_math).

-include("luerl.hrl").

?MODULEDOC(false).

-export([install/1,abs/3,acos/3,asin/3,atan2/3,atan/3,ceil/3,cos/3,cosh/3,deg/3,exp/3,floor/3,
         fmod/3,frexp/3,ldexp/3,log/3,log10/3,max/3,min/3,modf/3,pow/3,rad/3,random/3,randomseed/3,
         sin/3,sinh/3,sqrt/3,tan/3,tanh/3,tointeger/3,type/3]).

-export([internalize/1,externalize/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).	%Shorten this


%% Use the correct random number module.

-ifdef(NEW_RAND).
-define(RAND_UNIFORM(S), rand:uniform_s(S)).
-define(RAND_UNIFORM(L, S), rand:uniform_s(L, S)).
-define(RAND_SEED(), rand:seed_s(exs1024)).
-define(RAND_SEED(S1,S2,S3), rand:seed_s(exs1024, {S1,S2,S3})).
-define(RAND_EXTERNALIZE(S), rand_externalize(S)).
-define(RAND_INTERNALIZE(S), rand_internalize(S)).
-else.
-define(RAND_UNIFORM(S), random:uniform_s(S)).
-define(RAND_UNIFORM(L, S), random:uniform_s(L, S)).
-define(RAND_SEED(), random:seed0()).
-define(RAND_SEED(S1,S2,S3),                    %Naughty, copied from source
        {(abs(S1) rem (30269-1) + 1),           %PRIME1
         (abs(S2) rem (30307-1) + 1),           %PRIME2
         (abs(S3) rem (30323-1) + 1)}).         %PRIME3
-define(RAND_EXTERNALIZE(S), S).                % random has just three integers for state so no special work needed.
-define(RAND_INTERNALIZE(S), S).
-endif.

install(St0) ->
    St1 = St0#luerl{rand=?RAND_SEED()},        	%Default initial random seed
    luerl_heap:alloc_table(table(), St1).

table() ->
    [{<<"abs">>,#erl_mfa{m=?MODULE,f=abs}},
     {<<"acos">>,#erl_mfa{m=?MODULE,f=acos}},
     {<<"asin">>,#erl_mfa{m=?MODULE,f=asin}},
     {<<"atan">>,#erl_mfa{m=?MODULE,f=atan}},
     {<<"atan2">>,#erl_mfa{m=?MODULE,f=atan2}}, %For 5.2 backwards compatibility
     {<<"ceil">>,#erl_mfa{m=?MODULE,f=ceil}},
     {<<"cos">>,#erl_mfa{m=?MODULE,f=cos}},
     {<<"cosh">>,#erl_mfa{m=?MODULE,f=cosh}},   %For 5.2 backwards compatibility
     {<<"deg">>,#erl_mfa{m=?MODULE,f=deg}},
     {<<"exp">>,#erl_mfa{m=?MODULE,f=exp}},
     {<<"floor">>,#erl_mfa{m=?MODULE,f=floor}},
     {<<"fmod">>,#erl_mfa{m=?MODULE,f=fmod}},
     {<<"frexp">>,#erl_mfa{m=?MODULE,f=frexp}}, %For 5.2 backwards compatibility
     {<<"huge">>,1.7976931348623157e308},       %From the specs
     {<<"ldexp">>,#erl_mfa{m=?MODULE,f=ldexp}}, %For 5.2 backwards compatibility
     {<<"log">>,#erl_mfa{m=?MODULE,f=log}},
     {<<"log10">>,#erl_mfa{m=?MODULE,f=log10}}, %For 5.1 backwards compatibility
     {<<"max">>,#erl_mfa{m=?MODULE,f=max}},
     {<<"maxinteger">>,16#7FFFFFFFFFFFFFFF},    %From Lua 5.4.3
     {<<"min">>,#erl_mfa{m=?MODULE,f=min}},
     {<<"mininteger">>,-16#8000000000000000},   %From Lua 5.4.3
     {<<"modf">>,#erl_mfa{m=?MODULE,f=modf}},
     {<<"pi">>,math:pi()},
     {<<"pow">>,#erl_mfa{m=?MODULE,f=pow}},
     {<<"rad">>,#erl_mfa{m=?MODULE,f=rad}},
     {<<"random">>,#erl_mfa{m=?MODULE,f=random}},
     {<<"randomseed">>,#erl_mfa{m=?MODULE,f=randomseed}},
     {<<"sin">>,#erl_mfa{m=?MODULE,f=sin}},
     {<<"sinh">>,#erl_mfa{m=?MODULE,f=sinh}},   %For 5.2 backwards compatibility
     {<<"sqrt">>,#erl_mfa{m=?MODULE,f=sqrt}},
     {<<"tan">>,#erl_mfa{m=?MODULE,f=tan}},
     {<<"tanh">>,#erl_mfa{m=?MODULE,f=tanh}},   %For 5.2 backwards compatibility
     {<<"tointeger">>,#erl_mfa{m=?MODULE,f=tointeger}},
     {<<"type">>,#erl_mfa{m=?MODULE,f=type}}
    ].

%% abs(Args, State) -> {[Ret],State}.

abs(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[abs(N)],St};
	_ -> badarg_error(abs, As, St)
    end.

acos(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:acos(N)],St};
	_ -> badarg_error(acos, As, St)
    end.

asin(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:asin(N)],St};
	_ -> badarg_error(asin, As, St)
    end.

atan(_, As, St) ->
    case get_number_args(As) of
	[N1,N2|_] when is_number(N1), is_number(N2) ->
	    {[math:atan2(N1, N2)],St};
	[N|_] when is_number(N) -> {[math:atan(N)],St};
	_ -> badarg_error(atan, As, St)
    end.

atan2(_, As, St) ->                                %For 5.2 backwards compatibility
    case get_number_args(As) of
	[N1,N2|_] when is_number(N1), is_number(N2) ->
	    {[math:atan2(N1, N2)],St};
	_ -> badarg_error(atan2, As, St)
    end.

ceil(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[ceil(N)],St};
	_ -> badarg_error(ceil, As, St)
    end.

-ifndef(HAS_CEIL).
%% ceil(Number) -> integer().
%%  Ceil does not exist before 20 so we need to do it ourselves.

ceil(N) when is_integer(N) -> N;
ceil(N) when is_float(N) -> round(N + 0.5).
-endif.

cos(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:cos(N)],St};
	_ -> badarg_error(cos, As, St)
    end.

cosh(_, As, St) ->                                 %For 5.2 backwards compatibility
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:cosh(N)],St};
	_ -> badarg_error(cosh, As, St)
    end.

deg(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[180.0*N/math:pi()],St};
	_ -> badarg_error(deg, As, St)
    end.

exp(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:exp(N)],St};
	_ -> badarg_error(exp, As, St)
    end.

floor(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[floor(N)],St};
	_ -> badarg_error(floor, As, St)
    end.

-ifndef(HAS_FLOOR).
%% floor(Number) -> integer().
%%  Floor does not exist before 20 so we need to do it ourselves.

floor(N) when is_integer(N) -> N;
floor(N) when is_float(N) -> round(N - 0.5).
-endif.

fmod(_, As, St) ->
    case get_number_args(As) of
	[X,Y|_] when is_number(X), is_number(Y) ->
	    Div = trunc(X/Y),
	    Rem = X - Div*Y,
	    {[Rem],St};
	_ -> badarg_error(fmod, As, St)
    end.

frexp(_, As, St) ->                                %For 5.2 backwards compatibility
    %% M,E such that X = M * 2 ^ E.
    case get_number_args(As) of
	[X|_] when is_number(X)  ->
	    %% The sneaky bit!
	    <<Sign:1,Exp0:11,Man0:52>> = <<(X+0.0)/float>>,
	    Two52 = 1 bsl 52,
	    Man1 = (Man0 bor Two52)/Two52,
	    %% Bit naughty here, export Man2, Exp1
	    if Man1 >= 1.0 -> Man2 = Man1/2, Exp1 = Exp0 - 1022;
	       Man1 < 0.5 -> Man2 = Man1*2.0, Exp1 = Exp0 - 1024;
	       true -> Man2 = Man1, Exp1 = Exp0 - 1023
	    end,
	    Ret = if Sign =:= 1 -> -Man2;
		     true -> Man2
		  end,
	    {[float(Ret),Exp1],St};
	_ -> badarg_error(frexp, As, St)
    end.

ldexp(_, As, St) ->                                %For 5.2 backwards compatibility
    case get_number_args(As) of
	[M,E|_] when is_float(M), is_integer(E) ->
	    {[M*math:pow(2, E)],St};
%% 	    <<X/float>> = <<0:1,E:11,M:52>>,
%% 	    {[X],St};
	_ -> badarg_error(ldexp, As, St)
    end.

log(_, As, St) ->
    case get_number_args(As) of
	[N1,N2|_] when is_number(N1), N2 == 10 ->
	    {[math:log10(N1)],St};		%Seeing it is builtin
	[N1,N2|_] when is_number(N1), is_number(N2) ->
	    {[math:log(N1)/math:log(N2)],St};
	[N|_] when is_number(N) ->
	    {[math:log(N)],St};
	_ -> badarg_error(log, As, St)
    end.

log10(_, As, St) ->				%For 5.1 backwards compatibility
    case get_number_args(As) of
	[N|_] when N == 0 -> {[-500.0],St};	%Bit hacky
	[N|_] when is_number(N) ->
	    {[math:log10(N)],St};
	_ -> badarg_error(log10, As, St)
    end.

max(_, As, St) ->
    case luerl_lib:args_to_numbers(As) of
	[_|_]=Ns -> {[lists:max(Ns)],St};	%At least one number
	_ -> badarg_error(max, As, St)
    end.

min(_, As, St) ->
    case luerl_lib:args_to_numbers(As) of
	[_|_]=Ns -> {[lists:min(Ns)],St};	%At least one number
	_ -> badarg_error(min, As, St)
    end.

modf(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_integer(N) -> {[N,0.0],St};
	[N|_] when is_float(N) ->
	    I = trunc(N),			%Integral part
	    {[I,float(N-I)],St};
	_ -> badarg_error(modf, As, St)
    end.

pow(_, As, St) ->                                  %For 5.2 backwards compatibility
    case get_number_args(As) of
	[N1,N2|_] when is_number(N1) and is_number(N2) ->
	    {[math:pow(N1, N2)],St};
	_ -> badarg_error(pow, As, St)
    end.

rad(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:pi()*N/180.0],St};
	_ -> badarg_error(rad, As, St)
    end.

random(_, As, #luerl{rand=S0}=St) ->
    case luerl_lib:args_to_integers(As) of
	[] ->					%0.0 - 1.0
	    {R,S1} = ?RAND_UNIFORM(S0),
	    {[R],St#luerl{rand=S1}};
	[M] when M >= 1 ->
	    {R,S1} = ?RAND_UNIFORM(M, S0),
	    {[R],St#luerl{rand=S1}};
	[M,N] when N >= M ->
	    {R,S1} = ?RAND_UNIFORM(N - M + 1, S0),
	    {[R + M - 1],St#luerl{rand=S1}};
	_ -> badarg_error(random, As, St)
    end.

randomseed(_, As, St) ->
    case get_number_args(As) of
	[S|_] when is_number(S) ->
	    %% Split integer or float-64 into three integers.
	    <<A1:24,A2:24,A3:16>> = <<S/float>>,
	    {[],St#luerl{rand=?RAND_SEED(A1, A2, A3)}};
	_ -> badarg_error(randomseed, As, St)
    end.

sin(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:sin(N)],St};
	_ -> badarg_error(sin, As, St)
    end.

sinh(_, As, St) ->                                 %For 5.2 backwards compatibility
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:sinh(N)],St};
	_ -> badarg_error(sinh, As, St)
    end.

sqrt(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:sqrt(N)],St};
	_ -> badarg_error(sqrt, As, St)
    end.

tan(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:tan(N)],St};
	_ -> badarg_error(tan, As, St)
    end.

tanh(_, As, St) ->                                 %For 5.2 backwards compatibility
    case get_number_args(As) of
	[N|_] when is_number(N) -> {[math:tanh(N)],St};
	_ -> badarg_error(tanh, As, St)
    end.

tointeger(_, As, St) ->
    case get_number_args(As) of
	[N|_] when is_integer(N) -> {[N],St};
	[N|_] when is_float(N) ->
	    case trunc(N) of
		I when I == N -> {[I],St};
		_ -> {[nil], St}
	    end;
	[_|_] -> {[nil],St};
	[] -> badarg_error(tointeger, As, St)
    end.

type(_, As, St) ->
    %% No conversion here.
    case As of
	[N|_] when is_integer(N) -> {[<<"integer">>],St};
	[N|_] when is_float(N) -> {[<<"float">>],St};
	[_|_] -> {[nil],St};			%Not a number
	[] -> badarg_error(type, As, St)
    end.

%% get_number_args(Args) -> [Number].
%%  Convert args to numbers inserting nil when not possible. This
%%  allows us to disting between no numbers and an empty list.

get_number_args(As) ->
    lists:map(fun luerl_lib:arg_to_number/1, As).

%% get_number_args([A|As]) ->
%%     case luerl_lib:tonumber(A) of
%% 	N when is_number(N) ->
%% 	    [N|get_number_args(As)];
%% 	nil -> []
%%     end;
%% get_number_args([]) -> [].

internalize(S) ->
   ?RAND_INTERNALIZE(S).

externalize(S) ->
   ?RAND_EXTERNALIZE(S).

-ifdef(NEW_RAND).
rand_externalize(#luerl{rand=S0}=St) ->
    St#luerl{rand=rand:export_seed_s(S0)}.

rand_internalize(#luerl{rand=S0}=St) ->
    St#luerl{rand=rand:seed_s(S0)}.
-endif.
