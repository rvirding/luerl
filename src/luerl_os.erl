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

%% File    : luerl_os.erl
%% Author  : Robert Virding
%% Purpose : The os library for Luerl.

-module(luerl_os).

-export([install/1]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

install(St) ->
    luerl_eval:alloc_table(table(), St).

table() ->
    [{<<"clock">>,{function,fun clock/2}},
     {<<"date">>,{function,fun date/2}},
     {<<"difftime">>,{function,fun difftime/2}},
     {<<"getenv">>,{function,fun getenv/2}},
     {<<"time">>,{function,fun time/2}}].

getenv([<<>>|_], St) -> {[nil],St};
getenv([A|_], St) when is_binary(A) ; is_number(A) ->
    case os:getenv(luerl_lib:to_list(A)) of
	Env when is_list(Env) ->
	    {[list_to_binary(Env)],St};
	false -> {[nil],St}
    end;
getenv(As, _) -> lua_error({badarg,getenv,As}).

%% Time functions.

clock(_, St) ->					%This is wrong!
    {Mega,S,Micro} = now(),
    {[1.0e6*Mega+S+Micro*1.0e-6],St}.

date(_, St) ->
    {{Ye,Mo,Da},{Ho,Mi,Sec}} = calendar:local_time(),
    Str = io_lib:fwrite("~w-~.2.Ow-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
			[Ye,Mo,Da,Ho,Mi,Sec]),
    {[iolist_to_binary(Str)],St}.

difftime([A1,A2|_], St) ->
    {[A2-A1],St}.

time(_, St) ->					%Time since 1 Jan 1970
    {M,S,_} = now(),
    {[1.0e6*M+S],St}.
