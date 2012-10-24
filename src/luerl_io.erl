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

%% File    : luerl_io.erl
%% Author  : Robert Virding
%% Purpose : The io library for Luerl.

%% This is a quick hack to get io working. It will be improved in time.

-module(luerl_io).

-include("luerl.hrl").

-export([install/1]).

-import(luerl_lib, [lua_error/1]).		%Shorten this

install(St) ->
    luerl_eval:alloc_table(table(), St).

%% table() -> [{FuncName,Function}].

table() ->
    [{<<"flush">>,{function,fun flush/2}},
     {<<"write">>,{function,fun write/2}}
    ].

flush(_, St) -> {[true],St}.

write(As, St) ->
    case luerl_lib:conv_list(As, [lua_string]) of
	nil -> lua_error({badarg,write,As});
	Ss ->
	    lists:foreach(fun (S) -> io:format("~s", [S]) end, Ss),
	    {[#userdata{d=standard_io}],St}
    end.
