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

%% File    : luerl_ex.erl
%% Authors : Cees de Groot
%% Purpose : Elixir-style wrappers for luerl.erl

%% This module just contains functions that forward to luerl.erl, but place
%% the VM State arguments in the first position rather than the last. This
%% better matches Elixir conventions and allows for using the Elixir pipe
%% operator '|>' to chain Luerl function calls.

-module('Elixir.Luerl.New').

-export([init/0,gc/1,
	 load/2,load/3,loadfile/2,loadfile/3,
	 load_module/3,
	 do/2,dofile/2,
	 call/3,call_chunk/3,call_function/3,call_method/4,
	 get_table_keys/2,set_table_keys/3,
	 get_stacktrace/1,
	 encode/2,encode_list/2,decode/2,decode_list/2]).

init() ->
    luerl_new:init().

gc(St) ->
    luerl_new:gc(St).

load(St, Bin) ->
    luerl_new:load(Bin, St).

load(St, Bin, Opts) ->
    luerl_new:load(Bin, Opts, St).

loadfile(St, Name) ->
    luerl_new:loadfile(Name, St).

loadfile(St, Name, Opts) ->
    luerl_new:loadfile(Name, Opts, St).

load_module(St, Fp, Mod) ->
    luerl_new:load_module(Fp, Mod, St).

do(St, S) ->
    luerl_new:do(S, St).

dofile(St, Path) ->
    luerl_new:dofile(Path, St).

call(St, C, Args) ->
    luerl_new:call(C, Args, St).

call_chunk(St, C, Args) ->
    luerl_new:call_chunk(C, Args, St).

call_function(St, Fp, Args) ->
    luerl_new:call_function(Fp, Args, St).

call_method(St, Obj, Meth, Args) ->
    luerl_new:call_method(Obj, Meth, Args, St).

get_table_keys(St, Keys) ->
    luerl_new:get_table_keys(Keys, St).

set_table_keys(St, Keys, Val) ->
    luerl_new:set_table_keys(Keys, Val, St).

get_stacktrace(St) ->
    luerl_new:get_stacktrace(St).

encode(St, V) ->
    luerl_new:encode(V, St).

encode_list(St, Ts) ->
    luerl_new:encode_list(Ts, St).

decode(St, V) ->
    luerl_new:decode(V, St).

decode_list(St, Lts) ->
    luerl_new:decode_list(Lts, St).
