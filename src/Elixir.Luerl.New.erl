%% Copyright (c) 2013-2021 Robert Virding
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

%% Basic user API to luerl.
-export([init/0,gc/1,
         load/2,load/3,loadfile/2,loadfile/3,
         load_module/3,load_module_dec/3,
         do/2,do_dec/2,do/3,do_dec/3,
         dofile/2,dofile/3,dofile_dec/2,dofile_dec/3,
         call/3,call_chunk/2,call_chunk/3,
         call_function/3,call_function_dec/3,
         call_method/4,call_method_dec/4,
         get_table_keys/2,get_table_keys_dec/2,
         set_table_keys/3,set_table_keys_dec/3,
         get_stacktrace/1
        ]).

%% Tracing.
-export([set_trace_func/2,clear_trace_func/1,
         set_trace_data/2,get_trace_data/1]).

%% Encoding and decoding.
-export([encode/2,encode_list/2,decode/2,decode_list/2]).

init() ->
    luerl_new:init().

gc(St) ->
    luerl_new:gc(St).

set_trace_func(St, Func) ->
    luerl_new:set_trace_func(Func, St).

clear_trace_func(St) ->
    luerl_new:clear_trace_func(St).

get_trace_data(St) ->
    luerl_new:get_trace_data(St).

set_trace_data(St, Tdata) ->
    luerl_new:set_trace_data(Tdata, St).

load(St, Bin) ->
    luerl_new:load(Bin, St).

load(St, Bin, Opts) ->
    luerl_new:load(Bin, Opts, St).

loadfile(St, Name) ->
    luerl_new:loadfile(Name, St).

loadfile(St, Name, Opts) ->
    luerl_new:loadfile(Name, Opts, St).

load_module(St, Lfp, Mod) ->
    luerl_new:load_module(Lfp, Mod, St).

load_module_dec(St, Dfp, Mod) ->
    luerl_new:load_module_dec(Dfp, Mod, St).

do(St, S) ->
    luerl_new:do(S, St).

do(St, S, Opts) ->
    luerl_new:do(S, Opts, St).

do_dec(St, S) ->
    luerl_new:do_dec(S, St).

do_dec(St, S, Opts) ->
    luerl_new:do_dec(S, Opts, St).

dofile(St, Path) ->
    luerl_new:dofile(Path, St).

dofile(St, Path, Opts) ->
    luerl_new:dofile(Path, Opts, St).

dofile_dec(St, Path) ->
    luerl_new:dofile_dec(Path, St).

dofile_dec(St, Path, Opts) ->
    luerl_new:dofile_dec(Path, Opts, St).

call(St, C, Args) ->
    luerl_new:call(C, Args, St).

call_chunk(St, C) ->
    luerl_new:call_chunk(C, St).

call_chunk(St, C, Args) ->
    luerl_new:call_chunk(C, Args, St).

call_function(St, Fp, Args) ->
    luerl_new:call_function(Fp, Args, St).

call_function_dec(St, Dfunc, Dargs) ->
    luerl_new:call_function_dec(Dfunc, Dargs, St).

call_method(St, Obj, Meth, Args) ->
    luerl_new:call_method(Obj, Meth, Args, St).

call_method_dec(St, Dobj, Dmeth, Dargs) ->
    luerl_new:call_method_dec(Dobj, Dmeth, Dargs, St).

get_table_keys(St, Keys) ->
    luerl_new:get_table_keys(Keys, St).

get_table_keys_dec(St, Dkeys) ->
    luerl_new:get_table_keys_dec(Dkeys, St).

set_table_keys(St, Keys, Val) ->
    luerl_new:set_table_keys(Keys, Val, St).

set_table_keys_dec(St, Dkeys, Dval) ->
    luerl_new:set_table_keys_dec(Dkeys, Dval, St).

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
