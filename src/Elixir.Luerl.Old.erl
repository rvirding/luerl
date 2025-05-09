%% Copyright (c) 2013-2024 Robert Virding
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
%% Purpose : Elixir-style wrappers for luerl_old.erl

%% This module just contains functions that forward to luerl_old.erl,
%% but place the VM State arguments in the first position rather than
%% the last. This better matches Elixir conventions and allows for
%% using the Elixir pipe operator '|>' to chain Luerl function calls.

-module('Elixir.Luerl.Old').

-include("luerl.hrl").

?MODULEDOC("""
Legacy Elixir API for Luerl

This module provides Elixir-friendly wrappers for the luerl_old module
with state as the first argument for better pipe operator usage.
New code should use the Luerl module instead.
""").

-export([eval/2,evalfile/2,
         do/2,dofile/2,
         load/2,load/3,
         loadfile/2,loadfile/3,
         path_loadfile/2,path_loadfile/3,path_loadfile/4,
         load_module/3,load_module1/3,
         call/3,call_chunk/3,
         call_function/3,call_function1/3,function_list/2,
         call_method/3,call_method1/3,method_list/2,
         get_table/2,get_table1/2,set_table/3,set_table1/3,set_table1/4,
         init/0,stop/1,gc/1,
         set_trace_func/2,clear_trace_func/1,
         set_trace_data/2,get_trace_data/1,
         get_stacktrace/1,
         externalize/1,internalize/1
        ]).

%% Encoding and decoding.
-export([encode/2,encode_list/2,decode/2,decode_list/2]).

eval(St, Chunk) ->
     luerl_old:eval(Chunk, St).

evalfile(St, Path) ->
    luerl_old:evalfile(Path, St).

do(St, S) ->
    luerl_old:do(S, St).

dofile(St, Path) ->
    luerl_old:dofile(Path, St).

load(St, Bin) ->
    luerl_old:load(Bin, St).

load(St, Bin, Opts) ->
    luerl_old:load(Bin, Opts, St).

loadfile(St, Name) ->
    luerl_old:loadfile(Name, St).

loadfile(St, Name, Opts) ->
    luerl_old:loadfile(Name, Opts, St).

path_loadfile(St, Name) ->
    luerl_old:path_loadfile(Name, St).

path_loadfile(St, Dirs, Name) ->
    luerl_old:path_loadfile(Dirs, Name, St).

path_loadfile(St, Dir, Name, Opts) ->
    luerl_old:path_loadfile(Dir, Name, Opts, St).

load_module(St, Fp, Mod) ->
    luerl_old:load_module(Fp, Mod, St).

load_module1(St, Fp, Mod) ->
    luerl_old:load_module1(Fp, Mod, St).

init() ->
    luerl_old:init().

call(St, C, As) ->
    luerl_old:call(C, As, St).

call_chunk(St, C, As) ->
    luerl_old:call_chunk(C, As, St).

call_function(St, Fp, As) ->
    luerl_old:call_function(Fp, As, St).

call_function1(St, Lfp, Las) ->
    luerl_old:call_function1(Lfp, Las, St).

function_list(St, Ks) ->
    luerl_old:function_list(Ks, St).

call_method(St, Fp, As) ->
    luerl_old:call_method(Fp, As, St).

call_method1(St, Fp, Las) ->
    luerl_old:call_method1(Fp, Las, St).

method_list(St, Ks) ->
    luerl_old:method_list(Ks, St).

get_table(St, Fp) ->
    luerl_old:get_table(Fp, St).

get_table1(St, Fp) ->
    luerl_old:get_table1(Fp, St).

set_table(St, Fp, V) ->
    luerl_old:set_table(Fp, V, St).

set_table1(St, Lfp, Lv) ->
    luerl_old:set_table1(Lfp, Lv, St).

set_table1(St, Tab, Key, Lv) ->
    luerl_old:set_table1(Tab, Key, Lv, St).

stop(St) ->
    luerl_old:stop(St).

gc(St) ->
    luerl_old:gc(St).

set_trace_func(St, Func) ->
    luerl_old:set_trace_func(Func, St).

clear_trace_func(St) ->
    luerl_old:clear_trace_func(St).

get_trace_data(St) ->
    luerl_old:get_trace_data(St).

set_trace_data(St, Tdata) ->
    luerl_old:set_trace_data(Tdata, St).

get_stacktrace(St) ->
    luerl_old:get_stacktrace(St).

encode_list(St, Ts) ->
    luerl_old:encode_list(Ts, St).

encode(St, V) ->
    luerl_old:encode(V, St).

decode_list(St, Lts) ->
    luerl_old:decode_list(Lts, St).

decode(St, V) ->
    luerl_old:decode(V, St).

externalize(St) ->
    luerl_old:externalize(St).

internalize(St) ->
    luerl_old:internalize(St).
