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

-module('Elixir.Luerl').

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
         encode/2,encode_list/2,decode/2,decode_list/2
        ]).

eval(St, Chunk) ->
     luerl:eval(Chunk, St).

evalfile(St, Path) ->
    luerl:evalfile(Path, St).

do(St, S) ->
    luerl:do(S, St).

dofile(St, Path) ->
    luerl:dofile(Path, St).

load(St, Bin) ->
    luerl:load(Bin, St).

load(St, Bin, Opts) ->
    luerl:load(Bin, Opts, St).

loadfile(St, Name) ->
    luerl:loadfile(Name, St).

loadfile(St, Name, Opts) ->
    luerl:loadfile(Name, Opts, St).

path_loadfile(St, Name) ->
    luerl:path_loadfile(Name, St).

path_loadfile(St, Dirs, Name) ->
    luerl:path_loadfile(Dirs, Name, St).

path_loadfile(St, Dir, Name, Opts) ->
    luerl:path_loadfile(Dir, Name, Opts, St).

load_module(St, Fp, Mod) ->
    luerl:load_module(Fp, Mod, St).

load_module1(St, Fp, Mod) ->
    luerl:load_module1(Fp, Mod, St).

init() ->
    luerl:init().

call(St, C, As) ->
    luerl:call(C, As, St).

call_chunk(St, C, As) ->
    luerl:call_chunk(C, As, St).

call_function(St, Fp, As) ->
    luerl:call_function(Fp, As, St).

call_function1(St, Lfp, Las) ->
    luerl:call_function1(Lfp, Las, St).

function_list(St, Ks) ->
    luerl:function_list(Ks, St).

call_method(St, Fp, As) ->
    luerl:call_method(Fp, As, St).

call_method1(St, Fp, Las) ->
    luerl:call_method1(Fp, Las, St).

method_list(St, Ks) ->
    luerl:method_list(Ks, St).

get_table(St, Fp) ->
    luerl:get_table(Fp, St).

get_table1(St, Fp) ->
    luerl:get_table1(Fp, St).

set_table(St, Fp, V) ->
    luerl:set_table(Fp, V, St).

set_table1(St, Lfp, Lv) ->
    luerl:set_table1(Lfp, Lv, St).

set_table1(St, Tab, Key, Lv) ->
    luerl:set_table1(Tab, Key, Lv, St).

stop(St) ->
    luerl:stop(St).

gc(St) ->
    luerl:gc(St).

encode_list(St, Ts) ->
    luerl:encode_list(Ts, St).

encode(St, V) ->
    luerl:encode(V, St).

decode_list(St, Lts) ->
    luerl:decode_list(Lts, St).

decode(St, V) ->
    luerl:decode(V, St).
