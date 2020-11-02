%% Copyright (c) 2020 Robert Virding
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

%% File    : luerl_new.erl
%% Authors : Robert Virding
%% Purpose : Basic LUA 5.3 interface.

-module(luerl_new).

-include("luerl.hrl").

-export([init/0,gc/1,
	 load/2,load/3,loadfile/2,loadfile/3,
	 load_module/3,
	 do/2,dofile/2,
	 call/3,call_chunk/3,call_function/3,call_method/4,
	 get_table_keys/2,set_table_keys/3,
	 get_stacktrace/1
	]).

%% Encoding and decoding.
-export([encode/2,encode_list/2,decode/2,decode_list/2]).

%% Just call the other module!

init() -> new_luerl:init().
gc(St) -> new_luerl:gc(St).
load(Str, St) ->
    new_luerl:load(Str, St).
load(Str, Opts, St) ->
    new_luerl:load(Str, Opts, St).
loadfile(Name, St) ->
    new_luerl:init(Name, St).
loadfile(Name, Opts, St) ->
    new_luerl:loadfile(Name, Opts, St).
load_module(TabPath, Module, St) ->
    new_luerl:load_module(TabPath, Module, St).
do(What, St) ->
    new_luerl:do(What, St).
dofile(Name, St) ->
    new_luerl:dofile(Name, St).
call(FuncRef, Args, St) ->
    new_luerl:call(FuncRef, Args, St).
call_chunk(FuncRef, Args, St) ->
    new_luerl:call_chunk(FuncRef, Args, St).
call_function(FuncRef, Args, St) ->
    new_luerl:call_function(FuncRef, Args, St).
call_method(Obj, Meth, Args, St) ->
    new_luerl:call_method(Obj, Meth, Args, St).
get_table_keys(Keys, St) ->
    new_luerl:get_table_keys(Keys, St).
set_table_keys(Keys, Val, St) ->
    new_luerl:set_table_keys(Keys, Val, St).
get_stacktrace(St) ->
    new_luerl:get_stacktrace(St).
encode(Term, St) ->
    new_luerl:encode(Term, St).
encode_list(Terms, St) ->
    new_luerl:encode_list(Terms, St).
decode(Term, St) ->
    new_luerl:decode(Term, St).
decode_list(Terms, St) ->
    new_luerl:decode_list(Terms, St).
