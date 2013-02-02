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

%% File    : luerl_comp_peep.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.2 compiler for Luerl.

%% Does peep-hole optimisation in the compiler.

-module(luerl_comp_peep).

-include("luerl.hrl").
-include("luerl_comp.hrl").
-include("luerl_instrs.hrl").

-export([chunk/2]).

%% chunk(St0, Opts) -> {ok,St0}.

chunk(#code{code=Is0}=Code, Opts) ->
    Is1 = instrs(Is0, nil),			%No local state
    luerl_comp:debug_print(Opts, "cp: ~p\n", [Is1]),
    {ok,Code#code{code=Is1}}.

%% Combining instructions, table in Acc.
instrs([?PUSH,?LOAD_LIT(L),?GET_KEY|Is], St) ->
    instrs([?GET_LIT_KEY(L)|Is], St);
instrs([?PUSH,?LOAD_LIT(L),?SET_KEY|Is], St) ->
    instrs([?SET_LIT_KEY(L)|Is], St);
instrs([?LOAD_LIT(L),?PUSH|Is], St) -> instrs([?PUSH_LIT(L)|Is], St);
instrs([?LOAD_LVAR(D,I),?PUSH|Is], St) -> instrs([?PUSH_LVAR(D,I)|Is], St);
instrs([?LOAD_EVAR(D,I),?PUSH|Is], St) -> instrs([?PUSH_EVAR(D,I)|Is], St);
instrs([?LOAD_GVAR(K),?PUSH|Is], St) -> instrs([?PUSH_GVAR(K)|Is], St);
%% Are these safe? Value should be left in Acc.
instrs([?STORE_LVAR(D,I),?LOAD_LVAR(D,I)|Is], St) ->
    instrs([?STORE_LVAR(D,I)|Is], St);
instrs([?STORE_EVAR(D,I),?LOAD_EVAR(D,I)|Is], St) ->
    instrs([?STORE_EVAR(D,I)|Is], St);
instrs([?STORE_GVAR(K),?LOAD_GVAR(K)|Is], St) ->
    instrs([?STORE_GVAR(K)|Is], St);
%% Doing sub instructions.
instrs([?FDEF(Lsz,Esz,Pars,Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?FDEF(Lsz,Esz,Pars,Fis1)|instrs(Is, St)];
instrs([?BLOCK(Lsz,Esz,Bis0)|Is], St) ->
    Bis1 = instrs(Bis0, St),
    [?BLOCK(Lsz,Esz,Bis1)|instrs(Is, St)];
instrs([?REPEAT(Ris0)|Is], St) ->
    Ris1 = instrs(Ris0, St),
    [?REPEAT(Ris1)|instrs(Is, St)];
instrs([?WHILE(Eis0, Wis0)|Is], St) ->
    Eis1 = instrs(Eis0, St),
    Wis1 = instrs(Wis0, St),
    [?WHILE(Eis1, Wis1)|instrs(Is, St)];
instrs([?IF_TRUE(Tis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    [?IF_TRUE(Tis1)|instrs(Is, St)];
instrs([?IF_FALSE(Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?IF_FALSE(Fis1)|instrs(Is, St)];
instrs([?IF(Tis0, Fis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    Fis1 = instrs(Fis0, St),
    [?IF(Tis1, Fis1)|instrs(Is, St)];
instrs([?NFOR(V, Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?NFOR(V, Fis1)|instrs(Is, St)];
instrs([?GFOR(Vs, Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?GFOR(Vs, Fis1)|instrs(Is, St)];
%% Nothing to do.
instrs([I|Is], St) ->
    [I|instrs(Is, St)];
instrs([], _) -> [].
