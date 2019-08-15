%% Copyright (c) 2013 Robert Virding
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
%%  A chunk is now a list of instructions to define the function.

chunk(#code{code=Is0}=Code, Opts) ->
    Is1 = instrs(Is0, nil),			%No local state
    luerl_comp:debug_print(Opts, "cp: ~p\n", [Is1]),
    {ok,Code#code{code=Is1}}.

%% Combining instructions.
instrs([?PUSH_LIT_INFO(L),?GET_KEY_INFO_NOTUSED|Is], St) ->  % here are two info, I can't mix them.
    instrs([?GET_LIT_KEY_INFO(L)|Is], St);
instrs([?PUSH_LIT_INFO(L),?SET_KEY_INFO_NOTUSED|Is], St) ->
    instrs([?SET_LIT_KEY_INFO(L)|Is], St);


%% Must check these properly, probably seldom used anyway.
%% instrs([?STORE_EVAR_INFO(D, I),?PUSH_EVAR_INFO_NOTUSED(D, I)|Is], St) ->
%%     instrs([?DUP_INFO,?STORE_EVAR_INFO(D, I)|Is], St);
%% instrs([?STORE_LVAR_INFO(D, I),?PUSH_LVAR_INFO_NOTUSED(D, I)|Is], St) ->
%%     instrs([?DUP_INFO,?STORE_LVAR_INFO(D, I)|Is], St);
%% instrs([?STORE_GVAR_INFO(K),?PUSH_GVAR_INFO_NOTUSED(K)|Is], St) ->
%%     instrs([?DUP_INFO,?STORE_EVAR_INFO(D, I)|Is], St);

instrs([?PUSH_LIT_INFO(L),?MULTIPLE_INFO_NOTUSED|Is], St) ->         %% IMPORTANT: the two Infos are different.
    instrs([?PUSH_LAST_LIT_INFO(L)|Is], St);                         %% I can use only one of them in mixed instructions
instrs([?PUSH_LVAR_INFO(D, I),?MULTIPLE_INFO_NOTUSED|Is], St) ->
    instrs([?PUSH_LAST_LVAR_INFO(D, I)|Is], St);
instrs([?PUSH_EVAR_INFO(D, I),?MULTIPLE_INFO_NOTUSED|Is], St) ->
    instrs([?PUSH_LAST_EVAR_INFO(D, I)|Is], St);
instrs([?PUSH_GVAR_INFO(K),?MULTIPLE_INFO_NOTUSED|Is], St) ->
    instrs([?PUSH_LAST_GVAR_INFO(K)|Is], St);

instrs([?POP_INFO,?POP_INFO_NOTUSED|Is], St) ->
    instrs([?POP2_INFO|Is], St);

%% Doing sub instructions.
instrs([?PUSH_FDEF_INFO(Lsz,Esz,Pars,Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?PUSH_FDEF_INFO(Lsz,Esz,Pars,Fis1)|instrs(Is, St)];
instrs([?BLOCK_INFO_NOTUSED(0,0,Bis)|Is], St) ->		%No need for block
    instrs(Bis ++ Is, St);
instrs([?BLOCK_INFO(Lsz,Esz,Bis0)|Is], St) ->
    Bis1 = instrs(Bis0, St),
    [?BLOCK_INFO(Lsz,Esz,Bis1)|instrs(Is, St)];
instrs([?REPEAT_INFO(Ris0)|Is], St) ->
    Ris1 = instrs(Ris0, St),
    [?REPEAT_INFO(Ris1)|instrs(Is, St)];
instrs([?WHILE_INFO(Eis0, Wis0)|Is], St) ->
    Eis1 = instrs(Eis0, St),
    Wis1 = instrs(Wis0, St),
    [?WHILE_INFO(Eis1, Wis1)|instrs(Is, St)];
instrs([?AND_THEN_INFO(Tis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    [?AND_THEN_INFO(Tis1)|instrs(Is, St)];
instrs([?OR_ELSE_INFO(Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?OR_ELSE_INFO(Fis1)|instrs(Is, St)];
instrs([?IF_TRUE_INFO(Tis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    [?IF_TRUE_INFO(Tis1)|instrs(Is, St)];
instrs([?IF_FALSE_INFO(Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?IF_FALSE_INFO(Fis1)|instrs(Is, St)];
instrs([?IF_INFO(Tis, [])|Is], St) ->
    instrs([?IF_TRUE_INFO(Tis)|Is], St);
instrs([?IF_INFO([], Fis)|Is], St) ->		%This should never happen
    instrs([?IF_FALSE_INFO(Fis)|Is], St);
instrs([?IF_INFO(Tis0, Fis0)|Is], St) ->
    Tis1 = instrs(Tis0, St),
    Fis1 = instrs(Fis0, St),
    [?IF_INFO(Tis1, Fis1)|instrs(Is, St)];
instrs([?NFOR_INFO(V, Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?NFOR_INFO(V, Fis1)|instrs(Is, St)];
instrs([?GFOR_INFO(Vs, Fis0)|Is], St) ->
    Fis1 = instrs(Fis0, St),
    [?GFOR_INFO(Vs, Fis1)|instrs(Is, St)];

%% Nothing to do.
instrs([I|Is], St) -> [I|instrs(Is, St)];
instrs([], _) -> [].
