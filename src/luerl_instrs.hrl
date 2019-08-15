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

%% File    : luerl_instrs.hrl
%% Author  : Robert Virding
%% Purpose : Internal LUA 5.2 instructions.

%% Expression instructions.

% I had to duplicate INFO_EMTPY, INFO and INFO_NOTUSED cases becase in pattern patching and in variable_creaction
% we had to use different version. Empty: in variable creation, simple INFO and INFO_NOTUSED: in pattern matching

% HINT:  token_info_internal_created(HINT) receives a HINT, its a parameter for debugging, if you want,
%        you can insert the parameters, too, example:  {push_lit, L, 'INFO_EMPTY'} instead of push_lit

-define(PUSH_LIT_INFO_EMPTY(L), {luerl_comp:token_info_internal_created(push_lit), push_lit,L}).
-define(PUSH_LIT_INFO(L), {Info, push_lit,L}). % for pattern matching, existing Info
-define(PUSH_LIT_INFO_NOTUSED(L), {_Info, push_lit,L}).

-define(PUSH_LVAR_INFO_EMPTY(D,I), {luerl_comp:token_info_internal_created(push_lvar), push_lvar,D,I}).
-define(PUSH_LVAR_INFO(D,I), {Info, push_lvar,D,I}).
-define(PUSH_LVAR_INFO_NOTUSED(D,I), {_Info, push_lvar,D,I}).

-define(PUSH_EVAR_INFO_EMPTY(D, I), {luerl_comp:token_info_internal_created(push_evar), push_evar,D,I}).
-define(PUSH_EVAR_INFO(D, I), {Info, push_evar,D,I}).
-define(PUSH_EVAR_INFO_NOTUSED(D, I), {_Info, push_evar,D,I}).

-define(PUSH_GVAR_INFO_EMPTY(K), {luerl_comp:token_info_internal_created(push_gvar), push_gvar,K}).
-define(PUSH_GVAR_INFO(K), {Info, push_gvar,K}).
-define(PUSH_GVAR_INFO_NOTUSED(K), {_Info, push_gvar,K}).

-define(PUSH_LAST_LIT_INFO_EMPTY(L), {luerl_comp:token_info_internal_created(push_last_lit), push_last_lit,L}).	%[?PUSH_LIT_INFO_EMPTY,?MULTIPLE_INFO_EMPTY]
-define(PUSH_LAST_LIT_INFO(L), {Info, push_last_lit,L}).	%[?PUSH_LIT_INFO_EMPTY,?MULTIPLE_INFO_EMPTY]

-define(PUSH_LAST_LVAR_INFO_EMPTY(D,I), {luerl_comp:token_info_internal_created(push_last_lvar), push_last_lvar,D,I}).
-define(PUSH_LAST_LVAR_INFO(D,I), {Info, push_last_lvar,D,I}).

-define(PUSH_LAST_EVAR_INFO_EMPTY(D, I), {luerl_comp:token_info_internal_created(push_last_evar), push_last_evar,D,I}).
-define(PUSH_LAST_EVAR_INFO(D, I), {Info, push_last_evar,D,I}).

-define(PUSH_LAST_GVAR_INFO_EMPTY(K), {luerl_comp:token_info_internal_created(push_last_gvar), push_last_gvar,K}).
-define(PUSH_LAST_GVAR_INFO(K), {Info, push_last_gvar,K}).

-define(STORE_LVAR_INFO_EMPTY(D, I), {luerl_comp:token_info_internal_created(store_lvar), store_lvar,D,I}).
-define(STORE_LVAR_INFO(D, I), {Info, store_lvar,D,I}).
-define(STORE_LVAR_INFO_NOTUSED(D, I), {_Info, store_lvar,D,I}).

-define(STORE_EVAR_INFO_EMPTY(D, I), {luerl_comp:token_info_internal_created(store_evar), store_evar,D,I}).
-define(STORE_EVAR_INFO(D, I), {Info, store_evar,D,I}).

-define(STORE_GVAR_INFO_EMPTY(K), {luerl_comp:token_info_internal_created(store_gvar), store_gvar,K}).
-define(STORE_GVAR_INFO(K), {Info, store_gvar,K}).

-define(GET_KEY_INFO_EMPTY, {luerl_comp:token_info_internal_created(get_key), get_key}).			%Acc = Stk[Acc]
-define(GET_KEY_INFO, {Info, get_key}).			%Acc = Stk[Acc]
-define(GET_KEY_INFO_NOTUSED, {_Info, get_key}). % in mixed instructions I can use one Info

-define(GET_LIT_KEY_INFO_EMPTY(K), {luerl_comp:token_info_internal_created(get_lit_key), get_lit_key,K}).	%[?PUSH_LIT_INFO_EMPTY(K),?GET_KEY_INFO_EMPTY]
-define(GET_LIT_KEY_INFO(K), {Info, get_lit_key,K}).	%[?PUSH_LIT_INFO_EMPTY(K),?GET_KEY_INFO_EMPTY]

-define(SET_KEY_INFO_EMPTY, {luerl_comp:token_info_internal_created(set_key), set_key}).			%Stk[
-define(SET_KEY_INFO, {Info, set_key}).			%Stk[
-define(SET_KEY_INFO_NOTUSED, {_Info, set_key}).

-define(SET_LIT_KEY_INFO_EMPTY(K), {luerl_comp:token_info_internal_created(set_lit_key), set_lit_key,K}).	%[?PUSH_LIT_INFO_EMPTY(K),?SET_KEY_INFO_EMPTY]
-define(SET_LIT_KEY_INFO(K), {Info, set_lit_key,K}).	%[?PUSH_LIT_INFO_EMPTY(K),?SET_KEY_INFO_EMPTY]

-define(SINGLE_INFO_EMPTY, {luerl_comp:token_info_internal_created(simple), single}).			%Ensure single value
-define(SINGLE_INFO, {Info, single}).			%Ensure single value

-define(MULTIPLE_INFO_EMPTY, {luerl_comp:token_info_internal_created(multiple), multiple}).			%Ensure multiple value
-define(MULTIPLE_INFO, {Info, multiple}).			%Ensure multiple value
-define(MULTIPLE_INFO_NOTUSED, {_Info, multiple}).			%Ensure multiple value


-define(BUILD_TAB_INFO_EMPTY(Fc, I), {luerl_comp:token_info_internal_created(build_tab), build_tab,Fc,I}).
-define(BUILD_TAB_INFO(Fc, I), {Info, build_tab,Fc,I}).

-define(FCALL_INFO_EMPTY(Ac), {luerl_comp:token_info_internal_created(fcall), fcall,Ac}).
-define(FCALL_INFO(Ac), {Info, fcall,Ac}).

-define(TAIL_FCALL_INFO_EMPTY(Ac), {luerl_comp:token_info_internal_created(tail_fcall), tail_fcall,Ac}).
-define(TAIL_FCALL_INFO(Ac), {Info, tail_fcall,Ac}).

-define(MCALL_INFO_EMPTY(M, Ac), {luerl_comp:token_info_internal_created(mcall), mcall,M,Ac}).
-define(MCALL_INFO(M, Ac), {Info, mcall,M,Ac}).

-define(TAIL_MCALL_INFO_EMPTY(M, Ac), {luerl_comp:token_info_internal_created(tail_mcall), tail_mcall,M,Ac}).
-define(TAIL_MCALL_INFO(M, Ac), {Info, tail_mcall,M,Ac}).

-define(OP_INFO_EMPTY(Op,Ac), {luerl_comp:token_info_internal_created(op), op,Op,Ac}).
-define(OP_INFO(Op,Ac), {Info, op,Op,Ac}).

-define(PUSH_FDEF_INFO_EMPTY(Lsz, Esz, Pars, Is), {luerl_comp:token_info_internal_created(push_fdef), push_fdef,Lsz,Esz,Pars,Is}).
-define(PUSH_FDEF_INFO(LocalFrameSize, EnvironmentFrameSize, FuncParams, Instructions), {Info, push_fdef, LocalFrameSize, EnvironmentFrameSize, FuncParams, Instructions}).
-define(PUSH_FDEF_INFO_NOTUSED(Lsz, Esz, Pars, Is), {_Info, push_fdef,Lsz,Esz,Pars,Is}).

%% Control instructions.

-define(BLOCK_INFO_EMPTY(Lsz, Esz, Is), {luerl_comp:token_info_internal_created(block), block,Lsz,Esz,Is}).
-define(BLOCK_INFO(Lsz, Esz, Is), {Info, block,Lsz,Esz,Is}).
-define(BLOCK_INFO_NOTUSED(Lsz, Esz, Is), {_Info, block,Lsz,Esz,Is}).

-define(WHILE_INFO_EMPTY(E, B), {luerl_comp:token_info_internal_created(while), while,E,B}).
-define(WHILE_INFO(E, B), {Info, while,E,B}).

-define(REPEAT_INFO_EMPTY(B), {luerl_comp:token_info_internal_created(repeat), repeat,B}).
-define(REPEAT_INFO(B), {Info, repeat,B}).

-define(AND_THEN_INFO_EMPTY(T), {luerl_comp:token_info_internal_created(and_then), and_then,T}).
-define(AND_THEN_INFO(T), {Info, and_then,T}).

-define(OR_ELSE_INFO_EMPTY(T), {luerl_comp:token_info_internal_created(or_else), or_else,T}).
-define(OR_ELSE_INFO(T), {Info, or_else,T}).

-define(IF_TRUE_INFO_EMPTY(T), {luerl_comp:token_info_internal_created(if_true), if_true,T}).
-define(IF_TRUE_INFO(T), {Info, if_true,T}).

-define(IF_FALSE_INFO_EMPTY(T), {luerl_comp:token_info_internal_created(if_false), if_false,T}).
-define(IF_FALSE_INFO(T), {Info, if_false,T}).

-define(IF_INFO_EMPTY(T, F), {luerl_comp:token_info_internal_created('if'), 'if',T,F}).
-define(IF_INFO(T, F), {Info, 'if',T,F}).

-define(NFOR_INFO_EMPTY(V, B), {luerl_comp:token_info_internal_created(nfor), nfor,V,B}).
-define(NFOR_INFO(V, B), {Info, nfor,V,B}).

-define(GFOR_INFO_EMPTY(Vs, B), {luerl_comp:token_info_internal_created(gfor), gfor,Vs,B}).
-define(GFOR_INFO(Vs, B), {Info, gfor,Vs,B}).

-define(BREAK_INFO_EMPTY, {luerl_comp:token_info_internal_created(break), break}).
-define(BREAK_INFO, {Info, break}).

-define(RETURN_INFO_EMPTY(Ac), {luerl_comp:token_info_internal_created(return), return,Ac}).
-define(RETURN_INFO(Ac), {Info, return,Ac}).

%% Stack instructions.
-define(PUSH_INFO_EMPTY, {luerl_comp:token_info_internal_created(push), push}).
-define(PUSH_INFO, {Info, push}).

-define(POP_INFO_EMPTY, {luerl_comp:token_info_internal_created(pop), pop}).
-define(POP_INFO, {Info, pop}).
-define(POP_INFO_NOTUSED, {_Info, pop}).

-define(POP2_INFO_EMPTY, {luerl_comp:token_info_internal_created(pop2), pop2}).
-define(POP2_INFO, {Info, pop2}).

-define(SWAP_INFO_EMPTY, {luerl_comp:token_info_internal_created(swap), swap}).
-define(SWAP_INFO, {Info, swap}).

-define(DUP_INFO_EMPTY, {luerl_comp:token_info_internal_created(dup), dup}).
-define(DUP_INFO, {Info, dup}).

-define(PUSH_VALS_EMPTY(Vc), {luerl_comp:token_info_internal_created(push_vals), push_vals,Vc}).
-define(PUSH_VALS_INFO(Vc), {Info, push_vals,Vc}).

-define(POP_VALS_EMPTY(Vc), {luerl_comp:token_info_internal_created(pop_vals), pop_vals,Vc}).
-define(POP_VALS_INFO(Vc), {Info, pop_vals,Vc}).
