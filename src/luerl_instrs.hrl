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
-define(LOAD_LIT(L), {load_lit,L}).			%Load a literal
-define(LOAD_LVAR(D, I), {load_lvar,D,I}).
-define(LOAD_EVAR(D, I), {load_evar,D,I}).
-define(LOAD_GVAR(K), {load_gvar,K}).

-define(STORE_LVAR(D, I), {store_lvar,D,I}).
-define(STORE_EVAR(D, I), {store_evar,D,I}).
-define(STORE_GVAR(K), {store_gvar,K}).

-define(GET_KEY, get_key).			%Acc = Stk[Acc]
-define(GET_LIT_KEY(K), {get_lit_key,K}).	%[?PUSH,?LIT(K),?KEY]
-define(SET_KEY, set_key).			%Stk[
-define(SET_LIT_KEY(K), {set_lit_key,K}).	%[?PUSH,?LIT(K),?SET_KEY]

-define(SINGLE, single).			%Ensure single value
-define(MULTIPLE, multiple).			%Ensure multiple value

-define(BUILD_TAB(Fc, I), {build_tab,Fc,I}).
-define(CALL(Ac), {call,Ac}).
-define(TAIL_CALL(Ac), {tail_call,Ac}).
-define(OP(Op,Ac), {op,Op,Ac}).
-define(FDEF(Lsz, Esz, Pars, Is), {fdef,Lsz,Esz,Pars,Is}).

%% Control instructions.

-define(BLOCK(Lsz, Esz, Is), {block,Lsz,Esz,Is}).

-define(WHILE(E, B), {while,E,B}).
-define(REPEAT(B), {repeat,B}).
-define(IF_TRUE(T), {if_true,T}).
-define(IF_FALSE(T), {if_false,T}).
-define(IF(T, F), {'if',T,F}).
-define(NFOR(V, B), {nfor,V,B}).
-define(GFOR(Vs, B), {gfor,Vs,B}).
-define(BREAK, break).
-define(RETURN(Ac), {return,Ac}).
%% Stack instructions, mainly to/from accumulator.
-define(PUSH, push).
-define(POP, pop).
-define(DROP, drop).
-define(SWAP, swap).
-define(PUSH_VALS(Vc), {push_vals,Vc}).
-define(POP_VALS(Vc), {pop_vals,Vc}).
%% Optimisations and combined instructions.
-define(PUSH_LIT(L), {push_lit,L}).		%?LIT(L), ?PUSH
-define(PUSH_LVAR(D,I), {push_lvar,D,I}).	%?LVAR(D,I), ?PUSH
-define(PUSH_EVAR(D, I), {push_evar,D,I}).	%?EVAR(D,I), ?PUSH
-define(PUSH_GVAR(K), {push_gvar,K}).		%?GVAR(K), ?PUSH
