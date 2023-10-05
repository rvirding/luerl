%% Copyright (c) 2019 Robert Virding
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
%% Purpose : Internal LUA 5.3 instructions.

%% Expression instructions.
-define(PUSH_LIT(L), {push_lit,L}).
-define(PUSH_LVAR(D,I), {push_lvar,D,I}).
-define(PUSH_EVAR(D, I), {push_evar,D,I}).
-define(PUSH_GVAR(K), {push_gvar,K}).

-define(PUSH_LAST_LIT(L), {push_last_lit,L}).	%[?PUSH_LIT,?MULTIPLE]
-define(PUSH_LAST_LVAR(D,I), {push_last_lvar,D,I}).
-define(PUSH_LAST_EVAR(D, I), {push_last_evar,D,I}).
-define(PUSH_LAST_GVAR(K), {push_last_gvar,K}).

-define(STORE_LVAR(D, I), {store_lvar,D,I}).
-define(STORE_EVAR(D, I), {store_evar,D,I}).
-define(STORE_GVAR(K), {store_gvar,K}).

-define(GET_KEY, get_key).			%Acc = Stk[Acc]
-define(GET_LIT_KEY(K), {get_lit_key,K}).	%[?PUSH_LIT(K),?GET_KEY]
-define(SET_KEY, set_key).			%Stk[
-define(SET_LIT_KEY(K), {set_lit_key,K}).	%[?PUSH_LIT(K),?SET_KEY]

-define(SINGLE, single).			%Ensure single value
-define(MULTIPLE, multiple).			%Ensure multiple value

-define(BUILD_TAB(Fc, I), {build_tab,Fc,I}).
-define(FCALL, fcall).
-define(TAIL_FCALL, tail_fcall).
-define(MCALL(M), {mcall,M}).
-define(TAIL_MCALL(M), {tail_mcall,M}).
-define(OP(Op,Ac), {op,Op,Ac}).
-define(PUSH_FDEF(Anno, Lsz, Esz, Pars, Is),
	{push_fdef,Anno,Lsz,Esz,Pars,Is}).
-define(PUSH_FDEF(FnRef), {push_fdef,FnRef}).

%% Control instructions.
-define(BLOCK(Lsz, Esz, Is), {block,Lsz,Esz,Is}).
-define(BLOCK_OPEN(Lsz, Esz), {block_open,Lsz,Esz}).
-define(BLOCK_CLOSE, block_close).
-define(WHILE(E, B), {while,E,B}).
-define(WHILE_LOOP(Eis, Wis), {while_loop,Eis,Wis}).
-define(REPEAT(B), {repeat,B}).
-define(REPEAT_LOOP(B), {repeat_loop,B}).
-define(AND_THEN(T), {and_then,T}).
-define(OR_ELSE(T), {or_else,T}).
-define(IF_TRUE(T), {if_true,T}).
-define(IF(T, F), {'if',T,F}).
-define(NFOR(V, B), {nfor,V,B}).
-define(NFOR_LOOP(N, Limit, Step, Fis), {nfor_loop,N,Limit,Step,Fis}).
-define(GFOR(Vs, B), {gfor,Vs,B}).
-define(GFOR_CALL(Func, Data, Val, Fis), {gfor_call,Func,Data,Val,Fis}).
-define(GFOR_LOOP(Func, Data, Fis), {gfor_loop,Func,Data,Fis}).
-define(BREAK, break).
-define(RETURN(Ac), {return,Ac}).

%% Stack instructions.
-define(PUSH, push).
-define(POP, pop).
-define(POP2, pop2).
-define(SWAP, swap).
-define(DUP, dup).
-define(PUSH_VALS(Vc), {push_vals,Vc}).
-define(POP_VALS(Vc), {pop_vals,Vc}).
-define(POP_ARGS(Ac), {pop_args,Ac}).
-define(PUSH_ARGS(Al), {push_args,Al}).

%% Comment and line instructions.
-define(COMMENT(C), {comment,C}).
-define(CURRENT_LINE(L, File), {current_line,L,File}).
