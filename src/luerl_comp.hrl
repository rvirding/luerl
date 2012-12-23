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

%% File    : luerl_comp.hrl
%% Author  : Robert Virding
%% Purpose : Internal LUA 5.2 compiler definitions.

%% The chunk compile info.

-record(chunk, {code=none,			%Code
		fs=[],				%Variable frames
		locv=false,			%Local variables
		locf				%Local frame
	       }).

%% Define internal data macros.

%% Statements.
-record(assign, {l,vs,es}).

-record(return, {l,es}).

-record(break, {l}).

-record(block, {l,ss=[],
		local=[],free=[],		%Local, free variables
		used=[],			%Variables in sub blocks
		locf=false}).			%Local function

-record(while, {l,e,b=[]}).

-record(repeat, {l,b=[],e}).

-record(nfor, {l,v,init,limit,step,b=[]}).

-record(gfor, {l,vs,gens,b=[]}).

-record('if', {l,tests=[],else}).

-record(local_assign, {l,vs,es}).

-record(local_fdef, {l,v,f}).

%% Expressions.
-record(fdef, {l,ps=[],ss=[],
	       local=[],free=[],		%Local, free variables
	       used=[],				%Variables in sub blocks
	       locf=false}).			%Local function

-record(lit, {l,v}).

-record(op, {l,op,as=[]}).

-record(single, {l,e}).

-record(dot, {l,e,r}).

-record(var, {l,n}).

-record(fcall, {l,as=[]}).

-record(mcall, {l,m,as=[]}).

-record(key, {l,k}).

-record(tc, {l,fs=[]}).

-record(efield, {l,v}).

-record(kfield, {l,k,v}).

%% Variable types.
-record(lvar, {n,i}).				%Local name, index
-record(fvar, {n,d,i}).				%Frame name, depth, index
-record(gvar, {n}).				%Global name

%% Instructions.

%% Expression instructions.
-define(LOAD_LIT(L), {load_lit,L}).			%Load a literal
-define(LOAD_LVAR(I), {load_lvar,I}).		%Load variable into acc
-define(LOAD_FVAR(D, I), {load_fvar,D,I}).
-define(LOAD_GVAR(K), {load_gvar,K}).
-define(STORE_LVAR(I), {store_lvar,I}).		%Store variable from acc
-define(STORE_FVAR(D, I), {store_fvar,D,I}).
-define(STORE_GVAR(K), {store_gvar,K}).
-define(SINGLE, single).
-define(GET_KEY, get_key).			%Acc = Stk[Acc]
-define(GET_LIT_KEY(K), {get_key,K}).		%[?PUSH,?LIT(K),?KEY]
-define(SET_KEY, set_key).			%Stk[
-define(SET_LIT_KEY(K), {set_key,K}).		%[?PUSH,?LIT(K),?SET_KEY]
-define(BUILD_TAB(Fc), {build_tab,Fc}).
-define(CALL(Ac), {call,Ac}).
-define(TAIL_CALL(Ac), {tail_call,Ac}).
-define(OP(Op,Ac), {op,Op,Ac}).
-define(FDEF(Ps, Is, L, Sz), {fdef,Ps,Is,L,Sz}).
%% Control instructions.
-define(BLOCK(Is, L, Sz), {block,Is,L,Sz}).
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
-define(PUSH_LVAR(I), {push_lvar,I}).		%?LVAR(I), ?PUSH
-define(PUSH_FVAR(D, I), {push_fvar,D,I}).	%?FVAR(D,I), ?PUSH
-define(PUSH_GVAR(K), {push_gvar,K}).		%?GVAR(K), ?PUSH

%% -record(push_vals,{ac}).
%% #push_vals{ac=Ac}
%% ?PUSH_VALS(Ac)
