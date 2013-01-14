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

%% The code compile info.
-record(code, {code=none,			%Code
	       cst=none				%Shared compiler state
	      }).

%% Compiler state passed between passes.
-record(cst, {}).				%Nothing yet

%% Variable data.
-record(vars, {local=[],free=[],		%Local, free variables
	       used=[],				%Used in sub blocks
	       fused=[]				%Used in sub-functions
	      }).

%% Define internal data macros.

%% Statements.
-record(assign_stmt, {l,vs,es}).

-record(call_stmt, {l,call}).

-record(return_stmt, {l,es}).

-record(break_stmt, {l}).

-record(block_stmt, {l,ss=[],			%Block statement
		     vars=none,			%Variable info
		     lf=[],			%Local frame
		     ef=[],			%Env frame
		     local=none,		%Local variables
		     locf=false}).		%Local functions

-record(while_stmt, {l,e,b=[]}).

-record(repeat_stmt, {l,b=[],e}).

-record(nfor_stmt, {l,v,init,limit,step,b=[]}).

-record(gfor_stmt, {l,vs,gens,b=[]}).

-record(if_stmt, {l,tests=[],else}).

-record(local_assign_stmt, {l,vs,es}).

-record(local_fdef_stmt, {l,v,f}).

-record(block, {l,ss=[],			%Sub-blocks
		vars=none,			%Variable info
		lf=[],				%Local frame
		ef=[]}).

%% Expressions.
-record(fdef, {l,ps=[],ss=[],
	       vars=none,			%Variable info
	       lf=[],
	       ef=[],
	       local=none,			%Local variables
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
-record(lvar, {n,d,i}).				%Local name, depth, index
-record(evar, {n,d,i}).				%Environment name, depth, index
-record(gvar, {n}).				%Global name

%% Instructions.

%% Expression instructions.
-define(LOAD_LIT(L), {load_lit,L}).			%Load a literal
%%-define(LOAD_LVAR(I), {load_lvar,I}).		%Load variable into acc
%%-define(LOAD_FVAR(D, I), {load_fvar,D,I}).

-define(LOAD_LVAR(D, I), {load_lvar,D,I}).
-define(LOAD_EVAR(D, I), {load_evar,D,I}).

-define(LOAD_GVAR(K), {load_gvar,K}).
%%-define(STORE_LVAR(I), {store_lvar,I}).		%Store variable from acc
%%
-define(STORE_FVAR(D, I), {store_fvar,D,I}).

-define(STORE_LVAR(D, I), {store_lvar,D,I}).
-define(STORE_EVAR(D, I), {store_evar,D,I}).

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
%% -define(FDEF(Ps, Is, L, Sz), {fdef,Ps,Is,L,Sz}).

-define(FDEF(Lsz, Lps, Esz, Eps, Is), {fdef,Lsz,Lps,Esz,Eps,Is}).

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
%%-define(PUSH_LVAR(I), {push_lvar,I}).		%?LVAR(I), ?PUSH
%%-define(PUSH_FVAR(D, I), {push_fvar,D,I}).	%?FVAR(D,I), ?PUSH

-define(PUSH_LVAR(D,I), {push_lvar,D,I}).	%?LVAR(D,I), ?PUSH
-define(PUSH_EVAR(D, I), {push_evar,D,I}).	%?EVAR(D,I), ?PUSH

-define(PUSH_GVAR(K), {push_gvar,K}).		%?GVAR(K), ?PUSH

%% -record(push_vals,{ac}).
%% #push_vals{ac=Ac}
%% ?PUSH_VALS(Ac)
