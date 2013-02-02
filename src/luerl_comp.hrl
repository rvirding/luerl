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
		     lsz=none,			%Local frame size
		     lf=[],			%Local frame
		     esz=none,			%Env frame size
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
		lsz=none,			%Local frame size
		lf=[],				%Local frame
		esz=none,			%Env frame size
		ef=[],
		locf=false}).

%% Expressions.
-record(fdef, {l,ps=[],ss=[],
	       vars=none,			%Variable info
	       lsz=none,			%Local frame size
	       lf=[],
	       esz=none,			%Env frame size
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
