%% Copyright (c) 2013-2019 Robert Virding
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

%% File    : luerl_comp.hrl
%% Author  : Robert Virding
%% Purpose : Internal LUA 5.2 compiler definitions.

%% Common compiler information

-record(cinfo, {lfile=[],			%Lua file name
		vfile=[],			%Virtual file name
		opts=[]				%Compiler options
	       }).

%% Some useful macros.
-define(IF(Test,True,False), case Test of true -> True; false -> False end).
-define(WHEN_OPT(Opt,Opts,Fun), ?IF(member(Opt, Opts), Fun(), ok)).
-define(DEBUG_PRINT(Format,Args,Opts),
        ?WHEN_OPT(debug_print, Opts,
                  fun () -> io:fwrite(Format, Args) end)).

%% Variable data.
-record(vars, {local=[],			%Local variables
	       free=[],				%Free variables
	       used=[],				%Used in sub blocks
	       fused=[]				%Used in sub-functions
	      }).

%% Define internal data macros.

%% Statements.
%%  The line number here, 'l', can be a line number or annotation list.

-record(assign_stmt, {l,vars,exps}).

-record(call_stmt, {l,call}).

-record(return_stmt, {l,exps}).

-record(break_stmt, {l}).

-record(block_stmt, {l,
		     body=[],			%Block body statements
		     vars=none,			%Variable info
		     lsz=none,			%Local frame size
		     loc=not_used,		%Local var block template
		     esz=none,			%Env frame size
		     env=not_used,		%Local env block template
		     %%local=none,		%Local variables
		     locf=false}).		%Local functions

-record(while_stmt, {l,exp,body=[]}).

-record(repeat_stmt, {l,body=[]}).

-record(nfor_stmt, {l,
		    var,			%Loop variable
		    init,limit,step,		%The init, limit, step values
		    body=[]}).			%Loop body

-record(gfor_stmt, {l,
		    vars,			%Loop variables
		    gens,			%Generators
		    body=[]}).			%Loop body

-record(if_stmt, {l,tests=[],else_block}).

-record(local_assign_stmt, {l,vars,exps}).

-record(local_fdef_stmt, {l,var,func}).

-record(expr_stmt, {l,exp}).			%Pseudo stmt for expressions

-record(block, {l,
		body=[],			%Block body statements
		vars=none,			%Variable info
		lsz=none,			%Local frame size
		loc=not_used,			%Local var block template
		esz=none,			%Env frame size
		env=not_used,			%Local env block template
		locf=false}).

%% Expressions.
%%  The line number here, 'l', can be a line number or annotation list.

-record(fdef, {l,
	       pars=[],				%Parameters
	       body=[],				%Function body statements
	       vars=none,			%Variable info
	       lsz=none,			%Local frame size
	       loc=not_used,			%Local var block template
	       esz=none,			%Env frame size
	       env=not_used,			%Local env block template
	       %%local=none,			%Local variables
	       locf=false}).			%Local function

-record(lit, {l,val}).				%Literal value

-record(op, {l,op,args=[]}).

-record(dot, {l,exp,rest}).

-record(single, {l,exp}).

-record(var, {l,name}).

-record(fcall, {l,args=[]}).			%Function call

-record(mcall, {l,meth,args=[]}).		%Method call

-record(key, {l,key}).

-record(tabcon, {l,fields=[]}).			%Table constructor

-record(efield, {l,val}).

-record(kfield, {l,key,val}).

%% Variable types.
%%  The line number here, 'l', can be a line number or annotation list.

-record(lvar, {l,n,d,i}).			%Local name, depth, index
-record(evar, {l,n,d,i}).			%Environment name, depth, index
-record(gvar, {l,n}).				%Global name
