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

%% File    : luerl_comp.hrl
%% Author  : Robert Virding
%% Purpose : Internal LUA 5.2 compiler definitions.

%% The code compile info.
-record(code, {code=none,			%Code
	       cst=none				%Shared compiler state
	      }).

%% Compiler state passed between passes.
%%  This includes necessary things from the compiler state.
-record(cst, {opts=[],				%Keep the options here as well
	      lua_file=""				%The lua file name
	     }).

%% Variable data.
-record(vars, {local=[],free=[],		%Local, free variables
	       used_in_sub_blocks = [],				%Used in sub blocks
	       used_in_sub_funcs = []				%Used in sub-functions
	      }).

%% Define internal data macros.

%% Statements.
%% before Info map here was line number or annotation list
%%  The line number here, 'l', can be a line number or annotation list.

-record(assign_stmt, {info,variable_statement,expressions}).

-record(call_stmt, {info,call}).

-record(return_stmt, {info, expressions}).

-record(break_stmt, {info}).

-record(block_stmt, {info,
		     block_statement = [],			%Block statements
		     vars = none,			%Variable info
		     local_frame_size = none,			%Local frame size
		     local_frame = [],			%Local frame
		     environment_frame_size = none,			%Env frame size
		     environment_frame = [],			%Env frame
		     local_variables = none,		%Local variables
		     local_functions = false}).		%Local functions

-record(while_stmt, {info,expression,block=[]}).

-record(repeat_stmt, {info,block=[]}).

-record(nfor_stmt, {info,v,init,limit,step,block=[]}).

-record(gfor_stmt, {info,vs,gens,block=[]}).

-record(if_stmt, {info,tests=[],else}).

-record(local_assign_stmt, {info,vs, expressions}).

-record(local_fdef_stmt, {info,v,f}).

-record(expr_stmt, {info,exp}).			%Pseudo stmt for expressions

-record(block, {info,
		sub_blocks =[],				%Sub-blocks
		vars=none,			%Variable info
		local_frame_size=none,			%Local frame size
		local_frame=[],				%Local frame
		environment_frame_size=none,			%Env frame size
		environment_frame=[],
		local_functions=false}).

%% Expressions.
%%  The line number here, 'l', can be a line number or annotation list.

-record(fdef, {info,
	       func_parameters =[],				%Parameters
	       statements =[],				%Statements
	       vars=none,			%Variable info
	       local_frame_size =none,			%Local frame size
	       local_frame =[],
	       environment_frame_size =none,			%Env frame size
	       environment_frame =[],
	       local_variables=none,			%Local variables
	       local_function=false}).			%Local function

-record(lit, {info,v}).

-record(op, {info,op,arguments=[]}).

-record(single, {info,e}).

-record(dot, {info,e,r}).

-record(var, {info, name}).

-record(fcall, {info,as=[]}).

-record(method_call, {info,m,as=[]}).

-record(key, {info, key}).

-record(table_constructor, {info, fields =[]}).

-record(efield, {info, value}).

-record(kfield, {info, key, value}).

%% Variable types.
-record(lvar, {name, depth, index}).				%Local name, depth, index
-record(evar, {name, depth, index}).				%Environment name, depth, index
-record(gvar, {name}).				%Global name

-record(info_structure, {source_file,
	                       linenum,
	                       token_position_in_line,
	                       original_token_description,
	                       internal_statement}).
