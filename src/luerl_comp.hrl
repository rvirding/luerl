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

%Lua file name
-record(cinfo, {
    lfile = [],
    %Virtual file name
    vfile = [],
    %Compiler options
    opts = []
}).

%% Some useful macros.
-define(IF(Test, True, False),
    case Test of
        true -> True;
        false -> False
    end
).
-define(WHEN_OPT(Opt, Opts, Fun), ?IF(member(Opt, Opts), Fun(), ok)).
-define(DEBUG_PRINT(Format, Args, Opts),
    ?WHEN_OPT(
        debug_print,
        Opts,
        fun() -> io:fwrite(Format, Args) end
    )
).

%% Variable data.

%Local variables
-record(vars, {
    local = [],
    %Free variables
    free = [],
    %Used in sub blocks
    used = [],
    %Used in sub-functions
    fused = []
}).

%% Define internal data macros.

%% Statements.
%%  The line number here, 'l', can be a line number or annotation list.

-record(assign_stmt, {l, vars, exps}).

-record(call_stmt, {l, call}).

-record(return_stmt, {l, exps}).

-record(break_stmt, {l}).

-record(block_stmt, {
    l,
    %Block body statements
    body = [],
    %Variable info
    vars = none,
    %Local frame size
    lsz = none,
    %Local var block template
    loc = not_used,
    %Env frame size
    esz = none,
    %Local env block template
    env = not_used,
    %%local=none,		%Local variables

    %Local functions
    locf = false
}).

-record(while_stmt, {l, exp, body = []}).

-record(repeat_stmt, {l, body = []}).

-record(nfor_stmt, {
    l,
    %Loop variable
    var,
    %The init, limit, step values
    init,
    limit,
    step,
    %Loop body
    body = []
}).

-record(gfor_stmt, {
    l,
    %Loop variables
    vars,
    %Generators
    gens,
    %Loop body
    body = []
}).

-record(if_stmt, {l, tests = [], else_block}).

-record(local_assign_stmt, {l, vars, exps}).

-record(local_fdef_stmt, {l, var, func}).

%Pseudo stmt for expressions
-record(expr_stmt, {l, exp}).

-record(block, {
    l,
    %Block body statements
    body = [],
    %Variable info
    vars = none,
    %Local frame size
    lsz = none,
    %Local var block template
    loc = not_used,
    %Env frame size
    esz = none,
    %Local env block template
    env = not_used,
    locf = false
}).

%% Expressions.
%%  The line number here, 'l', can be a line number or annotation list.

-record(fdef, {
    l,
    %Parameters
    pars = [],
    %Function body statements
    body = [],
    %Variable info
    vars = none,
    %Local frame size
    lsz = none,
    %Local var block template
    loc = not_used,
    %Env frame size
    esz = none,
    %Local env block template
    env = not_used,
    %%local=none,			%Local variables

    %Local function
    locf = false
}).

%Literal value
-record(lit, {l, val}).

-record(op, {l, op, args = []}).

-record(dot, {l, exp, rest}).

-record(single, {l, exp}).

-record(var, {l, name}).

%Function call
-record(fcall, {l, args = []}).

%Method call
-record(mcall, {l, meth, args = []}).

-record(key, {l, key}).

%Table constructor
-record(tabcon, {l, fields = []}).

-record(efield, {l, val}).

-record(kfield, {l, key, val}).

%% Variable types.
%%  The line number here, 'l', can be a line number or annotation list.

%Local name, depth, index
-record(lvar, {l, n, d, i}).
%Environment name, depth, index
-record(evar, {l, n, d, i}).
%Global name
-record(gvar, {l, n}).
