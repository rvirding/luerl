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

{application, luerl,
 [
  {description, "Luerl - an implementation of Lua on Erlang"},
  {vsn, "1"},
  {modules,[luerl.erl,
	    luerl_app.erl,
	    luerl_comp.erl,
	    luerl_comp_cg.erl,
	    luerl_comp_env.erl,
	    luerl_comp_locf.erl,
	    luerl_comp_peep.erl,
	    luerl_comp_vars.erl,
	    luerl_emul.erl,
	    luerl_lib.erl,
	    luerl_lib_basic.erl,
	    luerl_lib_io.erl,
	    luerl_lib_math.erl,
	    luerl_lib_os.erl,
	    luerl_lib_package.erl,
	    luerl_lib_string.erl,
	    luerl_lib_string_format.erl,
	    luerl_parse.yrl,
	    luerl_scan.xrl,
	    luerl_sup.erl,
	    ttdict.erl
	    ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
		  compiler,
		  parsetools
                 ]},
  {mod, {luerl_app, []}},
  {env, []}
 ]}.
