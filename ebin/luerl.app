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
  {modules,[
            luerl,
            luerl_app,
            luerl_comp,
            luerl_comp_cg,
            luerl_comp_env,
            luerl_comp_locf,
            luerl_comp_peep,
            luerl_comp_vars,
            luerl_emul,
            luerl_lib,
            luerl_lib_basic,
            luerl_lib_bit32,
            luerl_lib_io,
            luerl_lib_math,
            luerl_lib_os,
            luerl_lib_package,
            luerl_lib_string,
            luerl_lib_string_format,
            luerl_lib_table,
            luerl_parse,
            luerl_scan,
            luerl_sup,
            ttdict
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
