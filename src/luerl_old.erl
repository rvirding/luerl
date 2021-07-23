%% Copyright (c) 2013-2021 Robert Virding
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

%% File    : luerl_old.erl
%% Authors : Robert Virding, Henning Diedrich
%% Purpose : The old basic LUA 5.2 interface.

%% This module is just an interface to the older luerl.erl which might
%% make it easier to migrate the module names if we decide to do so in
%% the future.

-module(luerl_old).

-export(['$handle_undefined_function'/2]).

%% '$handle_undefined_function'(Func, Args)
%%  We just pass the buck and call the old luerl module.

'$handle_undefined_function'(Func, Args) ->
    apply(luerl, Func, Args).
