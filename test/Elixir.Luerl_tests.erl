%% Copyright (C) 2024 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module('Elixir.Luerl_tests').

-include_lib("eunit/include/eunit.hrl").

private_test() ->
    State1 = 'Elixir.Luerl':init(),
    State2 = 'Elixir.Luerl':put_private(State1, secret, <<"mysecret">>),
    ?assertMatch({ok, <<"mysecret">>}, 'Elixir.Luerl':get_private(State2, secret)),
    ?assertMatch(error, 'Elixir.Luerl':get_private(State2, missing)),
    State3 = 'Elixir.Luerl':delete_private(State2, secret),
    ?assertMatch(error, 'Elixir.Luerl':get_private(State3, secret)).
