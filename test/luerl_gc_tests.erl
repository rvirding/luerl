%% Copyright (C) 2025 Robert Virding, Dave Lucia
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

-module(luerl_gc_tests).

-include_lib("eunit/include/eunit.hrl").

%% Lua values an application keeps in the private data are roots for the
%% garbage collector: collectgarbage() used to free a table held only
%% there, leaving a dangling reference which a later table could reuse.

private_table_survives_gc_test() ->
    {ok,[T],St0} = luerl:do(<<"return {answer = 42}">>, luerl:init()),
    St1 = luerl:put_private(kept, T, St0),
    {ok,[],St2} = luerl:do(<<"collectgarbage() local filler = {{}, {}, {}}">>, St1),
    Kept = luerl:get_private(kept, St2),
    ?assertEqual([{<<"answer">>,42}], luerl:decode(Kept, St2)).

private_userdata_survives_gc_test() ->
    {U,St0} = luerl_heap:alloc_userdata(my_data, luerl:init()),
    St1 = luerl:put_private(ud, U, St0),
    {ok,[],St2} = luerl:do(<<"collectgarbage()">>, St1),
    ?assertEqual({userdata,my_data}, luerl:decode(luerl:get_private(ud, St2), St2)).

%% Tables nothing refers to are still collected.
unreferenced_table_is_collected_test() ->
    {ok,[T],St0} = luerl:do(<<"return {}">>, luerl:init()),
    {ok,[],St1} = luerl:do(<<"collectgarbage()">>, St0),
    ?assertError(_, luerl:decode(T, St1)).
