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

-module(luerl_scan_tests).

-include_lib("eunit/include/eunit.hrl").

syntax_error_test() ->
    State = luerl:init(),
    ?assertMatch({error, [{1,luerl_scan, {user,"syntax error near '\"'"}}], []}, luerl:do(<<"print(\"hi)">>, State)).

%% Decimal literals are converted in one go, so they are the correctly
%% rounded double: previously 9.9995e-05 was 9.999500000000001e-5 and the
%% smallest subnormal 5e-324 underflowed to 0.0.
float_literal_test_() ->
    Do = fun (Lit) ->
                 {ok,[F],_} = luerl:do_dec(<<"return ", Lit/binary>>, luerl:init()),
                 F
         end,
    [?_assertEqual(9.9995e-5, Do(<<"9.9995e-05">>)),
     ?_assertEqual(3.8364681155805944e25, Do(<<"3.8364681155805944e+25">>)),
     ?_assertEqual(5.0e-324, Do(<<"5e-324">>)),
     ?_assertEqual(2.2250738585072014e-308, Do(<<"2.2250738585072014e-308">>)),
     ?_assertEqual(0.0, Do(<<"1e-400">>)),
     ?_assertEqual(0.5, Do(<<".5">>)),
     ?_assertEqual(5.0, Do(<<"5.">>)),
     ?_assertEqual(100.0, Do(<<"1e2">>)),
     ?_assertEqual(42, Do(<<"42">>))].
