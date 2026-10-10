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

-module(luerl_string_format_tests).

-include_lib("eunit/include/eunit.hrl").

%% string.format's float conversions must give what C printf gives for
%% the same double: exact ties rounded half to even from the double's
%% exact value, a two digit exponent, upper case %E/%G and C's %g.

format(Fmt, Arg) ->
    {ok,[S],_} = luerl:call_function_dec([<<"string">>,<<"format">>], [Fmt,Arg], luerl:init()),
    S.

-define(_fmt(Expected, Fmt, Arg), ?_assertEqual(Expected, format(Fmt, Arg))).

f_test_() ->
    [?_fmt(<<"1.2">>, <<"%.1f">>, 1.25),
     ?_fmt(<<"0.12">>, <<"%.2f">>, 0.125),
     ?_fmt(<<"2.67">>, <<"%.2f">>, 2.675),
     ?_fmt(<<"1.00">>, <<"%.2f">>, 1.005),
     ?_fmt(<<"100.0">>, <<"%.1f">>, 99.95),
     ?_fmt(<<"-0.1">>, <<"%.1f">>, -0.05),
     ?_fmt(<<"2">>, <<"%.0f">>, 2.5),
     ?_fmt(<<"4">>, <<"%.0f">>, 3.5),
     ?_fmt(<<"2">>, <<"%.f">>, 1.5),
     ?_fmt(<<"3.141590">>, <<"%f">>, 3.14159),
     ?_fmt(<<"-0.000000">>, <<"%f">>, -0.0),
     ?_fmt(<<"100000000000000000000.00">>, <<"%.2f">>, 1.0e20),
     ?_fmt(<<"-0003.14">>, <<"%+08.2f">>, -3.14159),
     ?_fmt(<<"3.">>, <<"%#.0f">>, 3),
     ?_fmt(<<" 2.2">>, <<"% .1f">>, 2.25),
     ?_fmt(<<"0.1    ">>, <<"%-7.1f">>, 0.05)].

e_test_() ->
    [?_fmt(<<"1.250000e+00">>, <<"%e">>, 1.25),
     ?_fmt(<<"1.2e+00">>, <<"%.1e">>, 1.25),
     ?_fmt(<<"2e+00">>, <<"%.0e">>, 2.5),
     ?_fmt(<<"1.250000E-04">>, <<"%E">>, 0.000125),
     ?_fmt(<<"1.000000e+100">>, <<"%e">>, 1.0e100),
     ?_fmt(<<"4.940656e-324">>, <<"%e">>, 5.0e-324),
     ?_fmt(<<"+0.00e+00">>, <<"%+.2e">>, 0.0),
     ?_fmt(<<"3.e+00">>, <<"%#.0e">>, 3),
     ?_fmt(<<" -3.5000e+00">>, <<"%12.4e">>, -3.5),
     ?_fmt(<<"1.00e+01">>, <<"%.2e">>, 9.999)].

g_test_() ->
    [?_fmt(<<"1.25">>, <<"%g">>, 1.25),
     ?_fmt(<<"0.0001">>, <<"%g">>, 0.0001),
     ?_fmt(<<"1e-05">>, <<"%g">>, 1.0e-5),
     ?_fmt(<<"100">>, <<"%g">>, 100),
     ?_fmt(<<"1.23457e+08">>, <<"%g">>, 123456789),
     ?_fmt(<<"2.67">>, <<"%.3g">>, 2.675),
     ?_fmt(<<"1.00000">>, <<"%#g">>, 1),
     ?_fmt(<<"1E-10">>, <<"%G">>, 1.0e-10),
     ?_fmt(<<"0">>, <<"%.0g">>, 0.0),
     ?_fmt(<<"1e+06">>, <<"%g">>, 999999.5),
     ?_fmt(<<"0.30000000000000004">>, <<"%.17g">>, 0.1 + 0.2)].
