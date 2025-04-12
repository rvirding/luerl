%% Copyright (c) 2023 Mark Meeus
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

-module(lib_os_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0]).
-export([
    os_date_formatting/1,
    os_date_table/1,
    os_date_integrated/1,
    os_date_integrated_table/1
]).

all() ->
    [
        {group, date_support},
        {group, integrated}
    ].

groups() ->
    [
        {date_support, [parallel], [os_date_formatting, os_date_table]},
        {integrated, [parallel], [os_date_integrated, os_date_integrated_table]}
    ].

os_date_formatting(_) ->
    Date = {{2023, 1, 2}, {3, 4, 5}},
    ?assertEqual(<<"2023">>, luerl_lib_os_date:format(Date, <<"%Y">>)),
    ?assertEqual(<<"23">>, luerl_lib_os_date:format(Date, <<"%y">>)),
    ?assertEqual(<<"02">>, luerl_lib_os_date:format(Date, <<"%d">>)),
    ?assertEqual(<<"01">>, luerl_lib_os_date:format(Date, <<"%m">>)),
    ?assertEqual(<<"03">>, luerl_lib_os_date:format(Date, <<"%H">>)),
    ?assertEqual(<<"04">>, luerl_lib_os_date:format(Date, <<"%M">>)),
    ?assertEqual(<<"05">>, luerl_lib_os_date:format(Date, <<"%S">>)),
    ?assertEqual(<<"01/02/23">>, luerl_lib_os_date:format(Date, <<"%x">>)),
    ?assertEqual(<<"03:04:05">>, luerl_lib_os_date:format(Date, <<"%X">>)),
    %% Day of week
    ?assertEqual(<<"1">>, luerl_lib_os_date:format(Date, <<"%w">>)),

    %% Hour in 12H
    ?assertEqual(<<"12">>, luerl_lib_os_date:format({{2023, 1, 2}, {0, 0, 0}}, <<"%I">>)),
    ?assertEqual(<<"12">>, luerl_lib_os_date:format({{2023, 1, 2}, {0, 59, 0}}, <<"%I">>)),
    ?assertEqual(<<"01">>, luerl_lib_os_date:format({{2023, 1, 2}, {1, 00, 0}}, <<"%I">>)),
    ?assertEqual(<<"12">>, luerl_lib_os_date:format({{2023, 1, 2}, {12, 00, 0}}, <<"%I">>)),
    ?assertEqual(<<"11">>, luerl_lib_os_date:format({{2023, 1, 2}, {23, 00, 0}}, <<"%I">>)),

    ?assertEqual(<<"AM">>, luerl_lib_os_date:format({{2023, 1, 2}, {0, 0, 0}}, <<"%p">>)),
    ?assertEqual(<<"AM">>, luerl_lib_os_date:format({{2023, 1, 2}, {11, 59, 0}}, <<"%p">>)),
    ?assertEqual(<<"PM">>, luerl_lib_os_date:format({{2023, 1, 2}, {12, 0, 0}}, <<"%p">>)),
    ?assertEqual(<<"PM">>, luerl_lib_os_date:format({{2023, 1, 2}, {23, 59, 0}}, <<"%p">>)),

    %% Hour in 12H
    ?assertEqual(<<"09">>, luerl_lib_os_date:format({{2023, 1, 2}, {9, 0, 0}}, <<"%I">>)),
    %% ISO Week number
    ?assertEqual(<<"01">>, luerl_lib_os_date:format(Date, <<"%W">>)),
    ?assertEqual(<<"Jan">>, luerl_lib_os_date:format(Date, <<"%b">>)),
    ?assertEqual(<<"January">>, luerl_lib_os_date:format(Date, <<"%B">>)),
    ?assertEqual(<<"Mon">>, luerl_lib_os_date:format(Date, <<"%a">>)),
    ?assertEqual(<<"Monday">>, luerl_lib_os_date:format(Date, <<"%A">>)),

    ?assertEqual(<<"%">>, luerl_lib_os_date:format(Date, <<"%%">>)),
    ?assertEqual(
        <<"2023-01-02 03:04:05">>, luerl_lib_os_date:format(Date, <<"%Y-%m-%d %H:%M:%S">>)
    ).

os_date_table(_) ->
    Date = {{2023, 1, 2}, {3, 4, 5}},
    ?assertEqual(
        [
            {<<"year">>, 2023},
            {<<"month">>, 1},
            {<<"day">>, 2},
            {<<"hour">>, 3},
            {<<"min">>, 4},
            {<<"sec">>, 5},
            {<<"wday">>, 2}
        ],
        luerl_lib_os_date:format(Date, <<"*t">>)
    ).

os_date_integrated(_) ->
    State = luerl:init(),
    Chunk = <<"return os.date('noformat'), os.date(), os.date('%c', 1683371767)">>,
    {ok, [NoFormat, _, FromTimeStamp], _State1} = luerl:do(Chunk, State),
    ?assertEqual(<<"noformat">>, NoFormat),
    %% Date is "Sat May  6 13:16:07 2023",
    %% Just check year to avoid test flakiness
    ?assert(re:run(FromTimeStamp, <<"2023">>) =/= nomatch).

os_date_integrated_table(_) ->
    State = luerl:init(),
    Chunk = <<"return os.date('*t').year">>,
    {ok, [Result], _State1} = luerl:do(Chunk, State),
    {{Year, _, _}, _} = calendar:local_time(),
    ?assertEqual(Year, Result).
