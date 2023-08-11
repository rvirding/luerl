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

%% File    : lib_date_support.erl
%% Author  : Mark Meeus
%% Purpose : Date Support Module

-module(luerl_lib_os_date).

-export([format/2]).

%% IMPLEMENTED FORMATS:
%% %Y - Year with century, as decimal number (eg. 2007)
%% %y - Year without century, as decimal number (00 - 99)  (eg. 07)
%% %m - Month as decimal number (01 - 12)
%% %d - Day of month as decimal number (01 - 31)
%% %H - Hour in 24-hour format (00 - 23)
%% %M - Minute as decimal number (00 - 59)
%% %S - Second as decimal number (00 - 59)
%% %w - Weekday as decimal number (0 - 6; Sunday is 0)
%% %I - Hour in 12-hour format (01 - 12)
%% %W - Week of year as decimal number, with Monday as first day of week 1 (00 - 53)
%% %x - date (e.g., 09/16/98)
%% %X	- time (e.g., 23:48:10)
%% %c	- date and time (e.g., 09/16/98 23:48:10)
%% %b - Abbreviated month name (eg. Sep)
%% %B - Full month name (eg. September)
%% %a - Abbreviated weekday name (eg. Wed)
%% %A - Full weekday name (eg. Wednesday)
%% %p - Current locale’s A.M./P.M. indicator for 12-hour clock (eg. AM/PM)
%% %% - Percent sign

%% NOT IMPLEMENTED:
%% %j - Day of year as decimal number (001 - 366)
%% %U - Week of year as decimal number, with Sunday as first day of week 1 (00 - 53)
%% %Z - Time-zone name or abbreviation; no characters if time zone is unknown


%% *t format returns a table with the datetime values
format(DateTime, <<"*t">>) ->
    {{Ye,Mo,Da},{Ho,Mi,Sec}} = DateTime,
    [
        {<<"year">>, Ye},
        {<<"month">>, Mo},
        {<<"day">>, Da},
        {<<"hour">>, Ho},
        {<<"min">>, Mi},
        {<<"sec">>, Sec}
    ];

format(DateTime, Format) ->
    Patterns = [
        <<"%Y">>,
        <<"%y">>,
        <<"%m">>,
        <<"%d">>,
        <<"%H">>,
        <<"%M">>,
        <<"%S">>,
        <<"%w">>,
        <<"%I">>,
        <<"%W">>,
        <<"%x">>,
        <<"%X">>,
        <<"%c">>,
        <<"%b">>,
        <<"%B">>,
        <<"%a">>,
        <<"%A">>,
        <<"%p">>,
        <<"%%">>
    ],
    lists:foldl(fun(Pat, Str) -> replace_pattern(Str, DateTime, Pat) end, Format, Patterns).

replace_pattern(Str, DateTime, Pat) ->
    case re:run(Str, Pat) of
        nomatch -> Str;
        {match, _} ->
        {Fmt, Val} = get_pattern_values(Pat, DateTime),
        Formatted = io_lib:fwrite(Fmt,Val),
        re:replace(Str, Pat, Formatted, [{return, binary}, global])
    end.

get_pattern_values(<<"%Y">>, {{Ye, _, _}, _}) -> {"~.4.0w", [Ye]};
get_pattern_values(<<"%y">>, {{Ye, _, _}, _}) -> {"~.2.0w", [Ye rem 100]};
get_pattern_values(<<"%m">>, {{_, Mo, _}, _}) -> {"~.2.0w", [Mo]};
get_pattern_values(<<"%d">>, {{_, _, Da}, _}) -> {"~.2.0w", [Da]};
get_pattern_values(<<"%H">>, {_, {Ho, _, _}}) -> {"~.2.0w", [Ho]};
get_pattern_values(<<"%M">>, {_, {_, Mi, _}}) -> {"~.2.0w", [Mi]};
get_pattern_values(<<"%S">>, {_, {_, _, Sec}}) -> {"~.2.0w", [Sec]};
get_pattern_values(<<"%w">>, {Date, _}) -> {"~.1.0w", [get_day_number(Date)]}; % sun = 0 Sat = 6
get_pattern_values(<<"%I">>, {_, {Ho, _, _}}) -> {"~.2.0w", [get_am_pm_hour(Ho)]};
get_pattern_values(<<"%p">>, {_, {Ho, _, _}}) when Ho < 12 -> {"~s", [<<"AM">>]};
get_pattern_values(<<"%p">>, _) -> {"~s", [<<"PM">>]};
get_pattern_values(<<"%W">>, {Date, _}) -> {"~.1.0w", [element(2, calendar:iso_week_number(Date))]};
get_pattern_values(<<"%x">>, {{Ye, Mo, Da}, _}) -> {"~.2.0w/~.2.0w/~.2.0w", [Mo, Da, Ye rem 100]};
get_pattern_values(<<"%X">>, {_, {Ho, Mi, Sec}}) -> {"~.2.0w:~.2.0w:~.2.0w", [Ho, Mi, Sec]};
get_pattern_values(<<"%c">>, {{Ye, Mo, Da}, {Ho, Mi, Sec}}) -> {"~.2.0w/~.2.0w/~.2.0w ~.2.0w:~.2.0w:~.2.0w", [Mo, Da, Ye rem 100, Ho, Mi, Sec]};
get_pattern_values(<<"%%">>, _) -> {"~1s", ["%"]};

get_pattern_values(<<"%b">>, {{_, 1, _}, _})  -> {"~s", ["Jan"]};
get_pattern_values(<<"%b">>, {{_, 2, _}, _})  -> {"~s", ["Feb"]};
get_pattern_values(<<"%b">>, {{_, 3, _}, _})  -> {"~s", ["Mar"]};
get_pattern_values(<<"%b">>, {{_, 4, _}, _})  -> {"~s", ["Apr"]};
get_pattern_values(<<"%b">>, {{_, 5, _}, _})  -> {"~s", ["May"]};
get_pattern_values(<<"%b">>, {{_, 6, _}, _})  -> {"~s", ["Jun"]};
get_pattern_values(<<"%b">>, {{_, 7, _}, _})  -> {"~s", ["Jul"]};
get_pattern_values(<<"%b">>, {{_, 8, _}, _})  -> {"~s", ["Aug"]};
get_pattern_values(<<"%b">>, {{_, 9, _}, _})  -> {"~s", ["Sep"]};
get_pattern_values(<<"%b">>, {{_, 10, _}, _}) -> {"~s", ["Oct"]};
get_pattern_values(<<"%b">>, {{_, 11, _}, _}) -> {"~s", ["Nov"]};
get_pattern_values(<<"%b">>, {{_, 12, _}, _}) -> {"~s", ["Dec"]};

get_pattern_values(<<"%B">>, {{_, 1, _}, _})  -> {"~s", ["January"]};
get_pattern_values(<<"%B">>, {{_, 2, _}, _})  -> {"~s", ["February"]};
get_pattern_values(<<"%B">>, {{_, 3, _}, _})  -> {"~s", ["March"]};
get_pattern_values(<<"%B">>, {{_, 4, _}, _})  -> {"~s", ["April"]};
get_pattern_values(<<"%B">>, {{_, 5, _}, _})  -> {"~s", ["May"]};
get_pattern_values(<<"%B">>, {{_, 6, _}, _})  -> {"~s", ["June"]};
get_pattern_values(<<"%B">>, {{_, 7, _}, _})  -> {"~s", ["July"]};
get_pattern_values(<<"%B">>, {{_, 8, _}, _})  -> {"~s", ["August"]};
get_pattern_values(<<"%B">>, {{_, 9, _}, _})  -> {"~s", ["September"]};
get_pattern_values(<<"%B">>, {{_, 10, _}, _}) -> {"~s", ["October"]};
get_pattern_values(<<"%B">>, {{_, 11, _}, _}) -> {"~s", ["November"]};
get_pattern_values(<<"%B">>, {{_, 12, _}, _}) -> {"~s", ["December"]};

get_pattern_values(<<"%a">>, {Date, _}) -> {"~s", [get_abbreviated_day_name(get_day_number(Date))]};
get_pattern_values(<<"%A">>, {Date, _}) -> {"~s", [get_day_name(get_day_number(Date))]}.

get_day_number(Date) ->  calendar:day_of_the_week(Date) rem 7.

get_am_pm_hour(0) -> 12;
get_am_pm_hour(H) when H > 12 -> H - 12;
get_am_pm_hour(H) -> H.

get_abbreviated_day_name(0) -> "Sun";
get_abbreviated_day_name(1) -> "Mon";
get_abbreviated_day_name(2) -> "Tue";
get_abbreviated_day_name(3) -> "Wed";
get_abbreviated_day_name(4) -> "Thu";
get_abbreviated_day_name(5) -> "Fri";
get_abbreviated_day_name(6) -> "Sat".

get_day_name(0) -> "Sunday";
get_day_name(1) -> "Monday";
get_day_name(2) -> "Tuesday";
get_day_name(3) -> "Wednesday";
get_day_name(4) -> "Thursday";
get_day_name(5) -> "Friday";
get_day_name(6) -> "Saturday".

