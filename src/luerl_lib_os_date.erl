%% Copyright (c) 2023-2024 Robert Virding
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

-include("luerl.hrl").

?MODULEDOC(false).

-export([format/2]).

%% IMPLEMENTED FORMATS:
%% %A - Full weekday name (eg. Wednesday)
%% %a - Abbreviated weekday name (eg. Wed)
%% %B - Full month name (eg. September)
%% %b - Abbreviated month name (eg. Sep)
%% %c - date and time (e.g. Wed Jan 24 12:27:59 2024)
%% %D - date (e.g. 09/16/98)
%% %d - Day of month as decimal number (01 - 31)
%% %e - Day of month as decimal number ( 1 - 31)
%% %F - date (e.g. 2024-01-31)
%% %H - Hour in 24-hour format (00 - 23)
%% %I - Hour in 12-hour format (01 - 12)
%% %M - Minute as decimal number (00 - 59)
%% %m - Month as decimal number (01 - 12)
%% %n - Newline character
%% %P - As %p but lowercase
%% %p - Current locale’s A.M./P.M. indicator for 12-hour clock (eg. AM/PM)
%% %S - Second as decimal number (00 - 59)
%% %T - time (e.g. 23:48:10)
%% %t - Tab character
%% %u - Weekday as decimal number (1 - 7; Monday is 1)
%% %V - IOS week number (01 - 53)
%% %W - Week of year as decimal number, with Monday as first day of week 1 (00 - 53)
%% %w - Weekday as decimal number (0 - 6; Sunday is 0)
%% %X - time (e.g., 23:48:10)
%% %x - date (e.g., 09/16/98)
%% %Y - Year with century, as decimal number (eg. 2007)
%% %y - Year without century, as decimal number (00 - 99)  (eg. 07)
%% %Z - Time-zone name or abbreviation; no characters if time zone is unknown
%% %% - Percent sign

%% NOT IMPLEMENTED:
%% %j - Day of year as decimal number (001 - 366)
%% %U - Week of year as decimal number, with Sunday as first day of week 1 (00 - 53)


%% format(DateTime, Format) -> Formatted.
%%  *t format returns a table with the datetime values.
format(DateTime, <<"*t">>) ->
    {{Ye,Mo,Da}=Date,{Ho,Mi,Sec}} = DateTime,
    [
     {<<"year">>, Ye},
     {<<"month">>, Mo},
     {<<"day">>, Da},
     {<<"hour">>, Ho},
     {<<"min">>, Mi},
     {<<"sec">>, Sec},
     {<<"wday">>, get_day_number(Date)+1}
    ];
format(DateTime, Format) ->
    Patterns = [
        <<"%A">>,
        <<"%a">>,
        <<"%B">>,
        <<"%b">>,
        <<"%c">>,
        <<"%D">>,
        <<"%d">>,
        <<"%e">>,
        <<"%F">>,
        <<"%H">>,
        <<"%I">>,
        <<"%M">>,
        <<"%m">>,
        <<"%n">>,
        <<"%P">>,
        <<"%p">>,
        <<"%S">>,
        <<"%T">>,
        <<"%t">>,
        <<"%u">>,
        <<"%V">>,
        <<"%W">>,
        <<"%w">>,
        <<"%X">>,
        <<"%x">>,
        <<"%Y">>,
        <<"%y">>,
        <<"%Z">>,
        <<"%%">>
    ],
    lists:foldl(fun(Pat, Str) -> replace_pattern(Str, DateTime, Pat) end,
                Format, Patterns).

replace_pattern(Str, DateTime, Pat) ->
    case re:run(Str, Pat) of
        nomatch -> Str;
        {match, _} ->
            {Format, Val} = get_pattern_values(Pat, DateTime),
            Formatted = io_lib:fwrite(Format, Val),
            re:replace(Str, Pat, Formatted, [{return, binary}, global])
    end.

%% get_pattern_values(Patters, DateTime) -> {FormatString, Values}.

get_pattern_values(<<"%A">>, {Date, _}) ->
    {"~s", [get_day_name(get_day_number(Date))]};
get_pattern_values(<<"%a">>, {Date, _}) ->
    {"~s", [get_abbreviated_day_name(get_day_number(Date))]};
get_pattern_values(<<"%B">>, {{_, Mo, _}, _}) ->
    {"~s", [get_month_name(Mo)]};
get_pattern_values(<<"%b">>, {{_, Mo, _}, _}) ->
    {"~s", [get_abbreviated_month_name(Mo)]};
get_pattern_values(<<"%c">>, {{Ye, Mo, Da}=Date, {Ho, Mi, Sec}}) ->
    {"~s ~s ~.2w ~.2.0w:~.2.0w:~.2.0w ~.4.0w",
        [get_abbreviated_day_name(get_day_number(Date)),
         get_abbreviated_month_name(Mo),Da,
         Ho,Mi,Sec,
         Ye]};
get_pattern_values(<<"%D">>, {{Ye, Mo, Da}, _}) ->
    {"~.2.0w/~.2.0w/~.2.0w", [Mo, Da, Ye rem 100]};
get_pattern_values(<<"%d">>, {{_, _, Da}, _}) -> {"~.2.0w", [Da]};
get_pattern_values(<<"%e">>, {{_, _, Da}, _}) -> {"~.2w", [Da]};
get_pattern_values(<<"%F">>, {{Ye, Mo, Da}, _}) ->
    {"~.4.0w-~.2.0w-~.2.0w", [Ye, Mo, Da]};
get_pattern_values(<<"%H">>, {_, {Ho, _, _}}) -> {"~.2.0w", [Ho]};
get_pattern_values(<<"%I">>, {_, {Ho, _, _}}) ->
    {"~.2.0w", [get_am_pm_hour(Ho)]};
get_pattern_values(<<"%M">>, {_, {_, Mi, _}}) -> {"~.2.0w", [Mi]};
get_pattern_values(<<"%m">>, {{_, Mo, _}, _}) -> {"~.2.0w", [Mo]};
get_pattern_values(<<"%n">>, _) -> {"\n", []};
get_pattern_values(<<"%P">>, {_, {Ho, _, _}}) ->
    Val = if Ho < 12 -> <<"am">>;
             true -> <<"pm">>
          end,
    {"~s", [Val]};
get_pattern_values(<<"%p">>, {_, {Ho, _, _}}) ->
    Val = if Ho < 12 -> <<"AM">>;
             true -> <<"PM">>
          end,
    {"~s", [Val]};
get_pattern_values(<<"%S">>, {_, {_, _, Sec}}) -> {"~.2.0w", [Sec]};
get_pattern_values(<<"%T">>, {_, {Ho, Mi, Sec}}) ->
    {"~.2.0w:~.2.0w:~.2.0w", [Ho, Mi, Sec]};
get_pattern_values(<<"%t">>, _) -> {"\t", []};
get_pattern_values(<<"%u">>, {Date, _}) ->
    {"~.1.0w", [calendar:day_of_the_week(Date)]}; % Mon = 1 Sun = 7
get_pattern_values(<<"%V">>, {Date, _}) ->
    {_Year,Week} = calendar:iso_week_number(Date),
    {"~.2.0w", [Week]};
get_pattern_values(<<"%W">>, {Date, _}) ->
    {_Year,Week} = calendar:iso_week_number(Date),
    {"~.2.0w", [Week]};
get_pattern_values(<<"%w">>, {Date, _}) ->
    {"~.1.0w", [get_day_number(Date)]};         % Sun = 0 Sat = 6
get_pattern_values(<<"%X">>, {_, {Ho, Mi, Sec}}) ->
    {"~.2.0w:~.2.0w:~.2.0w", [Ho, Mi, Sec]};
get_pattern_values(<<"%x">>, {{Ye, Mo, Da}, _}) ->
    {"~.2.0w/~.2.0w/~.2.0w", [Mo, Da, Ye rem 100]};
get_pattern_values(<<"%Y">>, {{Ye, _, _}, _}) -> {"~.4.0w", [Ye]};
get_pattern_values(<<"%y">>, {{Ye, _, _}, _}) -> {"~.2.0w", [Ye rem 100]};
get_pattern_values(<<"%Z">>, _) -> {"", []};
get_pattern_values(<<"%%">>, _) -> {"~c", [$%]}.

%% get_day_number(Date) -> DayNumber.
%%  This is US so Sunday is day 1.

get_day_number(Date) -> calendar:day_of_the_week(Date) rem 7.

get_am_pm_hour(0) -> 12;
get_am_pm_hour(H) when H > 12 -> H - 12;
get_am_pm_hour(H) -> H.

get_month_name(1) -> "January";
get_month_name(2) -> "February";
get_month_name(3) -> "March";
get_month_name(4) -> "April";
get_month_name(5) -> "May";
get_month_name(6) -> "June";
get_month_name(7) -> "July";
get_month_name(8) -> "August";
get_month_name(9) -> "September";
get_month_name(10) -> "October";
get_month_name(11) -> "November";
get_month_name(12) -> "December".

get_abbreviated_month_name(1) -> "Jan";
get_abbreviated_month_name(2) -> "Feb";
get_abbreviated_month_name(3) -> "Mar";
get_abbreviated_month_name(4) -> "Apr";
get_abbreviated_month_name(5) -> "May";
get_abbreviated_month_name(6) -> "Jun";
get_abbreviated_month_name(7) -> "Jul";
get_abbreviated_month_name(8) -> "Aug";
get_abbreviated_month_name(9) -> "Sep";
get_abbreviated_month_name(10) -> "Oct";
get_abbreviated_month_name(11) -> "Nov";
get_abbreviated_month_name(12) -> "Dec".

get_day_name(0) -> "Sunday";
get_day_name(1) -> "Monday";
get_day_name(2) -> "Tuesday";
get_day_name(3) -> "Wednesday";
get_day_name(4) -> "Thursday";
get_day_name(5) -> "Friday";
get_day_name(6) -> "Saturday".

get_abbreviated_day_name(0) -> "Sun";
get_abbreviated_day_name(1) -> "Mon";
get_abbreviated_day_name(2) -> "Tue";
get_abbreviated_day_name(3) -> "Wed";
get_abbreviated_day_name(4) -> "Thu";
get_abbreviated_day_name(5) -> "Fri";
get_abbreviated_day_name(6) -> "Sat".
