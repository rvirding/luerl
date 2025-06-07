%% Copyright (c) 2013-2025 Robert Virding
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

%% File    : luerl_lib_string_format.erl
%% Author  : Robert Virding
%% Purpose : The string formatting for Luerl.

-module(luerl_lib_string_format).

-include("luerl.hrl").

?MODULEDOC(false).

-export([format/3]).

-import(luerl_lib, [lua_error/2,badarg_error/3]). %Shorten this

%% Luerl definitions of these types.
-define(WHITE_SPACE(C), (is_integer(C) andalso C >= $\000 andalso C =< $\s)).
-define(UPPER(C), (C >= $A andalso C =< $Z)).
-define(LOWER(C), (C >= $a andalso C =< $z)).
-define(ASCII(C), (C >= 0 andalso C =< 127)).
-define(DIGIT(C), (C >= $0 andalso C =< $9)).
-define(HEX(C), (C >= $A andalso C =< $F orelse
                 C >= $a andalso C =< $f orelse
                 ?DIGIT(C))).

format(F, As, St0) ->
    {Str,St1} = format_loop(luerl_lib:arg_to_list(F), As, St0),
    %% io:format("f ~w\n", [iolist_to_binary(Str)]),
    {[iolist_to_binary(Str)],St1}.
    %%{[unicode:characters_to_binary(Str)],St1}.

format_loop(Fmt, As, St) -> format_loop(Fmt, As, St, []).

format_loop([$%|Fmt0], As0, St0, Acc) ->
    {Format,Fmt1} = collect(Fmt0),
    {Out,As1,St1} = build(Format, As0, St0),
    format_loop(Fmt1, As1, St1, [Out|Acc]);
format_loop([$\\,C|Fmt], As, St, Acc) ->
    format_loop(Fmt, As, St, [C|Acc]);
format_loop([C|Fmt], As, St, Acc) ->
    format_loop(Fmt, As, St, [C|Acc]);
format_loop([], _, St, Acc) ->                 %Ignore extra arguments
    {lists:reverse(Acc),St}.

%% collect(Format) -> {{C,Flags,Field,Precision},Format}.
%%  Collect a conversion specification.
%%  %[flags][width][.precision][conversion]

collect(Fmt0) ->
    {Fl,Fmt1} = flags(Fmt0),                    %The flags characters
    {Fw,Fmt2} = field_width(Fmt1),              %The field width
    {P,Fmt3} = precision(Fmt2),                 %The precision
    {C,Fmt4} = collect_cc(Fmt3),                %The control character
    %% io:format("col C=~w Fl=~.2b Fw=~w P=~w\n", [C,Fl,Fw,P]),
    {{C,Fl,Fw,P},Fmt4}.

%% Handling the flags of a format.
%% Yes, we should use a tuple or record, but this is much more fun.

-define(FL_NONE, 0).
-define(FL_H, 2#00001).                         %# flag
-define(FL_Z, 2#00010).                         %0 flag
-define(FL_M, 2#00100).                         %- flag
-define(FL_S, 2#01000).                         %space flag
-define(FL_P, 2#10000).                         %+ flag

-define(SET_FLAG(FL,B), (FL bor (B))).
-define(FLAG_SET(FL, B), ((FL band (B)) =/= 0)).
-define(FLAG_CLR(FL, B), ((FL band (B)) =:= 0)).

flags(Fmt) -> flags(Fmt, ?FL_NONE).

flags([$#|Fmt], Fl) -> flags(Fmt, ?SET_FLAG(Fl, ?FL_H));
flags([$0|Fmt], Fl) -> flags(Fmt, ?SET_FLAG(Fl, ?FL_Z));
flags([$-|Fmt], Fl) -> flags(Fmt, ?SET_FLAG(Fl, ?FL_M));
flags([$\s|Fmt], Fl) -> flags(Fmt, ?SET_FLAG(Fl, ?FL_S));
flags([$+|Fmt], Fl) -> flags(Fmt, ?SET_FLAG(Fl, ?FL_P));
flags(Fmt, Fl) -> {Fl,Fmt}.

field_width(Fmt) ->
    %% io:format("fv ~s\n", [Fmt]),
    field_value(Fmt).

precision([$.|Fmt]) -> field_value(Fmt);
precision(Fmt) -> {none,Fmt}.

collect_cc([C|Fmt]) -> {C,Fmt};
collect_cc([]) -> {none,[]}.

field_value([C|_]=Fmt) when ?DIGIT(C) -> field_value(Fmt, 0);
field_value(Fmt) -> {none,Fmt}.

field_value([C|Fmt], F) when ?DIGIT(C) ->
    field_value(Fmt, 10*F + (C - $0));
field_value(Fmt, F) -> {F,Fmt}.

%% build({Conversion,Flags,FieldWidth,Precision}, Args) -> {Out,Args}.
%%  Build a string from the conversion specification.
%%  Implemented conversions are d,i o,u,x,X e,E f,F g,G c s %.
%%  No length modifiers, h L l, no conversions n p S C allowed.

%% Integer formats.
build({$c,Fl,Fw,_P}, [A|As], St) ->
    N = luerl_lib:arg_to_integer(A),
    C = N band 255,
    {adjust_str([C], Fl, Fw),As,St};
build({Conv,Fl,Fw,P}, [A|As], St) when Conv =:= $d ; Conv =:= $i ->
    I = luerl_lib:arg_to_integer(A),
    {format_decimal(Fl, Fw, P, I),As,St};
build({$u,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_integer(A),
    {format_unsigned(Fl, Fw, P, N),As,St};
build({$o,Fl,Fw,P}, [A|As], St) ->
    I = luerl_lib:arg_to_integer(A),
    {format_octal(Fl, Fw, P, I),As,St};
build({$x,Fl,Fw,P}, [A|As], St) ->
    I = luerl_lib:arg_to_integer(A),
    {format_hex(Fl, Fw, P, I),As,St};
build({$X,Fl,Fw,P}, [A|As], St) ->
    I = luerl_lib:arg_to_integer(A),
    {format_HEX(Fl, Fw, P, I),As,St};
%% Float formats.
build({$e,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_float(A),
    {format_e_float(Fl, Fw, P, N),As,St};
build({$E,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_float(A),
    {format_e_float(Fl, Fw, P, N),As,St};
build({$f,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_float(A),
    {format_f_float(Fl, Fw, P, N),As,St};
build({$F,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_float(A),
    {format_f_float(Fl, Fw, P, N),As,St};
build({$g,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_float(A),
    {format_g_float(Fl, Fw, P, N),As,St};
build({$G,Fl,Fw,P}, [A|As], St) ->
    N = luerl_lib:arg_to_float(A),
    {format_g_float(Fl, Fw, P, N),As,St};
%% %p
build({$q,Fl,Fw,P}, [A|As], St) ->
    if Fl =/= 0, Fw =/= none, P =/= none ->
            badarg_error(format, ['q',A], St);
       true ->
            S = build_q(A, St),
            %% io:format("q ~w\n  ~w\n", [A, S]),
            {S,As,St}
    end;
build({$s,Fl,Fw,P}, [A|As], St0) ->
    {S0,St1} = luerl_lib:tostring(A, St0),
    S1 = trim_bin(S0, P),
    {adjust_bin(S1, Fl, Fw),As,St1};
%% Literal % format.
build({$%,?FL_NONE,none,none}, As, St) ->       %No flags, field or precision!
    {"%",As,St}.

%% format_decimal(Flags, Field, Precision, Number) -> String.
%% format_octal(Flags, Field, Precision, Number) -> String.
%% format_hex(Flags, Field, Precision, Number) -> String.
%% format_HEX(Flags, Field, Precision, Number) -> String.
%% format_integer(Flags, Field, Precision, Number, String) -> String.
%%  Print integer Number with base Base. This is a bit messy as we are
%%  following string.format handling.

format_unsigned(Fl, F, P, N) ->
    %% We have to make the number always positive so we are assuming
    %% 124 bit integers which matches Lua 5.3.
    I = if N >= 0 -> N;
           true -> N band 16#FFFFFFFFFFFFFFFF
        end,
    Str = integer_to_list(I),
    format_integer(Fl, F, P, I, Str).

format_decimal(Fl, F, P, N) ->
    Str = integer_to_list(abs(N), 10),
    format_integer(Fl, F, P, N, Str).

format_octal(Fl, F, P, N) ->
    Str = integer_to_list(abs(N), 8),
    format_integer(Fl, F, P, N, Str).

format_hex(Fl, F, P, N) ->
    Str = lists:flatten(io_lib:fwrite("~.16b", [abs(N)])),
    format_integer(Fl, F, P, N, Str).

format_HEX(Fl, F, P, N) ->
    Str = lists:flatten(io_lib:fwrite("~.16B", [abs(N)])),
    format_integer(Fl, F, P, N, Str).

format_integer(Fl, F, P, N, Str0) ->
    Sign = sign(Fl, N),
    if P =/= none ->
            Str1 = Sign ++ lists:flatten(adjust_str(Str0, ?FL_Z, P)),
            adjust_str(Str1, (Fl band ?FL_M), F);
       ?FLAG_SET(Fl, ?FL_M) ->
            Str1 = Sign ++ Str0,
            adjust_str(Str1, Fl, F);
       ?FLAG_SET(Fl, ?FL_Z) andalso F =/= none ->
            Str1 = adjust_str(Str0, ?FL_Z, F-length(Sign)),
            Sign ++ Str1;
       true ->
            Str1 = Sign ++ Str0,
            adjust_str(Str1, Fl, F)
    end.

%% format_e_float(Flags, Field, Precision, Number) -> String.
%% format_f_float(Flags, Field, Precision, Number) -> String.
%% format_g_float(Flags, Field, Precision, Number) -> String.
%%  Print float Number in e/f/g format.

format_e_float(Fl, F, P, N) ->
    format_float(Fl, F, e_float_precision(P), "~.*e", float(N)).

format_f_float(Fl, F, P, N) ->
    format_float(Fl, F, f_float_precision(P), "~.*f", float(N)).

format_g_float(Fl, F, P, N) ->
    format_float(Fl, F, g_float_precision(P), "~.*g", float(N)).

format_float(Fl, F, P, Format, N) ->
    Str0 = lists:flatten(io_lib:format(Format, [P,abs(N)])),
    Sign = sign(Fl, N),
    if ?FLAG_SET(Fl, ?FL_M) ->
            Str1 = Sign ++ Str0,
            adjust_str(Str1, Fl, F);
       ?FLAG_SET(Fl, ?FL_Z) andalso (F =/= none) ->
            Str1 = adjust_str(Str0, ?FL_Z, F-length(Sign)),
            Sign ++ Str1;
       true ->
            Str1 = Sign ++ Str0,
            adjust_str(Str1, Fl, F)
    end.

e_float_precision(none) -> 7;
e_float_precision(P) -> P+1.

f_float_precision(none) -> 6;
f_float_precision(P) -> P.

g_float_precision(none) -> 6;
g_float_precision(P) -> P.

%% sign(Flags, Number) -> SignString.

sign(_, N) when N < 0 -> "-";                   %Always sign when N<0
sign(Fl, _) ->
    if ?FLAG_SET(Fl, ?FL_P) -> "+";             %+ flag has priority
       ?FLAG_SET(Fl, ?FL_S) -> " ";
       true -> ""
    end.

trim_bin(Bin, Prec) when is_integer(Prec), byte_size(Bin) > Prec ->
    binary:part(Bin, 0, Prec);
trim_bin(Bin, _) -> Bin.

%% adjust_bin(Binary, Flags, Field) -> iolist().
%% adjust_str(String, Flags, Field) -> iolist().

adjust_bin(Bin, ?FL_NONE, none) -> Bin;
adjust_bin(Bin, Fl, F) when is_integer(F), byte_size(Bin) < F ->
    Size = byte_size(Bin),
    Padding = lists:duplicate(F-Size, pad_char(Fl, F)),
    if ?FLAG_SET(Fl, ?FL_M) -> [Bin,Padding];
       true -> [Padding,Bin]
    end;
adjust_bin(Bin, _, _) -> Bin.

adjust_str(Str, ?FL_NONE, none) -> Str;
adjust_str(Str, Fl, F) when is_integer(F), length(Str) < F ->
    Size = length(Str),
    Padding = lists:duplicate(F-Size, pad_char(Fl, F)),
    if ?FLAG_SET(Fl, ?FL_M) -> [Str,Padding];
       true -> [Padding,Str]
    end;
adjust_str(Str, _, _) -> Str.

%% pad_char(Flags, Field) -> Char.

pad_char(Fl, F) ->
    if ?FLAG_SET(Fl, ?FL_M) -> $\s;             %'-' forces padding to " "
       ?FLAG_SET(Fl, ?FL_Z), F =/= none -> $0;
       true -> $\s
    end.

%% build_q(Arg, State) -> String.
%%  Build the quote for the right argument types.

build_q(S0, _St) when is_binary(S0) ->
    S1 = build_q_string(S0),
    %% io:format("bq ~w\n   ~w\n", [S0,S1]),
    [$",S1,$"];
build_q(I, _St) when is_integer(I) ->
    integer_to_binary(I);
build_q(I, _St) when is_float(I) ->
    float_to_binary(I);
build_q(nil, _St) -> <<"nil">>;
build_q(true, _St) -> <<"true">>;
build_q(false, _St) -> <<"false">>;
build_q(Arg, St) ->
    badarg_error(format, ['q',Arg], St).

%% build_q_string(String) -> String.
%%  Build the quoted string.

build_q_string(<<$\\,Q/binary>>) -> [$\\,$\\|build_q_string(Q)];
build_q_string(<<$\",Q/binary>>) -> [$\\,$\"|build_q_string(Q)];
build_q_string(<<$\n,Q/binary>>) -> [$\\,$\n|build_q_string(Q)];
%% Control characters.
build_q_string(<<C1,Q/binary>>) when C1 >= 0, C1 =< 31 ->
    build_q_dec(C1, Q);
build_q_string(<<C1,Q/binary>>) when C1 >= 127, C1 =< 159 ->
    build_q_dec(C1, Q);
build_q_string(<<173,Q/binary>>) ->
    %% Don't ask me why we do this for 173.
    [io_lib:format("\\173",[])|build_q_string(Q)];
%% And the rest.
build_q_string(<<C,Q/binary>>) -> [C|build_q_string(Q)];
build_q_string(<<>>) -> [].

build_q_dec(C1, <<>> = Q) ->
    [io_lib:format("\\~w", [C1])|build_q_string(Q)];
build_q_dec(C1, <<C2,_/binary>> = Q) when not ?DIGIT(C2) ->
    [io_lib:format("\\~w", [C1])|build_q_string(Q)];
build_q_dec(C1, Q) ->
    [io_lib:format("\\~.3.0w",[C1])|build_q_string(Q)].
