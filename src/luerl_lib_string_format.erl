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
    {[iolist_to_binary(Str)],St1}.

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
    %% io:format("~w\n", [Fmt0]),
    {Fl,Fmt1} = flags(Fmt0),                    %The flags characters
    {Fw,Fmt2} = field_width(Fmt1),              %The field width
    {P,Fmt3} = precision(Fmt2),                 %The precision
    {C,Fmt4} = collect_cc(Fmt3),                %The control character
    %% io:format("col C=~c Fl=~.2b Fw=~w P=~w\n", [C,Fl,Fw,P]),
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
build({Conv,Fl,Fw,P}, [A|As], St0) when Conv =:= $d ; Conv =:= $i ->
    {S,St1} = format_decimal(Fl, Fw, P, A, St0),
    {S,As,St1};
build({$u,Fl,Fw,P}, [A|As], St0) ->
    {S,St1} = format_unsigned(Fl, Fw, P, A, St0),
    {S,As,St1};
build({$o,Fl,Fw,P}, [A|As], St0) ->
    {S,St1} = format_octal(Fl, Fw, P, A, St0),
    {S,As,St1};
build({$x,Fl,Fw,P}, [A|As], St0) ->
    {S,St1} = format_hex(Fl, Fw, P, A, St0),
    {S,As,St1};
build({$X,Fl,Fw,P}, [A|As], St0) ->
    {S,St1} = format_HEX(Fl, Fw, P, A, St0),
    {S,As,St1};
%% Float formats.
build({$e,Fl,Fw,P}, [A|As], St) ->
    {format_e_float(Fl, Fw, P, A),As,St};
build({$E,Fl,Fw,P}, [A|As], St) ->
    {format_e_float(Fl, Fw, P, A),As,St};
build({$f,Fl,Fw,P}, [A|As], St) ->
    {format_f_float(Fl, Fw, P, A),As,St};
build({$F,Fl,Fw,P}, [A|As], St) ->
    {format_f_float(Fl, Fw, P, A),As,St};
build({$g,Fl,Fw,P}, [A|As], St) ->
    {format_g_float(Fl, Fw, P, A),As,St};
build({$G,Fl,Fw,P}, [A|As], St) ->
    {format_g_float(Fl, Fw, P, A),As,St};
%% %p
build({$q,Fl,Fw,P}, [A|As], St0) ->
    {S,St1} = format_q(Fl, Fw, P, A, St0),
    {S,As,St1};
build({$s,Fl,Fw,P}, [A|As], St0) ->
    {S,St1} = format_s(Fl, Fw, P, A, St0),
    {S,As,St1};
% Literal % format.
build({$%,?FL_NONE,none,none}, As, St) ->       %No flags, field or precision!
    {"%",As,St}.

%% format_decimal(Flags, Field, Precision, Argument, State) -> {String,State}.
%% format_octal(Flags, Field, Precision, Argument, State) -> {String,State}.
%% format_hex(Flags, Field, Precision, Argument, State) -> {String,State}.
%% format_HEX(Flags, Field, Precision, Argument, State) -> {String,State}.
%% format_unsigned(Flags, Field, Precision, Argument, State) -> {String,State}.
%%  Print integer Number with base Base. This is a bit messy as we are
%%  following string.format handling. Could actually generate any
%%  error here as the error will be caught in the string.format
%%  call. But to be safe.

format_decimal(Fl, _F, _P, A, St) when ?FLAG_SET(Fl, ?FL_H) ->
    badarg_error(format, ['d',A], St);          %Could just generate any error
format_decimal(Fl, F, P, A, St) ->
    I = luerl_lib:arg_to_integer(A),
    Str = integer_to_list(abs(I), 10),
    {format_integer(Fl, F, P, I, "", Str),St}.

format_octal(Fl, _F, _P, A, St) when ?FLAG_SET(Fl, ?FL_S) ->
    badarg_error(format, ['o',A], St);          %Could just generate any error
format_octal(Fl, F, P, A, St) ->
    I = luerl_lib:arg_to_integer(A),
    Str = integer_to_list(abs(I), 8),
    {format_integer(Fl, F, P, I, "0", Str),St}.

format_hex(Fl, F, P, A, St) ->
    I = luerl_lib:arg_to_integer(A),
    Str = lists:flatten(io_lib:fwrite("~.16b", [abs(I)])),
    {format_integer(Fl, F, P, I, "0x", Str),St}.

format_HEX(Fl, F, P, A, St) ->
    I = luerl_lib:arg_to_integer(A),
    Str = lists:flatten(io_lib:fwrite("~.16B", [abs(I)])),
    {format_integer(Fl, F, P, I, "0X", Str),St}.

format_unsigned(Fl, _F, _P, A, St) when
      ?FLAG_SET(Fl, ?FL_H bor ?FL_S bor ?FL_P) ->
    badarg_error(format, ['u',A], St);
format_unsigned(Fl, F, P, A, St) ->
    %% We have to make the number always positive so we are assuming
    %% 124 bit integers which matches Lua 5.3.
    N = luerl_lib:arg_to_integer(A),
    I = if N >= 0 -> N;
           true -> N band 16#FFFFFFFFFFFFFFFF
        end,
    Str = integer_to_list(I),
    {format_integer(Fl, F, P, I, Str),St}.

%% format_integer(Flag, Field, Precision, Number, String) -> String.
%% format_integer(Flag, Field, Precision, Number, Prefix, String) -> String.
%%  Do the actual formatting of integers after the checking.

format_integer(Fl, F, P, N, Str) ->
    format_integer(Fl, F, P, N, "", Str).

format_integer(Fl, F, P, N, Prefix, Str0) ->
    %% The # says add the prefix.
    Str1 = if ?FLAG_SET(Fl, ?FL_H) -> Prefix ++ Str0;
              true -> Str0
           end,
    Sign = sign(Fl, N),
    if P =/= none ->
            Str2 = Sign ++ lists:flatten(adjust_str(Str1, ?FL_Z, P)),
            adjust_str(Str2, (Fl band ?FL_M), F);
       ?FLAG_SET(Fl, ?FL_M) ->
            Str2 = Sign ++ Str1,
            adjust_str(Str2, Fl, F);
       ?FLAG_SET(Fl, ?FL_Z) andalso F =/= none ->
            Str2 = adjust_str(Str1, ?FL_Z, F-length(Sign)),
            Sign ++ Str2;
       true ->
            Str2 = Sign ++ Str1,
            adjust_str(Str2, Fl, F)
    end.

%% format_e_float(Flags, Field, Precision, Argument) -> String.
%% format_f_float(Flags, Field, Precision, Argument) -> String.
%% format_g_float(Flags, Field, Precision, Argument) -> String.
%%  Print float Argument in e/f/g format.

%% Erlang's own io_lib "~.*f"/"~.*e"/"~.*g" cannot represent every
%% precision C's printf (and Lua's %f/%e/%g, defined the same way)
%% allows: io_lib:format("~.*f",[0,_]) and io_lib:format("~.*g",[0,_])
%% both raise badarg (their floor is precision 1), and
%% io_lib:format("~.*e",[P,_]) raises badarg below P=2 (its "precision"
%% counts the leading digit too, unlike f/g, which is why
%% e_float_precision/1 already adds 1 — Lua %.Ne means N digits *after*
%% the point, e_float_precision converts that to Erlang's "N+1 total
%% significant digits" convention; only Lua's own precision 0 still
%% lands below Erlang's floor of 2 after that conversion). Below, only
%% Lua's %.0f/%.0e/%.0g actually hit one of these floors — every other
%% precision was already fine before this patch, and stays on the
%% original io_lib path unchanged.

format_e_float(Fl, F, P, A) ->
    EP = e_float_precision(P),
    if EP < 2 ->
            N = luerl_lib:arg_to_float(A),
            Str0 = format_e_zero_precision(abs(N)),
            format_float_str(Fl, F, N, Str0);
       true ->
            format_float(Fl, F, EP, "~.*e", A)
    end.

format_f_float(Fl, F, P, A) ->
    FP = f_float_precision(P),
    if FP < 1 ->
            N = luerl_lib:arg_to_float(A),
            %% "%.0f": round to the nearest integer and print with no
            %% decimal point at all — exactly what C's own %.0f does.
            Str0 = integer_to_list(round(abs(N))),
            format_float_str(Fl, F, N, Str0);
       true ->
            format_float(Fl, F, FP, "~.*f", A)
    end.

format_g_float(Fl, F, P, A) ->
    GP = g_float_precision(P),
    %% C's own %g: "if the precision is zero, it is taken as 1" — this
    %% is both the correct semantics (not a workaround) and happens to
    %% sidestep Erlang's identical precision-0 floor for ~g.
    format_float(Fl, F, max(GP, 1), "~.*g", A).

%% format_float(Flag, Field, Precision, Format, Argument) -> String

format_float(Fl, F, P, Format, A) ->
    N = luerl_lib:arg_to_float(A),
    Str0 = lists:flatten(io_lib:format(Format, [P,abs(N)])),
    format_float_str(Fl, F, N, Str0).

%% format_float_str(Flags, Field, Number, UnsignedNumeralString) -> String
%%  The shared sign/field/zero-pad handling every format_*_float/4
%%  clause needs, independent of how the unsigned numeral string
%%  itself was produced — factored out so the precision-0 special
%%  cases above can reuse it instead of duplicating this logic.

format_float_str(Fl, F, N, Str0) ->
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

%% format_e_zero_precision(AbsN) -> String
%%  "%.0e": a single mantissa digit, no decimal point at all (matching
%%  C's own %.0e). Erlang's ~e cannot go straight to 1 total
%%  significant digit, so this formats at 2 (~e's own floor — a single
%%  fractional digit, e.g. "3.7e+0"; already correctly rounded and
%%  renormalised by Erlang itself, e.g. 9.96 comes back as "1.0e+1",
%%  not "10.0e+0") and then rounds that one remaining fractional digit
%%  away by hand, including the one edge case *that* step can still
%%  trigger: a mantissa of 9.5..9.9 rounds up to "10", which must
%%  renormalise again to "1" with the exponent bumped once more
%%  (matching what C's own %.0e does for the same input).
format_e_zero_precision(AbsN) ->
    Str = lists:flatten(io_lib:format("~.*e", [2, AbsN])),
    [D1C, $., D2C, $e | ExpStr] = Str,
    D1 = D1C - $0,
    D2 = D2C - $0,
    Exp = list_to_integer(ExpStr),
    {Mantissa, Exp1} = if D2 >= 5, D1 =:= 9 -> {1, Exp+1};
                          D2 >= 5 -> {D1+1, Exp};
                          true -> {D1, Exp}
                       end,
    ExpSign = if Exp1 < 0 -> "-"; true -> "+" end,
    lists:flatten(io_lib:format("~we~s~w", [Mantissa, ExpSign, abs(Exp1)])).

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

%% format_s(Flags, Field, Precision, Argument, State) -> {String,State}.

format_s(Fl, Fw, P, A, St0) ->
    {S0,St1} = luerl_lib:tostring(A, St0),
    %% If any field and there is a 0 in the string then we have an error.
    %% Lua wants this.
    Fargs = (Fl =/= ?FL_NONE) orelse (Fw =/= none) orelse (P =/= none),
    case Fargs andalso binary:match(S0, <<0>>) =/= nomatch of
        true -> badarg_error(format, ['s',A], St1);
        false -> false
    end,
    S1 = trim_bin(S0, P),
    S2 = adjust_bin(S1, Fl, Fw),
    {S2,St1}.

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

%% format_q(Fl, Fw, P, Arg, State) -> {Striing,State}.
%%  Could actually generate any error here as the error will be caught
%%  in the string.format call. But to be safe.

format_q(?FL_NONE, none, none, Arg, St) ->
    S = format_q(Arg, St),
    {S,St};
format_q(_Fl, _Fw, _P, Arg, St) ->
    badarg_error(format, ['q',Arg], St).        %Could just generate any error

%% format_q(Arg, State) -> String.
%%  Build the quote for the right argument types.

format_q(S0, _St) when is_binary(S0) ->
    S1 = format_q_string(S0),
    [$",S1,$"];
format_q(I, _St) when is_integer(I) ->
    integer_to_binary(I);
format_q(I, _St) when is_float(I) ->
    float_to_binary(I);
format_q(nil, _St) -> <<"nil">>;
format_q(true, _St) -> <<"true">>;
format_q(false, _St) -> <<"false">>;
format_q(Arg, St) ->
    badarg_error(format, ['q',Arg], St).

%% format_q_string(String) -> String.
%%  Build the quoted string.

format_q_string(<<$\\,Q/binary>>) -> [$\\,$\\|format_q_string(Q)];
format_q_string(<<$\",Q/binary>>) -> [$\\,$\"|format_q_string(Q)];
format_q_string(<<$\n,Q/binary>>) -> [$\\,$\n|format_q_string(Q)];
%% Control characters.
format_q_string(<<C1,Q/binary>>) when C1 >= 0, C1 =< 31 ->
    format_q_dec(C1, Q);
format_q_string(<<C1,Q/binary>>) when C1 >= 127, C1 =< 159 ->
    format_q_dec(C1, Q);
format_q_string(<<173,Q/binary>>) ->
    %% Don't ask me why we do this for 173.
    [io_lib:format("\\173",[])|format_q_string(Q)];
%% And the rest.
format_q_string(<<C,Q/binary>>) -> [C|format_q_string(Q)];
format_q_string(<<>>) -> [].

format_q_dec(C1, <<>> = Q) ->
    [io_lib:format("\\~w", [C1])|format_q_string(Q)];
format_q_dec(C1, <<C2,_/binary>> = Q) when not ?DIGIT(C2) ->
    [io_lib:format("\\~w", [C1])|format_q_string(Q)];
format_q_dec(C1, Q) ->
    [io_lib:format("\\~.3.0w",[C1])|format_q_string(Q)].
