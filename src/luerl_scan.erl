%% Copyright (c) 2025 Robert Virding
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

%% File    : luerl_scan.erl
%% Author  : Robert Virding
%% Purpose : Token scanner for the LUA language.

%%% Much of the basic structure has been taken from erl_scan. A core
%%% difference is that we need to handle both the cases of only one
%%% token and many tokens. For this reason we have added the 'none'
%%% return.
%%%
%%% We chain a state, #luerl_scan{}, through the scanner but we never
%%% use it for anything. It was inherited from erl_scan and we left it
%%% just in case we might need it.

-module(luerl_scan).

-export([string/1,string/2,string/3,token/2,token/3,tokens/2,tokens/3,
         format_error/1]).

-export([start_name_char/1,name_char/1,is_keyword/1]).

-export([token_test/1,token_test/2,tokens_test/1,tokens_test/2]).

format_error({illegal_token,S}) ->
    io_lib:format(<<"illegal token '~s'">>, [S]);
format_error({illegal_chars,S}) ->
    io_lib:format(<<"illegal characters in '~s'">>, [S]);
format_error({illegal_chars,Type,S}) ->
    io_lib:format(<<"illegal characters in ~w '~s'">>, [Type,S]);
format_error({bad_format,Type,S}) ->
    io_lib:format(<<"bad format of ~w '~s'">>, [Type,S]);
format_error({user,S}) -> S;
format_error(Other) ->
    lists:flatten(io_lib:write(Other)).

%% Nothing in here yet, but who knows.
-record(luerl_scan, {}).

%% LFE definitions of these types.
-define(WHITE_SPACE(C), (is_integer(C) andalso C >= $\000 andalso C =< $\s)).
-define(UPPER(C), (C >= $A andalso C =< $Z)).
-define(LOWER(C), (C >= $a andalso C =< $z)).
-define(ASCII(C), (C >= 0 andalso C =< 127)).
-define(DIGIT(C), (C >= $0 andalso C =< $9)).
-define(HEX(C), (C >= $A andalso C =< $F orelse
                 C >= $a andalso C =< $f orelse
                 ?DIGIT(C))).
-define(CHAR(C), (C >= O andalso C < 16#110000)).
-define(UNICODE(C),
        (is_integer(C) andalso
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF))).

-define(UNI255(C), (is_integer(C) andalso 0 =< C andalso C =< 16#ff)).

%% string(String) ->
%% string(String, StartLine) ->
%% string(String, StartLine, Options) ->
%%     {ok,Tokens,LastLine}  | {error,Error,LastLine}.
%%  Scan a string for the tokens in it.

string(String) ->
    string(String, 1, []).

string(String, StartLocation) ->
    string(String, StartLocation, []).

string(String, StartLocation, _Options) ->
    string1(String, StartLocation, 1, [], #luerl_scan{}).

%% string1(Chars, Line, Column, Tokens, State) ->
%%     {ok,Tokens,LastLine} | {error,Error,LastLine}.

string1(Cs, Line, _Col, Toks, _St) when Cs =:= [] orelse Cs =:= eof ->
    %% No more chars left in the string.
    {ok,lists:reverse(Toks),anno(Line)};
string1(Cs0, Line0, Col0, Toks0, St0) ->
    %% Keep going!
    case scan1(Cs0, Line0, Col0, St0) of
        {more,{Cs1,Line1,Col1,St1,Extra,Fun}} ->
            %% Needs more, try again by appending eof.
            case Fun(Cs1 ++ eof, Line1, Col1, St1, Extra) of
                %% What to do when we get more here.
                {more,{Cs2,Line2,Col2,St2,Extra2,Fun2}} ->
                    Fun2(Cs2, Line2, Col2, St2, Extra2);
                {none,Rest,Line2,Col2,St2} ->
                    string1(Rest, Line2, Col2, Toks0, St2);
                {ok,Tok,_Rest,Line2,_Col2, _St2} ->
                    %% It worked and all is done.
                    {ok,lists:reverse([Tok|Toks0]),anno(Line2)};
                {{error,_,_}=Error,_Rest} ->
                    %% Really bad!
                    Error
            end;
        {none,Rest,Line1,Col1,St1} ->
            %% Nothing from this call so try again.
            string1(Rest, Line1, Col1, Toks0, St1);
        {ok,Tok,Rest,Line1,Col1,St1} ->
            %% It worked but still has chars left, try agiain.
            string1(Rest, Line1, Col1, [Tok|Toks0], St1);
        {{error,_,_}=Error,_Rest} ->
            %% Bad!
            Error
    end.

%% token(Continuation, String) ->
%% token(Continuation, String, StartLine) ->
%%     {more,Continuation} | {done,ReturnVal,RestChars}.
%%  Scan all tokens in the repeated calls threading the continuation
%%  through the calls.

token(Cont, Chars) ->
    token(Cont, Chars, 1, []).

token(Cont, Chars, StartLine) ->
    token(Cont, Chars, StartLine, []).

token([], Chars, Line, _Options) ->
    %% io:format("t4 ~p\n", [{[],Chars,Line,_Options}]),
    token1(Chars, Line, 1, #luerl_scan{}, [], fun scan/5); 
token({luerl_scan_token,Cs,Line,Col,St,Extra,Fun},
       Chars, _Line, _Options) ->
    %% io:format("t4 ~p\n", [{luerl_scan_token,Chars,_Line,_Options}]),
    token1(Cs ++ Chars, Line, Col, St, Extra, Fun).

%% token1(Chars, Line, Column, State, Extra, Fun) ->
%%     {done,{ok,Token,Line},Cs} | {done,Error,Cs} | {more,Continuation}.

token1(eof, Line, Col, St, Extra, Fun) ->
    Fun(eof, Line, Col, St, Extra);
token1([], Line, Col, St, Extra, Fun) ->
    {more,{luerl_scan_token,[],Line,Col,St,Extra,Fun}};
token1(Cs0, Line0, Col0, St0, Extra0, Fun0) ->
    %% io:format("t11 ~p\n", [{Cs0,Fun0,fun scan/5]),
    case Fun0(Cs0, Line0, Col0, St0, Extra0) of
        {more,{Cs1,Line1,Col1,St1,Extra1,Fun1}} ->
            Cont = {luerl_scan_token,Cs1,Line1,Col1,St1,Extra1,Fun1},
            %% io:format("tf1 ~p\n", [{Cs1,Line1,Col1,Fun1,fun scan/5}]),
            {more,Cont};
        {none,Rest,Line1,Col1,St1} ->
            %% Nothing from this call so try again.
            token1(Rest, Line1, Col1, St1, Extra0, fun scan/5);
        {ok,Token,Rest,Line1,_Col1,_St1} ->
            {done,{ok,Token,Line1},Rest};
        {{error,_,_}=Error,Rest} ->
            {done,Error,Rest}
    end.

%% tokens(Continuation, String) ->
%% tokens(Continuation, String, StartLine) ->
%% tokens(Continuation, String, StartLine, Options) ->
%%     {more,Continuation} | {done,ReturnVal,RestChars}.
%%  Scan all tokens in the repeated calls threading the continuation
%%  through the calls.

tokens(Cont, Chars) ->
    %% io:format("ts2 ~p\n", [{Cont,Chars}]),
    tokens(Cont, Chars, 1, []).

tokens(Cont, Chars, StartLine) ->
    %% io:format("ts3 ~p\n", [{Cont,Chars,StartLine}]),
    tokens(Cont, Chars, StartLine, []).

tokens([], Chars, Line, _Options) ->
    %% io:format("ts4 ~p\n", [{[],Chars}]),
    tokens1(Chars, Line, 1, [], #luerl_scan{}, [], fun scan/5); 
tokens({luerl_scan_tokens,Cs,Line,Col,Toks,St,Extra,Fun},
       Chars, _Line, _Options) ->
    %% io:format("ts4 ~p\n", [{Cont,Chars}]),
    tokens1(Cs ++ Chars, Line, Col, Toks, St, Extra, Fun).

%% tokens1(Chars, Line, Column, State, Extra, Fun) ->
%%     {done,{ok,Token,Line},Cs} | {done,Error,Cs} | {more,Continuation}.
%%  We loop inside this function for as long as we can until we need
%%  more characters or there is an error.

tokens1(Cs, Line0, Col0, Toks, St0, Extra0, Fun0) ->
    case Fun0(Cs, Line0, Col0, St0, Extra0) of
        {more,{Cs1,Line1,Col1,St1,Extra1,Fun1}} ->
            Cont = {luerl_scan_tokens,Cs1,Line1,Col1,Toks,St1,Extra1,Fun1},
            {more,Cont};
        {none,Rest,Line1,Col1,St1} ->
            if Rest =:= eof ->
                    {done,{ok,lists:reverse(Toks),anno(Line1)},eof};
               true ->
                    tokens1(Rest, Line1, Col1, Toks, St1, Extra0, fun scan/5)
            end;
        {ok,Tok,Rest,Line1,Col1,St1} ->
            if Rest =:= eof ->
                    {done,{ok,lists:reverse([Tok|Toks]),anno(Line1)},eof};
               true ->
                    tokens1(Rest, Line1, Col1, [Tok|Toks], St1, [], fun scan/5)
            end;
        {{error,_,_}=Error,Rest} ->
            {done,Error,Rest}
    end.

%%
%% Now to the actual scanning and collecting tokens.
%%

%% scan(Chars, Line, Column, State, Extra) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan one token.

scan(Chars, Line, Col, St, _) ->
    scan1(Chars, Line, Col, St).

%% scan1(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | None|  ScanError.

scan1_fun(Cs, Line, Col, St, _) ->
    scan1(Cs, Line, Col, St).

%% Newlines, space and tabs.
scan1([$\n|Cs], Line, _Col, St) ->
    skip_white_space(Cs, Line+1, 1, St);
scan1([$\s|Cs], Line, Col, St) ->
    skip_white_space(Cs, Line, Col+1, St);
scan1([$\t|Cs], Line, Col, St) ->
    skip_white_space(Cs, Line, Col+1, St);
%% Common punctuation marks.
scan1([$+|Cs], Line, Col, St) ->
    {ok,{'+',Line},Cs,Line,Col+1,St};
scan1([$*|Cs], Line, Col, St) ->
    {ok,{'*',Line},Cs,Line,Col+1,St};
scan1([$^|Cs], Line, Col, St) ->
    {ok,{'^',Line},Cs,Line,Col+1,St};
scan1([$#|Cs], Line, Col, St) ->
    {ok,{'#',Line},Cs,Line,Col+1,St};
scan1([$&|Cs], Line, Col, St) ->
    {ok,{'&',Line},Cs,Line,Col+1,St};
scan1([$(|Cs], Line, Col, St) ->
    {ok,{'(',Line},Cs,Line,Col+1,St};
scan1([$)|Cs], Line, Col, St) ->
    {ok,{')',Line},Cs,Line,Col+1,St};
scan1([${|Cs], Line, Col, St) ->
    {ok,{'}',Line},Cs,Line,Col+1,St};
scan1([$[|Cs], Line, Col, St) ->
    scan_square_bracket([$[|Cs], Line, Col, St);
scan1([$]|Cs], Line, Col, St) ->
    {ok,{']',Line},Cs,Line,Col+1,St};
scan1([$;|Cs], Line, Col, St) ->
    {ok,{';',Line},Cs,Line,Col+1,St};
scan1([$,|Cs], Line, Col, St) ->
    {ok,{',',Line},Cs,Line,Col+1,St};
scan1([C|Cs], Line, Col, St) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, Line, Col+1, St);
%% Punctuation characters and operators, first recognise multiples.
%% --
scan1("--"++Cs, Line, Col, St) ->
    scan_comment(Cs, Line, Col, St);
scan1("-"=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% //
scan1("//"++Cs, Line, Col, St) ->
    {ok,{'//',Line},Cs,Line,Col+2,St};
scan1("/"=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% >> >=
scan1(">>"++Cs, Line, Col, St) ->
    {ok,{'>>',Line},Cs,Line,Col+2,St};
scan1(">="++Cs, Line, Col, St) ->
    {ok,{'>=',Line},Cs,Line,Col+2,St};
scan1(">"=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% << <=
scan1("<<"++Cs, Line, Col, St) ->
    {ok,{'<<',Line},Cs,Line,Col+2,St};
scan1("<="++Cs, Line, Col, St) ->
    {ok,{'<=',Line},Cs,Line,Col+2,St};
scan1("<"=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% ==
scan1("=="++Cs, Line, Col, St) ->
    {ok,{'==',Line},Cs,Line,Col+2,St};
scan1("="=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% ::
scan1("::"++Cs, Line, Col, St) ->
    {ok,{'==',Line},Cs,Line,Col+2,St};
scan1(":"=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% ~=
scan1("~="++Cs, Line, Col, St) ->
    {ok,{'~=',Line},Cs,Line,Col+2,St};
scan1("~"=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% ... ..
scan1("..."++Cs, Line, Col, St) ->
    {ok,{'...',Line},Cs,Line,Col+3,St};
scan1(".."=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
scan1(".."++Cs, Line, Col, St) ->
    {ok,{'..',Line},Cs,Line,Col+2,St};
scan1("."=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun scan1_fun/5}};
%% Single character punctuation and operators.
scan1([$-|Cs], Line, Col, St) ->
    {ok,{'-',Line},Cs,Line,Col+1,St};
scan1([$/|Cs], Line, Col, St) ->
    {ok,{'/',Line},Cs,Line,Col+1,St};
scan1([$>|Cs], Line, Col, St) ->
    {ok,{'>',Line},Cs,Line,Col+1,St};
scan1([$<|Cs], Line, Col, St) ->
    {ok,{'<',Line},Cs,Line,Col+1,St};
scan1([$=|Cs], Line, Col, St) ->
    {ok,{'=',Line},Cs,Line,Col+1,St};
scan1([$:|Cs], Line, Col, St) ->
    {ok,{':',Line},Cs,Line,Col+1,St};
scan1([$~|Cs], Line, Col, St) ->
    {ok,{'~',Line},Cs,Line,Col+1,St};
scan1([$.|Cs], Line, Col, St) ->
    scan_dot(Cs, Line, Col, St);
%% Strings
scan1([$"|Cs], Line, Col, St) ->
    scan_string(Cs, Line, Col, $", St);
scan1([$'|Cs], Line, Col, St) ->
    scan_string(Cs, Line, Col, $', St);
%% Numbers.
scan1([$0|Cs], Line, Col, St) ->
    scan_zero_number(Cs, Line, Col+1, St);
scan1([C|Cs], Line, Col, St) when ?DIGIT(C) ->
    scan_dec_number([C|Cs], Line, Col, St);
%% Names.
scan1([C|Cs], Line, Col, St) ->
    case start_name_char(C) of
        true ->
            scan_name([C|Cs], Line, Col, St);
        false ->
            %% Everything else is unrecognised.
            scan_error({illegal_token,[C]}, Line, Col, Line, Col+1, Cs)
    end;
scan1([], Line, Col, St) ->
    %% Need more here.
    {more,{[],Line,Col,St,[],fun scan1_fun/5}};
scan1(eof, Line, Col, St) ->
    %% We didn't get anything and nothing left.
    {none,eof,Line,Col,St}.

%% skip_white_space(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.

skip_white_space_fun(Cs, Line, Col, St, _) ->
    skip_white_space(Cs, Line, Col, St).

skip_white_space([$\n|Cs], Line, _Col, St) ->
    skip_white_space(Cs, Line+1, 1, St);
skip_white_space([C|Cs], Line, Col, St) when ?WHITE_SPACE(C) ->
    skip_white_space(Cs, Line, Col+1, St);
skip_white_space([]=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,[],fun skip_white_space_fun/5}};
skip_white_space(Cs, Line, Col, St) ->
    scan1(Cs, Line, Col, St).

%% scan_dot(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan '.' to determine if it is a '.' or a number.

scan_dot_fun(Cs, Line, Col, St, _) ->
    scan_dot(Cs, Line, Col, St).

scan_dot([C|Cs], Line, Col, St) when ?DIGIT(C) ->
    scan_dec_number([$\.,C|Cs], Line, Col, St);
scan_dot([C|Cs], Line, Col, St) ->
    {ok,{'.',Line},[C|Cs],Line,Col+1,St};
scan_dot([]=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,none,fun scan_dot_fun/5}};
scan_dot(eof=Cs, Line, Col, St) ->
    {ok,{'.',Line},Cs,Line,Col,St}.

%% scan_comment(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column,State} | {none,Cs,Line,Col+1,St}.
%%  Check if the comment is a line comment or a long bracket
%%  comment. We do the same checking as in the "standard" Lua parser.

scan_comment(Cs, Line, Col, St) ->
    scan_comment(Cs, Line, Col, Line, Col+2, [], St).

scan_comment_fun(Cs, Line, Col, St, {Sline,Scol,Scs}) ->
    scan_comment(Cs, Line, Col, Sline, Scol, Scs, St).

scan_comment([$[|Cs], Sline, Scol, Line, Col, Scs, St) ->
    scan_comment1(Cs, Sline, Scol, Line, Col+1, Scs ++ [$[], St);
scan_comment([C|Cs], _Sline, _Scol, Line, Col, _Scs, St) ->
    scan_comment_body([C|Cs], Line, Col, St);
scan_comment([]=Cs, Sline, Scol, Line, Col, Scs, St) ->
    {more,{Cs,Line,Col,St,{Sline,Scol,Scs},fun scan_comment_fun/5}};
scan_comment(eof=Cs, _Sline, _Scol, Line, Col, _Scs, St) ->
    {none,Cs,Line,Col,St}.

scan_comment1_fun(Cs, Line, Col, St, {Sline,Scol,Scs}) ->
    scan_comment1(Cs, Line, Col, Sline, Scol, Scs, St).

scan_comment1([$[|_]=Cs, Sline, Scol, _Line, _Col, Scs, St) ->
    %% We have the long bracket start.
    scan_long_bracket_comment(Scs ++ Cs, Sline, Scol, St);
scan_comment1([$=|Cs], Sline, Scol, Line, Col, Scs, St) ->
    scan_comment1(Cs, Sline, Scol, Line, Col+1, Scs ++ [$=], St);
scan_comment1([_|_]=Cs, _Sline, _Scol, Line, Col, _Scs, St) ->
    scan_comment_body(Cs, Line, Col, St);
scan_comment1([]=Cs, Sline, Scol, Line, Col, Scs, St) ->
    {more,{Cs,Line,Col,St,{Sline,Scol,Scs},fun scan_comment1_fun/5}};
scan_comment1(eof=Cs, _Sline, _Scol, Line, Col, _Scs, St) ->
    {none,Cs,Line,Col,St}.

scan_comment_body_fun(Cs, Line, Col, St, _) ->
    scan_comment_body(Cs, Line, Col, St).

scan_comment_body([$\n|Cs], Line, _Col, St) ->
    {none,Cs,Line+1,0,St};
scan_comment_body([_|Cs], Line, Col, St) ->
    scan_comment_body(Cs, Line, Col+1, St);
scan_comment_body([]=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,none,fun scan_comment_body_fun/5}};
scan_comment_body(eof=Cs, Line, Col, St) ->
    {none,Cs,Line,Col,St}.

%% scan_square_bracket(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column,State} | {more,Continuation} | ScanError.
%%  Check whether the '[' is the start of a long bracket string or
%%  just a plain '[' token. We do the same checking as in the
%%  "standard" Lua parser.

scan_square_bracket_fun(Cs, Line, Col, St, _) ->
    scan_square_bracket(Cs, Line, Col, St).

scan_square_bracket([$[,C|Cs], Line, Col, St) when C =:= $[ ; C =:= $= ->
    %% Assume a long bracket here and leave the checking to the bracket.
    scan_long_bracket_string([$[,C|Cs], Line, Col, St);
scan_square_bracket([$[,C|Cs], Line, Col, St) ->
    {ok,{'[',Line},[C|Cs],Line,Col,St};
scan_square_bracket([$[|eof], Line, Col, St) ->
    {ok,{'[',Line},eof,Line,Col,St};
scan_square_bracket([_]=Cs, Line, Col, St) ->
    {more,{Cs,Line,Col,St,none,fun scan_square_bracket_fun/5}}.

scan_long_bracket_string(Cs, Line, Col, St) ->
    scan_long_bracket(Cs, Line, Col, string, St).

scan_long_bracket_comment(Cs, Line, Col, St) ->
    scan_long_bracket(Cs, Line, Col, comment, St).

%% scan_long_bracket(Chars, Line, Column, Type, State) ->
%%     {ok,Chars,Line,Column,State} | {more,...} | {eof,...}.
%%
%%  Scan a long bracket and return its characters. The scanning is
%%  split into 3 sections:
%%  the start section where we collect the '[' and depth
%%  the body section where we collect all the characters
%%  the end section where we check we have the right ending
%%
%%  We collect in the chars endings with the wrong depth.

-record(lb, {sline=0,                           %Start line
             scol=0,                            %Start column
             level=0,                           %Level
             type=unknown,                      %Long bracket type
             schars=[],                         %Start characters
             bchars=[],                         %Body characters
             echars=[]                          %End characters
            }).

scan_long_bracket([$[|Cs], Line, Col, Type, St) ->
    %% Initialse the long bracket state.
    Lb = #lb{sline=Line,scol=Col,level=0,type=Type,
             schars=[$[],bchars=[],echars=[]},
    scan_lb_start(Cs, Line, Col, Lb, St).

scan_lb_start(Cs, Line, Col, Lb, St) ->
    scan_lb_start(Cs, Line, Col, Lb#lb.schars, Lb, St).

scan_lb_start_fun(Cs, Line, Col, St, {Schars,Lb}) ->
    scan_lb_start(Cs, Line, Col, Schars, Lb, St).

scan_lb_start([$[|Cs], Line, Col, Schars, Lb0, St) ->
    %% Now we collect the characters.
    Lb1 = Lb0#lb{schars = Schars ++ [$[]},
    scan_lb_body(Cs, Line, Col+1, Lb1, St);
scan_lb_start([$=|Cs], Line, Col, Schars, Lb0, St) ->
    #lb{level = Level} = Lb0,
    Lb1 = Lb0#lb{level = Level+1},
    scan_lb_start(Cs, Line, Col+1, Schars ++ [$=], Lb1, St); 
scan_lb_start([_|_]=Cs, Line, Col, Schars, Lb0, St) ->
    %% Anything else is an error.
    Lb1 = Lb0#lb{schars = Schars},
    scan_lb_error(Cs, Line, Col, Lb1, St);
scan_lb_start([]=Cs, Line, Col, Schars, Lb, St) ->
    {more,{Cs,Line,Col,St,{Schars,Lb},fun scan_lb_start_fun/5}};
scan_lb_start(eof=Cs, Line, Col, Schars, Lb0, St) ->
    Lb1 = Lb0#lb{schars = Schars},
    scan_lb_error(Cs, Line, Col, Lb1, St).

scan_lb_error(Cs, Line, Col,
              #lb{sline = Sline,scol = Scol,type = Type,schars = Schars,bchars = Bchars}, _St) ->
    Chars = lists:sublist(Schars ++ Bchars, 10), %Only the first 10 characters
    scan_error({bad_format,Type,Chars},
               Sline, Scol, Line, Col, Cs).

scan_lb_body(Cs, Line, Col, Lb, St) ->
    scan_lb_body(Cs, Line, Col, Lb#lb.bchars, Lb, St).

scan_lb_body_fun(Cs, Line, Col, St, {Bchars,Lb}) ->
    scan_lb_body(Cs, Line, Col, Bchars, Lb, St).

scan_lb_body([$]|Cs], Line, Col, Bchars, Lb0, St) ->
    #lb{echars = Echars} = Lb0,
    Lb1 = Lb0#lb{bchars = Bchars,echars = Echars ++ [$]]},
    scan_lb_end(Cs, Line, Col+1, Lb1, St);
scan_lb_body([$\n|Cs], Line, _Col, Bchars, Lb, St) ->
    scan_lb_body(Cs, Line+1, 0, Bchars ++ [$\n], Lb, St);
scan_lb_body([C|Cs], Line, Col, Bchars, Lb, St) ->
    scan_lb_body(Cs, Line, Col+1, Bchars ++ [C], Lb, St);
scan_lb_body([]=Cs, Line, Col, Bchars, Lb, St) ->
    {more,{Cs,Line,Col,St,{Bchars,Lb},fun scan_lb_body_fun/5}};
scan_lb_body(eof=Cs, Line, Col, Bchars, Lb0, St) ->
    Lb1 = Lb0#lb{bchars = Bchars},
    scan_lb_error(Cs, Line, Col, Lb1, St).

scan_lb_end(Cs, Line, Col, Lb, St) ->
    scan_lb_end(Cs, Line, Col, Lb#lb.level, Lb#lb.echars, Lb, St).

scan_lb_end_fun(Cs, Line, Col, St, {Elevel,Echars,Lb}) ->
    scan_lb_end(Cs, Line, Col, Elevel, Echars, Lb, St).

scan_lb_end([$]|Cs], Line, Col, 0, _Echars, Lb, St) ->
    %% We have hit the right end.
    #lb{sline = Sline,type = Type,bchars = Bchars0} = Lb,
    case Type of
        string ->
            %% Drop first character if it is a newline. Export Bchars1.
            case Bchars0 of
                [$\n|Bchars1] -> Bchars1;
                Bchars1 -> Bchars1
            end,
            Binary = iolist_to_binary(Bchars1),
            Token = {'LITERALSTRING',Sline,Binary},
            {ok,Token,Cs,Line,Col+1,St};
        comment ->
            %% Just drop the comment.
            {none,Cs,Line,Col+1,St}
    end;
scan_lb_end([$]|Cs], Line, Col, Elevel, Echars, Lb0, St) when Elevel =/= 0 ->
    %% This was not the right end so we keep collecting characters.
    #lb{bchars = Bchars} = Lb0,
    Lb1 = Lb0#lb{bchars = Bchars ++ Echars ++ [$]], echars = []},
    %% io:format("sleb1 ~p\n'~c'   ~p\n", [Lb0,$],Lb1]),
    scan_lb_body(Cs, Line, Col+1, Lb1, St);
scan_lb_end([$=|Cs], Line, Col, Elevel, Echars, Lb, St) ->
    %% Keep track of the end level.
    %% io:format("slee ~p\n'~c'  ~p\n", [Lb,$=,Echars ++ [$=]]),
    scan_lb_end(Cs, Line, Col+1, Elevel-1, Echars ++ [$=], Lb, St);
scan_lb_end([C|Cs], Line, Col, _Elevel, Echars, Lb0, St) ->
    %% This was not a right end so we keep collecting characters.
    #lb{bchars = Bchars} = Lb0,
    Lb1 = Lb0#lb{bchars = Bchars ++ Echars ++ [C], echars = []},
    %% io:format("sleb2 ~p\n'~c'   ~p\n", [Lb0,C,Lb1]),
    scan_lb_body(Cs, Line, Col+1, Lb1, St);
scan_lb_end([]=Cs, Line, Col, Elevel, Echars, Lb, St) ->
    {more,{Cs,Line,Col,St,{Elevel,Echars,Lb},fun scan_lb_end_fun/5}};
scan_lb_end(eof=Cs, Line, Col, _Elevel, Echars, Lb0, St) ->
    Lb1 = Lb0#lb{echars = Echars},
    scan_lb_error(Cs, Line, Col, Lb1, St).

%% scan_zero_number(Chars, Line, Col, State) ->
%%     {ok,Token,Chars,Line,Column} | {more,Continuation} | ScanError.
%%  Scan a number starting with '0'.

scan_zero_number(Cs, Line, Col, St) ->
    scan_zero_number1(Cs, Line, Col, St).

scan_zero_number_fun(Cs, Line, Col, St, _) ->
    scan_zero_number1(Cs, Line, Col, St).

scan_zero_number1([C|Cs], Line, Col, St) when C =:= $x; C =:= $X ->
    %% Scan a base 16 number.
    scan_hex_number(Cs, Line, Col+1, St);
scan_zero_number1([], Line, Col, St) ->
    {more,{[],Line,Col,St,none,fun scan_zero_number_fun/5}};
scan_zero_number1(Cs, Line, Col, St) ->
    %% Pass the buck to the standard number scanner.
    scan_dec_number([$0|Cs], Line, Col, St).

%% scan_dec_number(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column,State} | {more,Continuation} | ScanError.
%%  Scan decimal numbers.

scan_dec_number(Cs, Line, Col, St) ->
    scan_dec_number(Cs, Line, Col, [], St).

scan_dec_number_fun(Cs, Line, Col, St, Digits) ->
    scan_dec_number(Cs, Line, Col, Digits, St).

scan_dec_number([C|Cs], Line, Col, Digits, St) when
      C =:= $.; C =:= $+; C =:= $- ->
    scan_dec_number(Cs, Line, Col+1, [C|Digits], St);
scan_dec_number([C|Cs], Line, Col, Digits, St) when
      C =:= $e; C =:= $E ->
    scan_dec_number(Cs, Line, Col+1, [C|Digits], St);
scan_dec_number([C|Cs], Line, Col, Digits, St) when ?DIGIT(C) ->
    scan_dec_number(Cs, Line, Col+1, [C|Digits], St);
scan_dec_number([C|Cs], Line, Col, Digits, St) ->
    %% Check if we have any "legal" not number characters and collect them.
    case name_char(C) of
        true ->
            scan_number_rest(Cs, Line, Col+1, [C|Digits], St);
        false ->
            scan_dec_number_check([C|Cs], Line, Col, Digits, St)
    end;
scan_dec_number([]=Cs, Line, Col, Digits, St) ->
    {more,{Cs,Line,Col,St,Digits,fun scan_dec_number_fun/5}};
scan_dec_number(eof=Cs, Line, Col, Digits, St) ->
    scan_dec_number_check(Cs, Line, Col, Digits, St).

scan_dec_number_check(Cs, Line, Col, Digits, St) ->
    CheckDigits = [$0|lists:reverse(Digits)],
    Result = case catch {ok,list_to_integer(CheckDigits)} of
                 {ok,I} -> {ok,I};
                 _ ->
                     case catch {ok,list_to_float(CheckDigits)} of
                         {ok,F} -> {ok,F};
                         _ -> error
                     end
             end,
    case Result of
        {ok,Number} ->
            Token = {'NUMERAL',Line,Number},
            {ok,Token,Cs,Line,Col,St};
        error ->
            scan_number_error(Cs, Line, Col, Digits, St)
    end.

%% scan_hex_number(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column,State} | {more,Continuation} | ScanError.
%%  Scan hexadecimal numbers.

scan_hex_number(Cs, Line, Col, St) ->
    scan_hex_number(Cs, Line, Col, [], St).

scan_hex_number_fun(Cs, Line, Col, St, Digits) ->
    scan_hex_number(Cs, Line, Col, Digits, St).

scan_hex_number([C|Cs], Line, Col, Digits, St) when
      C =:= $.; C =:= $+; C =:= $- ->
    scan_hex_number(Cs, Line, Col+1, [C|Digits], St);
scan_hex_number([C|Cs], Line, Col, Digits, St) when
      C =:= $p; C =:= $P ->
    scan_hex_number(Cs, Line, Col+1, [C|Digits], St);
scan_hex_number([C|Cs], Line, Col, Digits, St) when ?HEX(C) ->
    scan_hex_number(Cs, Line, Col+1, [C|Digits], St);
scan_hex_number([C|Cs], Line, Col, Digits, St) ->
    %% Check if we have any "legal" not number characters and collect them.
    case name_char(C) of
        true ->
            scan_number_rest(Cs, Line, Col+1, [C|Digits], St);
        false ->
            scan_hex_number_check([C|Cs], Line, Col, Digits, St)
    end;
scan_hex_number([]=Cs, Line, Col, Digits, St) ->
    {more,{Cs,Line,Col,St,Digits,fun scan_hex_number_fun/5}};
scan_hex_number(eof=Cs, Line, Col, Digits, St) ->
    scan_hex_number_check(Cs, Line, Col, Digits, St).

scan_hex_number_check(Cs, Line, Col, Digits, St) ->
    CheckDigits = [$0|lists:reverse(Digits)],
    Result = case hex_number_whole(CheckDigits, 0) of
                 {W,[]} -> {ok,W};
                 {W,[$.|Fcs]} ->
                     case hex_number_fraction(Fcs, 16.0, W) of
                         {HF,[]} -> {ok,HF};
                         {HF,[P|Ecs]} when P =:= $p; P =:= $P ->
                             case catch {ok,list_to_integer(Ecs)} of
                                 {ok,E} -> {ok,HF * math:pow(2, E)};
                                 _Other -> error
                             end;
                         _Other -> error
                     end;
                 {_,_Rest} ->
                     error
             end,
    case Result of
        {ok,Number} ->
            Token = {'NUMERAL',Line,Number},
            {ok,Token,Cs,Line,Col,St};
        error ->
            scan_number_error(Cs, Line, Col, Digits, St)
    end.

hex_number_whole([C|Cs], SoFar) when C >= $0, C =< $9 ->
    hex_number_whole(Cs, SoFar * 16 + (C - $0));
hex_number_whole([C|Cs], SoFar) when C >= $a, C =< $f ->
    hex_number_whole(Cs, SoFar * 16 + (C - $a + 10));
hex_number_whole([C|Cs], SoFar) when C >= $A, C =< $F ->
    hex_number_whole(Cs, SoFar * 16 + (C - $A + 10));
hex_number_whole(Cs, SoFar) ->
    {SoFar,Cs}.

hex_number_fraction([C|Cs], Pow, SoFar) when C >= $0, C =< $9 ->
    hex_number_fraction(Cs, Pow*16.0, SoFar + (C - $0)/Pow);
hex_number_fraction([C|Cs], Pow, SoFar) when C >= $a, C =< $f ->
    hex_number_fraction(Cs, Pow*16.0, SoFar + (C - $a + 10)/Pow);
hex_number_fraction([C|Cs], Pow, SoFar) when C >= $A, C =< $F ->
    hex_number_fraction(Cs, Pow*16.0, SoFar + (C - $A + 10)/Pow);
hex_number_fraction(Cs, _Pow, SoFar) ->
    {SoFar,Cs}.

%% scan_number_rest(Chars, Line, Column, Digits, State) -> ScanError.
%%  Collect all the "legal" not number characters and return error.

scan_number_rest([C|Cs], Line, Col, Digits, St) ->
    case name_char(C) of
        true ->
            scan_number_rest(Cs, Line, Col, [C|Digits], St);
        false ->
            scan_number_error([C|Cs], Line, Col, Digits, St)
    end;
scan_number_rest([]=Cs, Line, Col, St, Digits) ->
    {more,{Cs, Line, Col, St, Digits, fun scan_number_rest/5}};
scan_number_rest(eof=Cs, Line, Col, Digits, St) ->
    scan_number_error(Cs, Line, Col, Digits, St).

scan_number_error(Cs, Line, Col, Digits, _St) ->
    Text = lists:sublist(lists:reverse(Digits), 10),
    scan_error({illegal_chars,number,Text}, Line, Col, Line, Col, Cs).

%% scan_name(Chars, Line, Column, State) ->
%%     {ok,Token,Chars,Line,Column,State} | {more,Continuation} | ScanError.
%%  Scan names and keywords, which have the same syntax as names.

scan_name(Cs, Line, Col, St) ->
    scan_name1(Cs, Line, Col, [], St).

scan_name1_fun(Cs, Line, Col, St, Symcs) ->
    scan_name1(Cs, Line, Col, Symcs, St).

scan_name1([C|Cs], Line, Col, Symcs, St) ->
    case name_char(C) of
        true ->
            scan_name1(Cs, Line, Col+1, [C|Symcs], St);
        false ->
            Token = make_name_token(Symcs, Line),
            {ok,Token,[C|Cs],Line,Col+1,St}
    end;
scan_name1([]=Cs, Line, Col, Symcs, St) ->
    {more,{Cs,Line,Col,St,Symcs,fun scan_name1_fun/5}};
scan_name1(eof=Cs, Line, Col, Symcs, St) ->
    Token = make_name_token(Symcs, Line),
    {ok,Token,Cs,Line,Col,St}.

make_name_token(Chars, Line) ->
    Name = list_to_binary(lists:reverse(Chars)),
    case is_keyword(Name) of
        true -> {keyword_string(Name),Line};
        false -> {'NAME',Line,Name}
    end.

%% start_name_char(Char) -> true | false.
%% name_char(Char) -> true | false.
%%  Define start name chars and name chars.

%% start_name_char($^) -> false;                  %These 2 are for test cases
%% start_name_char($&) -> false;
start_name_char($_) -> true;
start_name_char(C) when C >= $A, C =< $Z -> true;
start_name_char(C) when C >= $a, C =< $z -> true;
start_name_char(_) -> false.

name_char($_) -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char(_) -> false.

keyword_string(Name) ->
    binary_to_atom(Name, latin1).               %Only latin1 in Lua

%% is_keyword(Name) -> boolean().
%%  Test if the name is a keyword.

is_keyword(<<"and">>) -> true;
is_keyword(<<"break">>) -> true;
is_keyword(<<"do">>) -> true;
is_keyword(<<"else">>) -> true;
is_keyword(<<"elseif">>) -> true;
is_keyword(<<"end">>) -> true;
is_keyword(<<"false">>) -> true;
is_keyword(<<"for">>) -> true;
is_keyword(<<"function">>) -> true;
is_keyword(<<"goto">>) -> true;
is_keyword(<<"if">>) -> true;
is_keyword(<<"in">>) -> true;
is_keyword(<<"local">>) -> true;
is_keyword(<<"nil">>) -> true;
is_keyword(<<"not">>) -> true;
is_keyword(<<"or">>) -> true;
is_keyword(<<"repeat">>) -> true;
is_keyword(<<"return">>) -> true;
is_keyword(<<"then">>) -> true;
is_keyword(<<"true">>) -> true;
is_keyword(<<"until">>) -> true;
is_keyword(<<"while">>) -> true;
is_keyword(_) -> false.

%% scan_string(Chars, Line, Column, StringQuote, State) ->
%%     {ok,Token,Chars,Line,Column,State} | {more,Continuation} | ScanError.
%%  Scan in a string. This is done in two stages, first all the string
%%  characters are collected and then they are processed to build the
%%  actual string.

-record(sd, {sline=0,                           %Start line
             scol=0,                            %Start column
             qchar='.',                         %String quote character
             schars=[]                          %String characters
             }).

scan_string(Cs, Line, Col, Qc, St) ->
    Sd = #sd{sline = Line,scol = Col,qchar = Qc,schars = []},
    scan_string(Cs, Line, Col, Qc, [], Sd, St).

scan_string_fun(Cs, Line, Col, St, {Qc,Schars,Sd}) -> 
    scan_string(Cs, Line, Col, Qc, Schars, Sd, St) .

scan_string([Qc|Cs], Line, Col, Qc, Schars, Sd0, St) ->
    Sd1 = Sd0#sd{schars = Schars},
    string_chars(Cs, Line, Col+1, Sd1, St);
scan_string([$\\,C|Cs], Line, Col, Qc, Schars, Sd, St) ->
    scan_string(Cs, Line, Col+2, Qc, [C,$\\|Schars], Sd, St);
scan_string([$\n|Cs], Line, _Col, Qc, Schars, Sd, St) ->
    scan_string(Cs, Line+1, 0, Qc, [$\n|Schars], Sd, St);
scan_string([$\\]=Cs, Line, Col, Qc, Schars, Sd, St) ->
    {more,{Cs,Line,Col,St,{Qc,Schars,Sd},fun scan_string_fun/5}};
scan_string([C|Cs], Line, Col, Qc, Schars, Sd, St) ->
    scan_string(Cs, Line, Col+1, Qc, [C|Schars], Sd, St);
scan_string([]=Cs, Line, Col, Qc, Schars, Sd, St) ->
    {more,{Cs,Line,Col,St,{Qc,Schars,Sd},fun scan_string_fun/5}};
scan_string(eof=Cs, Line, Col, _Qc, Schars, Sd0, St) ->
    Sd1 = Sd0#sd{schars = Schars},
    scan_string_error(Cs, Line, Col, Sd1, St).

scan_string_error(Cs, Line, Col, Sd, _St) ->
    #sd{sline = Sline,scol = Scol,qchar = Qc,schars = Schars} = Sd,
    Chars = lists:sublist([Qc|lists:reverse(Schars)], 10),
    scan_error({bad_format,string,Chars}, Sline, Scol, Line, Col, Cs).

%% string_chars(Chars, Line, Column, StringData, State) ->
%%     {ok,Token,Chars,Line,Column,State} | ScanError.

string_chars(Cs, Line, Col, Sd, St) ->
    try
        #sd{sline = Sline,schars = Schars} = Sd,
        Bytes = string_chars(lists:reverse(Schars), []),
        Binary = iolist_to_binary(Bytes),
        Token = {'LITERALSTRING',Sline,Binary},
        {ok,Token,Cs,Line,Col+1,St}
    catch
        _:_ ->
            scan_string_error(Cs, Line, Col, Sd, St)
    end.

string_chars([$\\ | Cs], Acc) ->
    string_bq_chars(Cs, Acc);
string_chars([$\n | _], _Acc) ->
    throw(string_error);
string_chars([C0 | Cs], Acc) ->
    C1 = string_unicode_char(C0),
    string_chars(Cs, [C1|Acc]);
string_chars([], Acc) ->
    lists:reverse(Acc).

string_unicode_char(C) when ?ASCII(C) -> C;
string_unicode_char(C0) ->
    case unicode:characters_to_binary([C0]) of
        Bin when is_binary(Bin) ->
            Bin;
        _Error ->
            throw(string_error)
    end.

%% string_bq_chars(Chars, Accumulator)
%%  Handle the backquotes characters. These always fit directly into
%%  one byte and are never UTF-8 encoded.

string_bq_chars([C1|Cs0], Acc) when C1 >= $0, C1 =< $9 ->   %1-3 decimal digits
    I1 = C1 - $0,
    case Cs0 of
        [C2|Cs1] when C2 >= $0, C2 =< $9 ->
            I2 = C2 - $0,
            case Cs1 of
                [C3|Cs2] when C3 >= $0, C3 =< $9 ->
                    I3 = C3 - $0,
                    Byte = 100*I1 + 10*I2 + I3,
                    %% Must fit into one byte!
                    (Byte =< 255) orelse throw(string_error),
                    string_chars(Cs2, [Byte|Acc]);
                _ ->
                    Byte = 10*I1 + I2,
                    string_chars(Cs1, [Byte|Acc])
            end;
        _ -> string_chars(Cs0, [I1|Acc])
    end;
string_bq_chars([$x,C1,C2|Cs], Acc) ->          %2 hex digits
    case hex_char(C1) and hex_char(C2) of
        true ->
            Byte = hex_val(C1)*16 + hex_val(C2),
            string_chars(Cs, [Byte|Acc]);
        false -> throw(string_error)
    end;
string_bq_chars([$u,${|Cs], Acc) ->             %Explicit utf-8 character
    string_bq_chars_utf8(Cs, [], Acc);
string_bq_chars([$z|Cs], Acc) ->                %Skip whitespace
    string_chars(skip_space(Cs), Acc);
string_bq_chars([C|Cs], Acc) ->
    case escape_char(C) of
        error -> throw(string_error);
        Esc -> string_chars(Cs, [Esc|Acc])
    end;
string_bq_chars([], Acc) ->
    Acc.

string_bq_chars_utf8([C|Cs], Cpchars, Acc) when ?HEX(C) ->
    string_bq_chars_utf8(Cs, Cpchars ++ [C], Acc);
string_bq_chars_utf8([$}|Cs], Cpchars, Acc) ->
    case unicode:characters_to_binary(Cpchars) of
        Bin when is_binary(Bin) ->
            string_chars(Cs, [Bin|Acc]);
        _Error ->
            throw(string_error)
    end;
string_bq_chars_utf8(_Cs, _Cpchars, _Acc) ->
    throw(string_error).

skip_space([C|Cs]) when C >= 0, C =< $\s -> skip_space(Cs);
skip_space(Cs) -> Cs.

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

hex_val(C) when C >= $0, C =< $9 -> C - $0;
hex_val(C) when C >= $a, C =< $f -> C - $a + 10;
hex_val(C) when C >= $A, C =< $F -> C - $A + 10.

escape_char($a) -> 7;                           %\a = BELL
escape_char($b) -> $\b;                         %\b = BS
escape_char($f) -> $\f;                         %\f = FF
escape_char($n) -> $\n;                         %\n = LF
escape_char($r) -> $\r;                         %\r = CR
escape_char($t) -> $\t;                         %\t = TAB
escape_char($v) -> $\v;                         %\v = VT
escape_char($\\) -> $\\;                        %\e = BACKSLASH
escape_char($") -> $";                          %\s = SPC
escape_char($') -> $';                          %\d = DEL
escape_char(_C) -> error.                       %Illegal

%% scan_error(Error, StartLine, StartCol, EndLine, EndCol, RestChars) ->
%%     {Error,Rest}.

scan_error(Error, Line, _Col, EndLine, _EndCol, Rest) ->
    Loc = Line,                                 %location(Line, Col)
    EndLoc = EndLine,                           %location(EndLine, EndCol)
    scan_error(Error, Loc, EndLoc, Rest).

scan_error(Error, ErrorLoc, EndLoc, Rest) ->
    {{error,{ErrorLoc,?MODULE,Error},EndLoc},Rest}.

%% location(Line, no_col) ->
%%     Line;
%% location(Line, Col) when is_integer(Col) ->
%%     {Line,Col}.

%% -compile({inline,[anno/1,incr_column/2,new_column/2,int_column/1]}).

anno(Location) ->
    erl_anno:new(Location).

%% token_test(Chars) -> {done,Done,Rest}.
%% token_test(Continuation, Chars) -> {done,Done,Rest}.
%% tokens_test(Chars) -> {done,Done,Rest}.
%% tokens_test(Continuation, Chars) -> {done,Done,Rest}.
%%  Carefully test the token(s) by steping over them one character at
%%  a time. This guarantees that they can handle the input string
%%  safely and correctly.

token_test(Cs) ->
    token_test([], Cs).

token_test(Cont0, [C|Cs]) ->
    case luerl_scan:token(Cont0, [C]) of
        {more,Cont1} ->
            token_test(Cont1, Cs);
        {done,_Done,_Rest}=Done ->
            Done
    end;
token_test(Cont, []) ->
    io:format("tt ~p\n", [Cont]),
    luerl_scan:token(Cont, eof).

tokens_test(Cs) ->
    tokens_test([], Cs).

tokens_test(Cont0, [C|Cs]) -> 
    case luerl_scan:tokens(Cont0, [C]) of
        {more,Cont1} ->
            tokens_test(Cont1, Cs);
        {done,_Done,_Rest}=Done ->
            Done
    end;
tokens_test(Cont, []) ->
    io:format("tst ~p\n", [Cont]),
    luerl_scan:tokens(Cont, eof).
