%% Copyright (c) 2013-2019 Robert Virding
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

%% File    : luerl_comp.erl
%% Author  : Robert Virding
%% Purpose : A basic LUA 5.3 compiler for Luerl.

%% This is the main loop of the Luerl compiler. While we can handle
%% errors in this loop they should never occur as Lua basically allows
%% almost everything that gets past the parser. The only exception are
%% goto's to undefined labels, but we don't handle goto's yet.
%%
%% We also have the first pass here. It normalises the code and
%% converts to an internal form.

-module(luerl_comp).


-export([file/1,file/2,string/1,string/2,chunk/1,chunk/2]).
-export([forms/1,forms/2]).

-export([debug_print/3]).

-import(lists, [member/2,keysearch/3,mapfoldl/3,foreach/2]).

-include_lib("kernel/include/file.hrl").

-include("luerl.hrl").
-include("luerl_comp.hrl").

?MODULEDOC(false).

%% The main Lua compiler state.

-record(luacomp, {base="",			%Base name
		  ldir="",			%Lua file dir
		  lfile="",			%Lua file
		  odir=".",			%Output directory
		  opts=[],			%User options
		  code=none,			%Code after last pass.
		  cinfo=none,			%Common compiler info
		  errors=[],
		  warnings=[]
		 }).

-define(NOFILE, "-no-file-").

%% file(Name) ->
%%     {ok,Chunk} | {error,Error,Warnings} | error}.
%% file(Name, Options) ->
%%     {ok,Chunk} | {error,Error,Warnings} | error}.

file(Name) -> file(Name, [verbose,report]).

file(Name, Opts) ->
    St0 = #luacomp{opts=Opts},
    St1 = filenames(Name, St0),
    do_compile(file_passes(), St1).

%% string(String) ->
%%     {ok,Chunk} | {error,Error,Warnings} | error}.
%% string(String, Options) ->
%%     {ok,Chunk} | {error,Error,Warnings} | error}.

string(Str) -> string(Str, [verbose,report]).

string(Str, Opts) when is_binary(Str) ->
    string(binary_to_list(Str), Opts);
string(Str, Opts) when is_list(Str) ->
    St0 = #luacomp{opts=Opts,code=Str},
    File = prop(module, Opts, ?NOFILE),
    St1 = filenames(File, St0),
    do_compile(list_passes(), St1).

%% chunk(Chunk) ->
%%     {ok,Chunk} | {error,Error,Warnings} | error}.
%% chunk(Chunk, Options) ->
%%     {ok,Chunk} | {error,Error,Warnings} | error}.

chunk(Chunk) -> chunk(Chunk, [verbose,report]).

chunk(Chunk, Opts) ->
    St0 = #luacomp{opts=Opts,code=Chunk},
    File = prop(module, Opts, ?NOFILE),
    St1 = filenames(File, St0),
    do_compile(chunk_passes(), St1).

%% forms(Forms)
%% forms(Forms, Options)
%%  The deprecated fuuncttions for compiling a chunk.

forms(C) -> chunk(C).
forms(C, Opts) -> chunk(C, Opts).

%% do_compile(Passes, CompilerState) ->
%%     {ok,Code} | {error,Error,Warnings} | error.

do_compile(Passes, St0) ->
    %% The compiler state already contains the filenames.
    Cinfo = compiler_info(St0),                 %The compiler info
    St1 = St0#luacomp{cinfo=Cinfo},
    case do_passes(Passes, St1) of
        {ok,St2} -> do_ok_return(St2);
        {error,St2} -> do_error_return(St2)
    end.

%% filenames(File, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(?NOFILE, St) -> St#luacomp{lfile=?NOFILE};
filenames(File, St) ->
    Suffix = ".lua",
    %% Test for explicit outdir.
    Odir = prop(outdir, St#luacomp.opts, "."),
    Ldir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    Lfile = luafile(Ldir, Base, Suffix),
    St#luacomp{base=Base,
	       ldir=Ldir,
	       lfile=Lfile,
	       odir=Odir}.

luafile(".", Base, Suffix) -> Base ++ Suffix;
luafile(Dir, Base, Suffix) ->
    filename:join(Dir, Base ++ Suffix).

%% prop(Key, PropList, Default) -> Value | Default.
%%  Find Key, Val from PropList else Default.

prop(Key, [{Key,Val}|_], _Def) -> Val;
prop(Key, [_|Plist], Def) ->  prop(Key, Plist, Def);
prop(_Key, [], Def) -> Def.

%% compiler_info(State) -> CompInfo.
%%  Initialise the #cinfo record passed into all compiler passes.

compiler_info(#luacomp{lfile=F,opts=Opts}) ->
    %% The file option may get a binary so we are helpful.
    Vfile = iolist_to_binary(prop(file, Opts, F)),
    #cinfo{lfile=F,vfile=Vfile,opts=Opts}.

%% file_passes() -> [Pass].
%% list_passes() -> [Pass].
%% chunk_passes() -> [Pass].
%%  Build list of passes.

file_passes() ->				%Reading from file
    [{do,fun do_scan_file/1},
     {when_flag,to_scan,{done,fun(St) -> {ok,St} end}},
     {do,fun do_parse/1} |
     chunk_passes()].

list_passes() ->				%Scanning string
    [{do,fun do_scan_string/1},
     {when_flag,to_scan,{done,fun(St) -> {ok,St} end}},
     {do,fun do_parse/1} |
     chunk_passes()].

chunk_passes() ->				%Doing the chunk
    [{when_flag,to_parse,{done,fun(St) -> {ok,St} end}},
     {do,fun do_init_comp/1},
     {do,fun do_comp_normalise/1},
     {when_flag,to_norm,{done,fun(St) -> {ok,St} end}},
     {do,fun do_comp_lint/1},
     {do,fun do_comp_vars/1},
     {when_flag,to_vars,{done,fun(St) -> {ok,St} end}},
     %% {do,fun do_comp_locf/1},
     {do,fun do_comp_env/1},
     {when_flag,to_env,{done,fun(St) -> {ok,St} end}},
     {do,fun do_code_gen/1},
     {unless_flag,no_iopt,{do,fun do_peep_op/1}}].

%% do_passes(Passes, State) -> {ok,State} | {error,Reason}.
%%  Interpret the list of commands in a pass.
%%
%%  Commands can be one of:
%%
%%  {when_flag,Flag,Cmd}
%%  {unless_flag,Flag,Cmd}
%%  {do,Fun}
%%  {done,PrintFun,Ext}

do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
	{ok,St1} -> do_passes(Ps, St1);
	{error,St1} -> {error,St1}
    end;
do_passes([{when_flag,Flag,Cmd}|Ps], St) ->
    case member(Flag, St#luacomp.opts) of
	true -> do_passes([Cmd|Ps], St);
	false -> do_passes(Ps, St)
    end;
do_passes([{unless_flag,Flag,Cmd}|Ps], St) ->
    case member(Flag, St#luacomp.opts) of
	true -> do_passes(Ps, St);
	false -> do_passes([Cmd|Ps], St)
    end;
do_passes([{done,Fun}|_], St) ->
    Fun(St);
do_passes([], St) -> {ok,St}.

%% do_scan_file(State) -> {ok,State} | {error,State}.
%% do_scan_string(State) -> {ok,State} | {error,State}.
%% do_parse(State) -> {ok,State} | {error,State}.
%% do_init_comp(State) -> {ok,State} | {error,State}.
%% do_comp_normalise(State) -> {ok,State} | {error,State}.
%% do_comp_lint(State) -> {ok,State} | {error,State}.
%% do_comp_vars(State) -> {ok,State} | {error,State}.
%% do_comp_env(State) -> {ok,State} | {error,State}.
%% do_comp_cg(State) -> {ok,State} | {error,State}.
%% do_comp_peep(State) -> {ok,State} | {error,State}.
%%  The actual compiler passes.

do_scan_file(#luacomp{lfile=Name,opts=Opts}=St) ->
    case luerl_io:scan_file(Name, 1) of
        {ok,Ts} ->
            debug_print(Opts, "scan: ~p\n", [Ts]),
            {ok,St#luacomp{code=Ts}};
        {eof,_} ->
            {ok,St#luacomp{code=[]}};
        {error,E} ->
            {error,St#luacomp{errors=[E]}}
    end.

do_scan_string(#luacomp{code=Str,opts=Opts}=St) ->
    case luerl_scan:string(Str) of
	{ok,Ts,_} ->
	    debug_print(Opts, "scan: ~p\n", [Ts]),
	    {ok,St#luacomp{code=Ts}};
	{error,E,_} -> {error,St#luacomp{errors=[E]}}
    end.

do_parse(#luacomp{code=Ts,opts=Opts}=St) ->
    case luerl_parse:chunk(Ts) of
	{ok,Chunk} ->
	    debug_print(Opts, "parse: ~p\n", [Chunk]),
	    {ok,St#luacomp{code=Chunk}};
	{error,E} -> {error,St#luacomp{errors=[E]}}
    end.

do_init_comp(#luacomp{}=St) ->
    %% Nothing to do here now.
    {ok,St}.

do_comp_normalise(#luacomp{code=Code0,cinfo=Cinfo}=St) ->
    {ok,Code1} = luerl_comp_normalise:chunk(Code0, Cinfo),
    {ok,St#luacomp{code=Code1}}.

do_comp_lint(#luacomp{code=Code,cinfo=Cinfo}=St) ->
    case luerl_comp_lint:chunk(Code, Cinfo) of
	{ok,Ws} -> {ok,St#luacomp{warnings=Ws}};
	{error,Es,Ws} -> {error,St#luacomp{errors=Es,warnings=Ws}}
    end.

do_comp_vars(#luacomp{code=Code0,cinfo=Cinfo}=St) ->
    {ok,Code1} = luerl_comp_vars:chunk(Code0, Cinfo),
    {ok,St#luacomp{code=Code1}}.

%% do_comp_locf(#luacomp{code=Code0,cinfo=Cinfo}=St) ->
%%     case luerl_comp_locf:chunk(Code0, Cinfo) of
%%         {ok,Code1} -> {ok,St#luacomp{code=Code1}};
%%         {ok,Code1,Ws} -> {ok,St#luacomp{code=Code1,warnings=Ws}};
%%         {error,Es} -> {error,St#luacomp{errors=Es}}
%%     end.

do_comp_env(#luacomp{code=Code0,cinfo=Cinfo}=St) ->
    {ok,Code1} = luerl_comp_env:chunk(Code0, Cinfo),
    {ok,St#luacomp{code=Code1}}.

do_code_gen(#luacomp{code=Code0,cinfo=Cinfo}=St) ->
    {ok,Code1} = luerl_comp_cg:chunk(Code0, Cinfo),
    {ok,St#luacomp{code=Code1}}.

do_peep_op(#luacomp{code=Code0,cinfo=Cinfo}=St) ->
    {ok,Code1} = luerl_comp_peep:chunk(Code0, Cinfo),
    {ok,St#luacomp{code=Code1}}.

do_ok_return(#luacomp{lfile=Lfile,opts=Opts,code=C,warnings=Ws}) ->
    Report = lists:member(report, Opts),
    ?IF(Report, list_warnings(Lfile, Ws), ok),
    {ok,C}.

do_error_return(#luacomp{lfile=Lfile,opts=Opts,errors=Es,warnings=Ws}) ->
    Report = lists:member(report, Opts),
    Return = lists:member(return, Opts),
    ?IF(Report, begin list_errors(Lfile, Es), list_warnings(Lfile, Ws) end, ok),
    ?IF(Return, {error,Es,Ws}, error).

debug_print(Opts, Format, Args) ->
    ?DEBUG_PRINT(Format, Args, Opts).

list_warnings(F, Ws) ->
    foreach(fun ({Line,Mod,Warn}) ->
                    Cs = Mod:format_error(Warn),
                    io:format("~s:~w: Warning: ~s\n", [F,Line,Cs]);
                ({Mod,Warn}) ->
                    Cs = Mod:format_error(Warn),
                    io:format("~s: Warning: ~s\n", [F,Cs])
            end, Ws).

list_errors(F, Es) ->
    foreach(fun ({Line,Mod,Error}) ->
                    Cs = Mod:format_error(Error),
                    io:format("~s:~w: ~s\n", [F,Line,Cs]);
                ({Mod,Error}) ->
                    Cs = Mod:format_error(Error),
                    io:format("~s: ~s\n", [F,Cs])
            end, Es).
