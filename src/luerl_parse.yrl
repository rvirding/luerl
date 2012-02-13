%% Copyright (c) 2012 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : luerl_parse.yrl
%% Author  : Robert Virding
%% Purpose : Parser for LUA 5.2.

%% The Grammar rules here are taken directly from the LUA 5.2
%% manual. Unfortunately it is not an LALR(1) grammar but I have
%% included a fix by Florian Weimer <fw@deneb.enyo.de> which makes it
%% so, but it needs some after processing. Actually his fix was
%% unnecessarily complex and all that was needed was to remove one
%% rule for statements.

Nonterminals
chunk block stats stat semi retstat label_stat
while_stat repeat_stat if_stat if_elseif if_else for_stat local_decl
funcname dottedname varlist var namelist
explist exp prefixexp args
functioncall
functiondef funcbody parlist
tableconstructor fieldlist fields field fieldsep
binop unop uminus.

Terminals
NAME NUMBER STRING

'and' 'break' 'do' 'else' 'elseif' 'end' 'false' 'for' 'function' 'goto' 'if' 
'in' 'local' 'nil' 'not' 'or' 'repeat' 'return' 'then' 'true' 'until' 'while' 

'+' '-' '*' '/' '%' '^' '#' '==' '~=' '<=' '>=' '<' '>' '='
'(' ')' '{' '}' '[' ']' '::' ';' ':' ',' '.' '..' '...' .


Rootsymbol chunk.

Left 100 'or'.
Left 200 'and'.
Left 300 '<' '>' '<=' '>=' '~=' '=='.
Right 400 '..'.
Left 500 '+' '-'.
Left 600 '*' '/' '%'.
Unary 700 'not' '#' uminus.
Right 800 '^'.

chunk -> block : '$1'  .

%% block ::= {stat} [retstat]

block -> stats : '$1' .
block -> stats retstat : '$1' ++ ['$2'] .

retstat -> return semi : {return,line('$1'),[]} .
retstat -> return explist semi : {return,line('$1'),'$2'} .

semi -> ';' .					%semi is never returned
semi -> '$empty' .

stats -> '$empty' : [] .
stats -> stats stat : '$1' ++ ['$2'] .

stat -> ';' : '$1' .
stat -> varlist '=' explist : {assign,line('$2'),'$1','$3'} .
%% Following rule removed to stop reduce-reduce conflict. Prefixexp
%% catches the same structure. We hope!
%% stat -> functioncall .
stat -> prefixexp : '$1' .
stat -> label_stat : '$1' .
stat -> 'break' : {break,line('$1')} .
stat -> 'goto' NAME : {goto,line('$1'),'$2'} .
stat -> 'do' block 'end' : {block,line('$1'),'$2'} .
stat -> while_stat : '$1' .
stat -> repeat_stat : '$1' .
stat -> if_stat : '$1' .
stat -> for_stat : '$1' .
stat -> function funcname funcbody : functiondef(line('$1'),'$2','$3') .
stat -> local local_decl : {local,'$2'} .

label_stat -> '::' NAME '::' : {label,line('$1'),'$2'} .

while_stat -> 'while' exp 'do' block 'end' : {while,line('$1'),'$2','$4'} .

repeat_stat -> 'repeat' block 'until' exp : {repeat,line('$1'),'$2','$4'} .

%% stat ::= if exp then block {elseif exp then block} [else block] end
if_stat -> 'if' exp 'then' block if_elseif if_else 'end' :
	{'if',line('$1'),[{'$2','$4'}|'$5'],'$6'} .

if_elseif -> if_elseif 'elseif' exp 'then' block : '$1' ++ [{'$3','$5'}] .
if_elseif -> '$empty' : [] .

if_else -> 'else' block : '$2' .
if_else -> '$empty' : [] .			%An empty block

%% stat ::= for Name '=' exp ',' exp [',' exp] do block end
%% stat ::= for namelist in explist do block end

for_stat -> 'for' NAME '=' explist do block end : 
	    numeric_for(line('$1'), '$2', '$4', '$6') .
for_stat -> 'for' namelist 'in' explist 'do' block 'end' :
	    generic_for(line('$1'), '$2', '$4', '$6') .

local_decl -> function NAME funcbody :
		  functiondef(line('$1'),'$2','$3') .
local_decl -> namelist : {assign,line(hd('$1')),'$1',[]} .
local_decl -> namelist '=' explist : {assign,line('$2'),'$1','$3'} .

%% funcname ::= Name {'.' Name} [':' Name]

funcname -> dottedname ':' NAME :
		dot_append(line('$2'), '$1', {method,line('$2'),'$3'}) .
funcname -> dottedname : '$1' .

dottedname -> NAME : '$1'.
dottedname -> dottedname '.' NAME : dot_append(line('$2'), '$1', '$3') . 

varlist -> var : ['$1'] .
varlist -> varlist ',' var : '$1' ++ ['$3'] .

var -> NAME : '$1' .
var -> prefixexp '[' exp ']' :
		dot_append(line('$2'), '$1', {key_field,line('$2'),'$3'}) .
var -> prefixexp '.' NAME : dot_append(line('$2'), '$1', '$3') . 

namelist -> NAME : ['$1'] .
namelist -> namelist ',' NAME : '$1' ++ ['$3'] .

explist -> exp : ['$1'] .
explist -> explist ',' exp : '$1' ++ ['$3'] .

exp -> 'nil' : '$1' .
exp -> 'false' : '$1' .
exp -> 'true' : '$1' .
exp -> NUMBER : '$1' .
exp -> STRING : '$1' .
exp -> '...' : '$1' .
exp -> functiondef : '$1' .
exp -> prefixexp : '$1' .
exp -> tableconstructor : '$1' .
exp -> exp binop exp : {op,line('$2'),cat('$2'),'$1','$3'} .
exp -> unop exp : {op,line('$1'),cat('$1'),'$2'} .
exp -> uminus : '$1' .

prefixexp -> var : '$1' .
prefixexp -> functioncall : '$1' .
prefixexp -> '(' exp ')' : {single,line('$1'),'$2'} .

functioncall -> prefixexp args :
		dot_append(line('$1'), '$1', {functioncall,line('$1'), '$2'}) .
functioncall -> prefixexp ':' NAME args :
		dot_append(line('$2'), '$1', {method,line('$2'),'$3','$4'}) .

args -> '(' ')' : [] .
args -> '(' explist ')' : '$2' .
args -> tableconstructor : ['$1'] .	%Syntactic sugar
args -> STRING : ['$1'] .		%Syntactic sugar

functiondef -> 'function' funcbody : functiondef(line('$1'), '$2').

funcbody -> '(' ')' block 'end' : {[],'$3'} .
funcbody -> '(' parlist ')' block 'end' : {'$2','$4'} .

parlist -> namelist : '$1' .
parlist -> namelist ',' '...' : '$1' ++ ['$3'] .
parlist -> '...' : ['$1'] .

tableconstructor -> '{' '}' : {table,line('$1'),[]} .
tableconstructor -> '{' fieldlist '}' : {table,line('$1'),'$2'} .

fieldlist -> fields : '$1' .
fieldlist -> fields fieldsep : '$1' .

fields ->  field : ['$1'] .
fields ->  fields fieldsep field : '$1' ++ ['$3'] .

field -> '[' exp ']' '=' exp : {key_field,line('$1'),'$2','$5'} .
field -> NAME '=' exp : {name_field,line('$1'),'$1','$3'} .
field -> exp : {exp_field,line('$1'),'$1'} .

fieldsep -> ',' .
fieldsep -> ';' .

binop -> '+' : '$1'.
binop -> '-' : '$1'.
binop -> '*' : '$1'.
binop -> '/' : '$1'.
binop -> '^' : '$1'.
binop -> '%' : '$1'.
binop -> '..' : '$1'.
binop -> '<' : '$1'.
binop -> '<=' : '$1'.
binop -> '>' : '$1'.
binop -> '>=' : '$1'.
binop -> '==' : '$1'.
binop -> '~=' : '$1'.
binop -> 'and' : '$1'.
binop -> 'or' : '$1'.

%%unop -> '-' : '$1'.
unop -> 'not' : '$1'.
unop -> '#' : '$1'.
     
uminus -> '-' exp : {op,line('$1'),'-','$2'}.

Erlang code.

-export([chunk/1,code/1]).

chunk(Ts) -> parse(Ts).

code(Ts) -> parse(Ts).

cat(T) -> element(1, T).
line(T) -> element(2, T).

%% numeric_for(Line, LoopVar, [Init,Test,Upd], Block).

numeric_for(Line, Var, [Init,Limit], Block) ->
    {for,Line,Var,Init,Limit,Block};
numeric_for(Line, Var, [Init,Limit,Step], Block) ->
    {for,Line,Var,Init,Limit,Step,Block};
numeric_for(Line, _, _, _) ->		%Wrong number of expressions
    return_error(Line, "illegal for").

%% generic_for(Line, Names, ExpList, Block).

generic_for(Line, Names, Exps, Block) ->
    {for,Line,Names,Exps,Block}.

%% functiondef(Line, Name, {Parameters,Body}).
%% functiondef(Line, {Parameters,Body}).

functiondef(Line, Name, {Pars,Body}) ->
    {functiondef,Line,Name,Pars,Body}.

functiondef(Line, {Pars,Body}) ->
    {functiondef,Line,Pars,Body}.

dot_append(Line, {'.',L,H,T}, Last) ->
    {'.',L,H,dot_append(Line, T, Last)};
dot_append(Line, H, Last) -> {'.',Line,H,Last}.
