%%
%%   Copyright 2014 - 2015 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   datalog

Nonterminals   CLAUSES GOAL HORN BODY ITEM PREDICATE TERMS TERM PAIRS LIT GUARD.
Terminals      '?-' ':-' '(' ')' '.' ',' '<' '=' '>' '!' '_' '^' '-' ':' '[' ']' '{' '}' symbol iri binary integer decimal.
Rootsymbol     CLAUSES.

%%
%%
CLAUSES -> GOAL CLAUSES :
   ['$1' | '$2'].
CLAUSES -> HORN CLAUSES :
   ['$1' | '$2'].
CLAUSES -> '$empty' :
   [].

%%
%%
GOAL -> '?-' symbol '(' TERMS ')' '.' :
   {atom('$2'), '$4'}.

%%
%%
HORN -> symbol '(' TERMS ')' ':-' BODY '.' :
   {atom('$1'), '$3', '$6'}.

%%
%%
BODY -> ITEM ',' BODY :
   ['$1' | '$3'].
BODY -> ITEM :
   ['$1'].

%%
%%
ITEM -> PREDICATE '(' ')' :
   {'$1', []}.

ITEM -> PREDICATE '(' TERMS ')' :
   {'$1', '$3'}.

ITEM -> symbol GUARD TERM :
   {'$2', atom('$1'), '$3'}.

%%
%%
PREDICATE -> symbol :
   atom('$1').

PREDICATE -> symbol ':' symbol :
  erlang:list_to_atom( unwrap('$1') ++ ":" ++ unwrap('$3') ).

PREDICATE -> symbol '.' symbol :
   {atom('$1'), atom('$3')}.

PREDICATE -> '.' symbol :
   {datalog, atom('$2')}.

%%
%%
TERMS -> TERM ',' TERMS :
   ['$1' | '$3'].
TERMS -> TERM :
   ['$1'].

%%
%%
TERM -> '{' PAIRS '}' :
   maps:from_list('$2').

TERM -> '(' TERMS ')' :
   erlang:list_to_tuple('$2').

TERM -> '[' TERMS ']' :
   '$2'.

TERM -> LIT :
   '$1'.

%%
%%
PAIRS -> symbol ':' TERM ',' PAIRS :
   [{binary('$1'), '$3'} | '$5'].

PAIRS -> symbol ':' TERM :
   [{binary('$1'), '$3'}].


%%
%% xsd:anyURI
LIT -> iri :
   {iri, binary('$1')}.
LIT -> symbol ':' symbol :
   {iri, binary('$1'), binary('$3')}.

%%
%% xsd:string
LIT -> binary :
   binary('$1').

%%
%% xsd:integer
LIT -> integer :
   integer('$1').
LIT -> '-' integer :
   -integer('$2').

%%
%% xsd:decimal
LIT -> decimal :
   decimal('$1').

LIT -> '-' decimal :
   -decimal('$2').

%%
%% xsd:dateTime
LIT -> integer '-' integer '-' integer symbol integer ':' integer ':' integer symbol :
   timestamp({{integer('$1'), integer('$3'), integer('$5')}, {integer('$7'), integer('$9'), integer('$11')}}).

LIT -> integer '-' integer '-' integer :
   {{integer('$1'), integer('$3'), integer('$5')}, {0, 0, 0}}.

LIT -> integer '-' integer :
   {{integer('$1'), integer('$3'), 0}, {0, 0, 0}}.

LIT -> '-' '-' integer '-' integer :
   {{0, integer('$3'), integer('$5')}, {0, 0, 0}}.

LIT -> integer ':' integer ':' integer symbol :
   {{0, 0, 0}, {integer('$1'), integer('$3'), integer('$5')}}.

%%
%% symbols
LIT -> symbol :
   atom('$1').

%%
%%
GUARD    -> '=' :
   '=:='.
GUARD    -> '>' :
   '>'.
GUARD    -> '<' :
   '<'.
GUARD    -> '>' '=' :
   '>='.
GUARD    -> '=' '>' :
   '>='.
GUARD    -> '=' '<' :
   '=<'.
GUARD    -> '<' '=' :
   '=<'.
GUARD    -> '!' '=' :
   '=/='.
GUARD    -> symbol  :
   atom('$1').

%%
%%
Erlang code.

unwrap({_, _, X}) ->
   X.

atom({_,_,X}) ->
   erlang:list_to_atom(X).

binary({_,_,X}) ->
   erlang:list_to_binary(X).

integer({_,_,X}) ->
   erlang:list_to_integer(X).

decimal({_,_,X}) ->
   erlang:list_to_float(X).

timestamp({{_, _, _}, {_, _, _}} = T) -> 
   Sec = calendar:datetime_to_gregorian_seconds(T) - 62167219200,
   {Sec div 1000000, Sec rem 1000000, 0}.
