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

Nonterminals   CLAUSES GOAL HORN BODY ITEM GUARD TERMS TERM.
Terminals      '?-' ':-' '(' ')' '[' ']' '.' ',' '<' '=' '>' '!' '_' '^' '-' ':' symbol iri binary integer decimal.
Rootsymbol     CLAUSES.


CLAUSES -> GOAL CLAUSES :
   ['$1' | '$2'].
CLAUSES -> HORN CLAUSES :
   ['$1' | '$2'].
CLAUSES -> '$empty' :
   [].

GOAL -> '?-' symbol '(' TERMS ')' '.' :
   {unwrap('$2'), '$4'}.

HORN -> symbol '(' TERMS ')' ':-' BODY '.' :
   {unwrap('$1'), '$3', '$6'}.

BODY -> ITEM ',' BODY :
   ['$1' | '$3'].
BODY -> ITEM :
   ['$1'].

ITEM -> symbol '(' ')' :
   {unwrap('$1'), []}.

ITEM -> symbol '(' TERMS ')' :
   {unwrap('$1'), '$3'}.

ITEM -> symbol '.' symbol '(' ')' :
   {{unwrap('$1'), unwrap('$3')}, []}.

ITEM -> symbol '.' symbol '(' TERMS ')' :
   {{unwrap('$1'), unwrap('$3')}, '$5'}.

ITEM -> '.' symbol '(' ')' :
   {{datalog, unwrap('$2')}, []}.

ITEM -> '.' symbol '(' TERMS ')' :
   {{datalog, unwrap('$2')}, '$4'}.

ITEM -> symbol GUARD TERM :
   {'$2', unwrap('$1'), '$3'}.

ITEM -> symbol GUARD '(' TERMS ')' :
   {'$2', unwrap('$1'), '$4'}.

TERMS -> TERM ',' TERMS :
   ['$1' | '$3'].
TERMS -> TERM :
   ['$1'].

%%
%% xsd:anyURI
TERM -> iri :
   {iri, binary('$1')}.
TERM -> symbol ':' symbol :
   {iri, binary('$1'), binary('$3')}.

%%
%% xsd:string
TERM -> binary :
   binary('$1').

%%
%% xsd:integer
TERM -> integer :
   integer('$1').

%%
%% xsd:decimal
TERM -> decimal :
   decimal('$1').

%%
%% xsd:dateTime
TERM -> '(' integer ',' integer ',' integer ')' :
   {integer('$2'), integer('$4'), integer('$6')}.

%TERM -> integer '-' integer '-' integer

%%
%% symbols
TERM -> symbol :
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

atom({_,_,X}) -> erlang:list_to_atom(X).
binary({_,_,X}) -> erlang:list_to_binary(X).
integer({_,_,X}) -> erlang:list_to_integer(X).
decimal({_,_,X}) -> erlang:list_to_float(X).


unwrap({_,_,X}) -> X.
