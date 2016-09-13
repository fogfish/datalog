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

Nonterminals   CLAUSES HORN BODY ITEM GUARD TERMS.
Terminals      ':-' '(' ')' ':' '.' ',' '<' '=' '>' '!' '_' lit symbol.
Rootsymbol     CLAUSES.


%% DATALOG -> '?-' symbol '(' TERMS ')' '.' CLAUSES :
%%   {unwrap('$2'), '$4', '$7'}.

CLAUSES -> HORN CLAUSES :
   ['$1' | '$2'].
CLAUSES -> '$empty' :
   [].

HORN -> symbol '(' TERMS ')' ':-' BODY '.' :
   {unwrap('$1'), '$3', '$6'}.

BODY -> ITEM ',' BODY :
   ['$1' | '$3'].
BODY -> ITEM :
   ['$1'].

ITEM -> symbol '(' TERMS ')' :
   {unwrap('$1'), '$3'}.

ITEM -> symbol ':' symbol '(' TERMS ')' :
   {{unwrap('$1'), unwrap('$3')}, '$5'}.

ITEM -> ':' symbol '(' TERMS ')' :
   {{datalog, unwrap('$2')}, '$4'}.

ITEM -> symbol GUARD lit :
   {'$2', unwrap('$1'), unwrap('$3')}.


TERMS -> lit ',' TERMS :
   [unwrap('$1') | '$3'].
TERMS -> symbol ',' TERMS :
   [unwrap('$1') | '$3'].
TERMS -> '_' ',' TERMS :
   ['_' | '$3'].
TERMS -> lit :
   [unwrap('$1')].
TERMS -> symbol :
   [unwrap('$1')].
TERMS -> '_' :
   ['_'].

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


%%
%%
Erlang code.

unwrap({_,_,X}) -> X.
