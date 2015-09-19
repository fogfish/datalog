%% @doc
%%   datalog

Nonterminals   DATALOG CLAUSES HORN BODY ITEM GUARD TERMS.
Terminals      '?-' ':-' '(' ')' '.' ',' '<' '=' '>' '!' lit var symbol.
Rootsymbol     DATALOG.


DATALOG -> '?-' symbol '(' TERMS ')' '.' CLAUSES :
   {unwrap('$2'), '$4', '$7'}.

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
ITEM -> symbol GUARD lit :
   {'$2', unwrap('$1'), unwrap('$3')}.

TERMS -> var ',' TERMS :
   [unwrap('$1') | '$3'].
TERMS -> lit ',' TERMS :
   [unwrap('$1') | '$3'].
TERMS -> symbol ',' TERMS :
   [unwrap('$1') | '$3'].
TERMS -> var :
   [unwrap('$1')].
TERMS -> lit :
   [unwrap('$1')].
TERMS -> symbol :
   [unwrap('$1')].

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
