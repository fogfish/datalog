%% @doc
%%   datalog

Nonterminals   DATALOG CLAUSES HORN BODY VARS.
Terminals      '?-' ':-' '(' ')' '.' ',' lit symbol.
Rootsymbol     DATALOG.


DATALOG -> '?-' symbol '(' VARS ')' '.' CLAUSES :
   {unwrap('$2'), '$4', '$7'}.

CLAUSES -> HORN CLAUSES :
   ['$1' | '$2'].
CLAUSES -> '$empty' :
   [].

HORN -> symbol '(' VARS ')' ':-' BODY '.' :
   {unwrap('$1'), '$3', '$6'}.

BODY -> symbol '(' VARS ')' ',' BODY :
   [{unwrap('$1'), '$3'} | '$6'].
BODY -> symbol '(' VARS ')' :
   [{unwrap('$1'), '$3'}].

VARS -> symbol ',' VARS :
   [unwrap('$1') | '$3'].
VARS -> symbol :
   [unwrap('$1')].

%%
%%
Erlang code.

unwrap({_,_,X}) -> X.
