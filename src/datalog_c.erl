%% @doc
%%   datalog compiler
%%   
%% @todo: move text to README.md
%%   It replaces variables with integer id. The variable is replaced with 
%%   positive int if predicate egress variable or with negative int if variable 
%%   is consumed by predicates e.g.
%%   c(X, Z) :- a(X, Y), b(Y, Z), d(Y, F)
%%     1  3      +1 +2    -2 +3    -2 +4
-module(datalog_c).
-include("datalog.hrl").

-export([
   make/2
]).

%%
%% compile datalog query
make(Ns, Query) ->
   datalog_t:prepare(
      datalog_t:compile(
         compile(Query, Ns)
      )
   ).

%%
%% compile datalog to abstract structures
compile({Id, Term}, Ns) -> 
   #p{ns = Ns, id = Id, t = Term};

%% compile built-in filter
compile({'>', Id, Term}, _Ns) ->
   #p{ns = filter, id = '>', t = [Id], s = Term};

%% compile horn clause
compile({Id, Head, Body}, Ns) ->
   #h{id = Id, head = Head, body = [compile(X, Ns) || X <- Body]};

compile(Query, Ns) ->
   [compile(X, Ns) || X <- Query].

   

