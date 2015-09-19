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
   make/1
]).

%%
%% compile datalog query, takes stream-interface and datalog program
%% returns abstract syntax tree for horn clauses and variable mapping
make({Head, Goal0, Rules0}) ->
   %% compile rules
   {Rules1, Var} = datalog_t:compile(
      compile(Rules0)
   ),
   Rules2 = datalog_t:prepare(Rules1),
   %% compile goal
   #h{head=Bind} = lists:keyfind(Head, #h.id, Rules2),
   Goal1  = lists:map(
      fun({I, X} = Xx) ->
         case dict:find(X, Var) of
            {ok, I} ->
               '_';
            _ ->
               Xx
         end
      end,
      lists:zip(Bind, Goal0)
   ),
   {Head, Goal1, Rules2}.

%%
%% compile datalog to abstract structures
compile({Id, Term}) -> 
   % compile predicate
   #p{id = Id, t = Term};

compile({'>', Id, Term}) ->
   % compile built-in filter
   #f{id = '>', t = [Id], s = Term};

compile({Id, Head, Body}) ->
   % compile horn clause
   #h{id = Id, head = Head, body = [compile(X) || X <- Body]};

compile(Datalog) ->
   [compile(X) || X <- Datalog].


