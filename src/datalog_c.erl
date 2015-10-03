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
%%   datalog compiler
%%   
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

%% compile built-in filter
compile({'>', Id, Term}) ->
   #f{id = '>', t = [Id], s = Term};

compile({'<', Id, Term}) ->
   #f{id = '<', t = [Id], s = Term};

%% compile horn clause
compile({Id, Head, Body}) ->
   #h{id = Id, head = Head, body = [compile(X) || X <- Body]};

compile(Datalog) ->
   [compile(X) || X <- Datalog].


