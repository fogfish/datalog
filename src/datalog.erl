%%
%%   Copyright 2014 Dmitry Kolesnikov, All Rights Reserved
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
%%   datalog evaluator
-module(datalog).

%%
%% datalog interface
-export([
   sigma/2,
   sigma/3,
   horn/2, 
   q/2,
   q/3,
   p/1,
   c/2,
   c/4,
   filter/1,
   takewhile/2
]).

-export_type([q/0, eval/0, heap/0, predicate/0]).

%%%----------------------------------------------------------------------------
%%%
%%% data types
%%%
%%%----------------------------------------------------------------------------

%% datalog query is a set of horn clauses
-type q()         :: #{horn() => [head() | body()]}.
-type head()      :: [atom()].
-type body()      :: [predicate()].
-type horn()      :: atom().

%% pattern is unit of work to access ground facts persisted in external storage. 
%% sigma function uses pattern as abstract sub-query definition towards the storage 
-type predicate() :: #{'@' => name(), '_' => head(), _ => pattern()}.
-type name()      :: f() | {module(), f()}.
-type pattern()   :: _ | [filter()].
-type filter()    :: {'>' | '<' | '>=' | '=<' | '=/=', _}.
-type f()         :: atom().

%% sigma function (@todo: define types)
-type eval()    :: fun( (_) -> datum:stream() ).
-type heap()    :: fun( (map()) -> eval() ).

%%%----------------------------------------------------------------------------
%%%
%%% datalog primitives
%%%
%%%----------------------------------------------------------------------------

%%
%% sigma evaluator (generator of relation streams) 
sigma(Head, Gen) ->
   sigma(Head, #{}, Gen).

sigma(Head, Filters, Gen) ->
   datalog_sigma:stream(Filters#{'@' => Gen, '_' => Head}).

%%
%% horn clause evaluator
horn(Head, Body) ->
   datalog_horn:stream(Head, Body).

%%
%% build datalog query evaluator
-spec q(_, _) -> eval().
-spec q(_, _, _) -> eval().

q(Expr, Env) ->
   ( Expr(Env) )(stream:new(#{})).   

q(Expr, Heap, Env) ->
   ( Expr(Env) )(stream:new(Heap)).   

%%%----------------------------------------------------------------------------
%%%
%%% sigma function helpers
%%%
%%%----------------------------------------------------------------------------

%%
%% in-line stream filter(s) using predicate term and pattern 
-spec filter(pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).

filter(Pattern) -> 
   datalog_sigma:filter(fun stream:filter/2, Pattern).

%%
%% in-line stream filter(s) using predicate term and pattern 
-spec takewhile(_, pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).

takewhile(X, Pattern) -> 
   datalog_sigma:takewhile(fun stream:takewhile/2, X, Pattern).


%%%----------------------------------------------------------------------------
%%%
%%% compiler
%%%
%%%----------------------------------------------------------------------------

%%
%% parse datalog to native format
-spec p(string()) -> datalog:q().

p(Datalog) ->
   p(Datalog, fun datalog_q:native/1).

p(Datalog, Compiler) ->
   try
      {ok, Lex, _} = datalog_leex:string(Datalog), 
      {ok, Req}    = datalog_yeec:parse(Lex),
      Compiler(Req)
   catch
   _:{badmatch, {error, {_, rds_parser, Reason}}} ->
      {error, Reason}; 
   _:{badmatch, {error, {_, rds_lexer,  Reason},_}} ->
      {error, Reason}; 
   _:{badmatch, Error} ->
      Error
   end. 

%%
%% compile native datalog to evaluator function 
c(Source, [{'?', #{'@' := Goal, '_' := Head}} | Datalog]) ->
   c(Goal, Head, Source, Datalog).

c(Goal, Head, Source, Datalog) ->
   LP = cc_horn(lens:get(lens:pair(Goal), Datalog), Source, Datalog),
   fun(Env) ->
      fun(Stream) ->
         stream:map(fun(X) -> maps:with(Head, X) end, (LP(Env))(Stream))
      end
   end.

cc_horn([Head, #{'@' := {datalog, stream}, '_' := Literal} = Predicate], Source, Datalog) ->
   datalog:horn(Head, [cc(Predicate#{'_' => Head, '.' => Literal}, Source, Datalog)]);
      
cc_horn([Head | Body], Source, Datalog) ->
   datalog:horn(Head, [cc(Predicate, Source, Datalog) || Predicate <- Body]).

cc(#{'@' := {datalog, stream}, '_' := Head} = Predicate, Source, _) ->
   datalog_sigma:stream(maps:put('@', fun Source:stream/2, Predicate));

cc(#{'@' := {datalog, Fun}} = Predicate, _, _) ->
   datalog_lang:Fun(Predicate);

cc(#{'@' := {Mod, Gen}, '_' := Head} = Predicate, _, _) ->
   N = length(Head),
   datalog_sigma:stream(maps:put('@', fun Mod:Gen/N, Predicate));

cc(#{'@' := Gen, '_' := Head} = Predicate, Source, Datalog) ->
   case lens:get(lens:pair(Gen), Datalog) of
      undefined ->
         N = length(Head),
         datalog_sigma:stream(maps:put('@', fun Source:Gen/N, Predicate));
      Horn ->
         cc_horn(cc_carry_guards(Horn, Predicate), Source, Datalog)
   end.

%%
%% current horn clause applies "filters" to projection
%% this filters needs to be lifted to sigma function
cc_carry_guards([Head | Body], #{'_' := Literal} = Predicate) ->
   %% a(x,y) :- ...
   %%   ...  :- ... a(x,"Test") ...
   Lits = maps:from_list(
      lists:filter(
         fun({_, X}) -> not is_atom(X) end, 
         lists:zip(Head, Literal)
      )
   ),
   %% a(x,y) :- ...
   %%   ...  :- ... a(x,y) ... y = "Test"
   Grds = maps:with(Head, Predicate),
   Pred = maps:merge(Grds, Lits), 
   [Head | [maps:merge(Pred, X) || X <- Body]].

