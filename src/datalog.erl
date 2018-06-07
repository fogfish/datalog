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
   c/3,
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

p(Datalog, PreProcessor) ->
   try
      {ok, Lex, _} = datalog_leex:string(Datalog), 
      {ok, Req}    = datalog_yeec:parse(Lex),
      PreProcessor(Req)
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
c(Source, Datalog) ->
   c(Source, Datalog, []).

c(Source, Datalog, Opts) ->
   case lists:keyfind(return, 1, Opts) of
      {_, maps} ->
         c_maps(Source, Datalog);
      _    ->
         c_list(Source, Datalog)
   end.

c_list(Source, [{'?', #{'@' := Goal, '_' := Head}} | Datalog]) ->
   Lprogram = lists:foldl(fun(Horn, LP) -> cc_horn(Horn, Source, LP) end, #{}, Datalog),
   Fun = maps:get(Goal, Lprogram),
   fun(Env) ->
      (Fun(Env))(Head)
   end.

c_maps(Source, [{'?', #{'@' := Goal, '_' := Head}} | Datalog]) ->
   Lprogram = lists:foldl(fun(Horn, LP) -> cc_horn(Horn, Source, LP) end, #{}, Datalog),
   {_, [Vars | _]} = lists:keyfind(Goal, 1, Datalog),
   Fun = maps:get(Goal, Lprogram),
   fun(Env) ->
      stream:map(
         fun(Tuple) -> maps:from_list( lists:zip(Vars, Tuple) ) end,
         (Fun(Env))(Head)
      )
   end.

cc_horn({Id, [Head, #{'@' := {datalog, stream}, '_' := Keys} = Sigma]}, Source, Lp) ->
   Lp#{Id => cc_sigma(Sigma#{'_' => Head, '.' => Keys}, Source, Lp)};

cc_horn({Id, [Head | Body]}, Source, Lp) ->
   Lp#{Id => datalog_vm:horn(Head, [cc_sigma(Sigma, Source, Lp) || Sigma <- Body])}.

cc_sigma(#{'@' := {datalog, stream}} = Sigma, Source, _) ->
   datalog_vm:stream(Sigma#{'@' => fun Source:stream/2});

cc_sigma(#{'@' := {datalog, Fun}} = Sigma, Source, _) ->
   Sigma#{'@' => datalog_lang:Fun(Sigma), '.' => pipe};

cc_sigma(#{'@' := Gen, '_' := Head} = Pred, Source, Lp) ->
   case maps:get(Gen, Lp, undefined) of
      undefined ->
         N = length(Head),
         Pred#{'@' => fun Source:Gen/N};
      Ref ->
         Pred#{'@' => Ref}
   end.
