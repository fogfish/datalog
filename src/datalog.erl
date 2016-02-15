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
%%
%% @todo 
%%   * known limitation - support single horn clause only 
-module(datalog).
-include("datalog.hrl").

%% evaluator interface
-export([
   horn/2, 
   q/1, 
   q/2
]).
%% build-in data types
-export([
   list/1
]).
%% compiler interface
-export([
   p/1,
   c/1,
   c/2
]).
-export_type([q/0, eval/0, heap/0, pattern/0]).

%%%----------------------------------------------------------------------------
%%%
%%% data types
%%%
%%%----------------------------------------------------------------------------

%% datalog query is a set of horn clauses
-type q()       :: #{ name() => [head() | body()] }.
-type head()    :: [atom()].
-type body()    :: [{name(), pattern()}].
-type name()    :: atom().

%% pattern is unit of work to access ground facts persisted in external storage. 
%% sigma function uses pattern as abstract sub-query definition towards the storage 
-type pattern() :: #{'_' => head(), _ => match()}.
-type match()   :: _ | [bif()].
-type bif()     :: {'>' | '<' | '>=' | '=<' | '=/=', _}.

%% sigma function
-type eval()    :: fun( (_) -> datum:stream() ).
-type heap()    :: fun( (#{}) -> eval() ).

%%%----------------------------------------------------------------------------
%%%
%%% datalog primitives
%%%
%%%----------------------------------------------------------------------------

%%
%% build horn clause evaluator
-spec horn(head(), [any()]) -> heap().

horn(Head, List) ->
   datalog_horn:stream(Head, List).

%%
%% build datalog query evaluator
-spec q(heap()) -> eval().
-spec q(_, heap()) -> eval().

q(Expr) ->
   Expr(#{}).

q(X, Expr) ->
   Expr(X).

%%%----------------------------------------------------------------------------
%%%
%%% built-in evaluator
%%%
%%%----------------------------------------------------------------------------

%%
%% build list evaluator using pattern-match specification
-spec(list/1 :: (pattern()) -> eval()).

list(Pattern) ->
   datalog_list:sigma(Pattern).


%%%----------------------------------------------------------------------------
%%%
%%% compiler
%%%
%%%----------------------------------------------------------------------------

%%
%% parse datalog to native format
-spec p(string()) -> datalog:q().

p(Datalog) ->
   try
      {ok, Lex, _} = datalog_leex:string(Datalog), 
      {ok, Req}    = datalog_yeec:parse(Lex),
      datalog_q:native(Req)
   catch
   _:{badmatch, {error, {_, rds_parser, Reason}}} ->
      {error, Reason}; 
   _:{badmatch, {error, {_, rds_lexer,  Reason},_}} ->
      {error, Reason}; 
   _:{badmatch, Error} ->
      Error
   end. 

%%
%% compile datalog to evaluator function
-spec c(q()) -> eval().

c(Datalog) ->
   c(datalog, Datalog).

c(Mod, Datalog) ->
   [Head | Horn] = hd(maps:values(Datalog)),
   datalog:q(
      datalog:horn(Head,
         [Mod:Fun(Pat) || {Fun, Pat} <- Horn] %% TODO: check if Fun implemented by Mod
      )
   ).

