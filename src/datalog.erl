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
   c/2,
   test/1
]).

%%
%% data types
-type(head()    :: [atom()]).
-type(pattern() :: #{'_' => head()}).

%%
%% build horn clause evaluator
-spec(horn/2 :: (head(), [any()]) -> any()).

horn(Head, List) ->
   datalog_horn:stream(Head, List).

%%
%% build datalog query evaluator
-spec(q/1 :: (any()) -> any()).
-spec(q/2 :: (any(), any()) -> datum:stream()).

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
-spec(list/1 :: (pattern()) -> any()).

list(Pattern) ->
   datalog_list:stream(Pattern).


%%%----------------------------------------------------------------------------
%%%
%%% compiler
%%%
%%%----------------------------------------------------------------------------

%%
%% parse datalog
-spec(p/1 :: (string()) -> any()).

p(Datalog) ->
   try
      {ok, Lex, _} = datalog_leex:string(Datalog), 
      {ok, Req}    = datalog_yeec:parse(Lex),
      datalog(Req)
   catch
   _:{badmatch, {error, {_, rds_parser, Reason}}} ->
      {error, Reason}; 
   _:{badmatch, {error, {_, rds_lexer,  Reason},_}} ->
      {error, Reason}; 
   _:{badmatch, Error} ->
      Error
   end. 

%%
%% compile datalog
-spec(c/1 :: (any()) -> any()).

c(Datalog) ->
   c(datalog, Datalog).

c(Mod, Datalog) ->
   [Head | Horn] = hd(maps:values(Datalog)),
   datalog:q(
      datalog:horn(Head,
         [Mod:Fun(Pat) || {Fun, Pat} <- Horn] %% TODO: check if Fun implemented by Mod
      )
   ).

%%
%% helper function to kick unit test
test(Spec) ->
   ct:run_test([{spec, Spec}]).


%%%----------------------------------------------------------------------------
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------

%%
%% convert list of horn clauses to datalog map
datalog(Horns) ->
   lists:foldl(
      fun({Horn, Head, Body}, Acc) ->
         {Pattern, Filter} = lists:partition(fun(X) -> size(X) =:= 2 end, Body),
         maps:put(Horn, [Head | hornx(Pattern, Filter)], Acc)
      end,
      #{},
      Horns 
   ).

hornx(Pattern, Filter) ->
   [pattern(X, Filter) || X <- Pattern].

pattern({Id, Pattern}, Filter) ->
   Spec = lists:foldl(
      fun(X, Acc) -> 
         filter(X, Filter, Acc) 
      end, 
      #{'_' => Pattern},
      Pattern
   ),
   {Id, Spec}.

filter(I, Filter, Acc) ->
   case 
      lists:filter(fun(X) -> erlang:element(2, X) =:= I end, Filter)
   of
      [] -> 
         Acc;
      Fs ->
         case lists:keyfind('=:=', 1, Fs) of
            false ->
               maps:put(I, [{P, F} || {P, _, F} <- Fs], Acc);
            {'=:=', _, F} ->
               maps:put(I, F, Acc)
         end
   end.

