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
   q/2, 
   q/3,
   bind/2,
   filter/2,
   takewhile/2,
   unique/1
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
-type pattern()   :: _ | [guard()].
-type guard()     :: {'>' | '<' | '>=' | '=<' | '=/=', _}.
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
%% build horn clause evaluator
-spec horn(head(), body()) -> heap().

horn(Head, Body) ->
   fun(X) ->
      datalog_horn:stream(Head, [Fun(X) || Fun <- Body])
   end.

%%
%% build datalog query evaluator
-spec q(_, _) -> eval().
-spec q(_, _, _) -> eval().

q(Expr, X) ->
   ( Expr(X) )(stream:new(#{})).   

q(Expr, Heap, X) ->
   ( Expr(X) )(stream:new(Heap)).   

%%%----------------------------------------------------------------------------
%%%
%%% build-in predicates
%%%
%%%----------------------------------------------------------------------------

%%
%% a predicate ensures unique terms within the stream
%% ```
%% h(x,z) :- a(x,y), b(y,z), :unique(x,z). 
%% ``` 
-spec unique(predicate()) -> _.

unique(X) -> datalog_lang:unique(X).  






%%
%% bind sigma pattern with resolved variable (resolved previously by sigma)  
-spec bind(map(), pattern()) -> pattern().

bind(Heap, Pattern) ->
   % maps:merge(...) provides out-of-implementation for the library
   % it merges two maps into a single map. if two keys (variables) exists in both maps 
   % the value in Pattern will be superseded by the value in Heap.
   % Thus any BIF are converted to pattern match
   maps:merge(Pattern, Heap).

%%
%% build a in-line stream filter(s)
-spec filter(_, pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).
-spec takewhile(_, pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).

filter(X, Pattern) ->
   streamwith(fun stream:filter/2, X, Pattern).

takewhile(X, Pattern) ->
   streamwith(fun stream:takewhile/2, X, Pattern).

%%
streamwith(Fun, X, Pattern)
 when is_atom(X) ->
   %% guard term p(..., X, ...), X > 5
   case Pattern of
      #{X := Filter} when is_list(Filter) ->
         streamwith(Fun, Filter);
      #{X := Value} ->
         streamwith(Fun, [{'=:=', Value}]);
      _ ->
         fun(_, Stream) -> Stream end
   end;

streamwith(Fun, Value, _) ->
   %% in-line term p(..., 5, ...)
   streamwith(Fun, [{'=:=', Value}]).

streamwith(Fun, Filters) ->
   fun(Lens, Stream) ->
      lists:foldl(fun(Filter, Acc) -> filter(Fun, Filter, Lens, Acc) end, Stream, Filters)
   end.

filter(Fun, {F, Val}, Lens, Stream) ->
   Fun(fun(X) -> check(F, Lens(X), Val) end, Stream).

check('>',   A, B) -> A >  B;
check('>=',  A, B) -> A >= B;
check('<',   A, B) -> A  < B;
check('=<',  A, B) -> A =< B;
check('=:=', A, B) -> A =:= B.

%%%----------------------------------------------------------------------------
%%%
%%% built-in evaluator
%%%
%%%----------------------------------------------------------------------------

%%
%% build list evaluator using pattern-match specification
-spec list(pattern()) -> eval().

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
%% compile native datalog to evaluator function 
-spec c(q()) -> heap().

c(Datalog) ->
   c(datalog, Datalog).

c(Mod, Datalog) ->
   {_Horn, [Head | Body]} = hd(maps:to_list(Datalog)),
   datalog:horn(Head,
      [cc(Mod, Pat) || Pat <- Body]
   ).

cc(_, #{'@' := {Mod, Fun}} = Pat) ->
   cc_eval(Mod, Fun, Pat);

cc(Mod, #{'@' := Fun} = Pat) ->
   cc_eval(Mod, Fun, Pat).

cc_eval(Mod, Fun, Pat) ->
   case 
      lists:keyfind(Fun, 1, Mod:module_info(exports))
   of
      {Fun, 1} ->
         Mod:Fun(Pat);
      _        ->
         Mod:sigma(Pat)
   end.

