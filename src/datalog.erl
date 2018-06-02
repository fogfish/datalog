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

%%
%% datalog interface
-export([
   sigma/2,
   sigma/3,
   horn/2, 
   q/2, 
   q/3
]).

%%
%% compiler interface
-export([
   p/1,
   % pflat/1,
   c/2,
   c/3 %,
   % cflat/2
]).

%%
%% datalog built-in predicates
-export([
   unique/1, flat/1,
   eq/1, ne/1, lt/1, gt/1, le/1, ge/1
]).

%%
%% sigma function utility 
-export([
   filter/1,
   takewhile/2
]).

%%
%% build-in data types
-export([
   list/1
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
%%% build-in predicates
%%%
%%%----------------------------------------------------------------------------

%%
%% a predicate ensures unique terms within the stream
%% ```
%% h(x,z) :- a(x,y), b(y,z), .unique(x,z). 
%% ``` 
-spec unique(predicate()) -> _.

unique(X) -> datalog_lang:unique(X).

-spec flat(predicate()) -> _.

flat(X) -> datalog_lang:flat(X).

%%
%% comparison predicates
%% ```
%% h(x,z) :- a(x,y), b(y,z), .eq(x,z). 
%% ``` 
-spec eq(predicate()) -> _.

eq(X) -> datalog_lang:eq(X).

-spec ne(predicate()) -> _.

ne(X) -> datalog_lang:ne(X).

-spec lt(predicate()) -> _.

lt(X) -> datalog_lang:lt(X).

-spec gt(predicate()) -> _.

gt(X) -> datalog_lang:gt(X).

-spec le(predicate()) -> _.

le(X) -> datalog_lang:le(X).

-spec ge(predicate()) -> _.

ge(X) -> datalog_lang:ge(X).


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
% -spec c(datalog:q()) -> heap().

c(Source, [{'?', #{'@' := Goal}} | Datalog]) ->
   c(Goal, Source, Datalog).

c(Goal, Source, Datalog) ->
   cc_horn(lens:get(lens:pair(Goal), Datalog), Source, Datalog).

cc_horn([Head, #{'@' := {datalog, stream}, '_' := Literal} = Predicate], Source, Datalog) ->
   datalog:horn(Head, [cc(Predicate#{'_' => Head, '.' => Literal}, Source, Datalog)]);
      
cc_horn([Head | Body], Source, Datalog) ->
   datalog:horn(Head, [cc(Predicate, Source, Datalog) || Predicate <- Body]).

cc(#{'@' := {datalog, stream}, '_' := Head} = Predicate, Source, _) ->
   N = length(Head),
   datalog_sigma:stream(maps:put('@', fun Source:stream/N, Predicate));

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
         cc_horn(Horn, Source, Datalog)
   end.


% c(Datalog) ->
%    c(datalog, Datalog).

% c(Mod, Datalog) ->
%    {_Horn, [Head | Body]} = hd(maps:to_list(Datalog)),
%    datalog:horn(Head,
%       [cc(Mod, Pat) || Pat <- Body]
%    ).

% cc(_, #{'@' := {Mod, Fun}} = Pat) ->
%    cc_eval(Mod, Fun, Pat);

% cc(Mod, #{'@' := Fun} = Pat) ->
%    cc_eval(Mod, Fun, Pat).

% cc_eval(Mod, Fun, Pat) ->
%    case 
%       lists:keyfind(Fun, 1, Mod:module_info(exports))
%    of
%       {Fun, 1} ->
%          Mod:Fun(Pat);
%       _        ->
%          Mod:sigma(Pat)
%    end.

%%
%% compile native datalog horn as single function 
% -spec cflat(atom(), datalog:q()) -> heap().

% cflat(Mod, [#{'_' := Head} | Body]) ->
%    datalog:horn(Head,
%       [cc_flat(Mod, Pat) || Pat <- Body]
%    ).

% cc_flat(Mod, [Head | Body]) ->
%    Env = lists:foldl(
%       fun(X, Acc) -> 
%          maps:merge(Acc, maps:without(['@', '_'], X)) 
%       end, 
%       #{}, 
%       Body
%    ),
%    Seq = [maps:with(['@', '_'], X) || X <- Body],
%    Mod:sigma(Env#{'@' => Seq, '_' => Head}).
