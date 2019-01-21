%%
%%   Copyright 2014 - 2016 Dmitry Kolesnikov, All Rights Reserved
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
-include("datalog.hrl").

%%
%% datalog interface
-export([
   t/0,
   q/2,
   p/1,
   c/2,
   c/3,
   % schema/1,
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
% -type head()      :: [atom()].
% -type body()      :: [predicate()].
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


%%
%% stream based recursion
t() ->
   stream:unfold(fun tt/1, [stream:build([1,2,3])]).

tt([undefined]) ->
   undefined;

tt([undefined | Stack]) ->
   tt(Stack);

tt([H | T]) ->
   spin(stream:head(H), [stream:tail(H) | T]).

spin(X, Stack) when X < 100 ->
   {X, [rec(X) | Stack]};
spin(X, Stack) ->
   {X, Stack}.

rec(X) ->
   stream:build([X * 10, X * 10, X * 10]).


%%
%% evaluate compiled datalog expression with environment
-spec q(_, _) -> eval().

q(Datalog, Env) ->
   Datalog(Env).   

%%%----------------------------------------------------------------------------
%%%
%%% sigma function helpers
%%%
%%%----------------------------------------------------------------------------

%%
%% in-line stream filter(s) using predicate term and pattern 
-spec filter(pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).

filter(Pattern) -> 
   datalog_lang:filter(fun stream:filter/2, Pattern).

%%
%% in-line stream filter(s) using predicate term and pattern 
-spec takewhile(_, pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).

takewhile(X, Pattern) -> 
   datalog_lang:filter(fun stream:takewhile/2, X, Pattern).


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
         c_map(Source, Datalog);
      {_, tuples} ->
         c_tuple(Source, Datalog);
      _    ->
         c_list(Source, Datalog)
   end.

c_list(Source, [#goal{id = Goal, head = Head} | Datalog]) ->
   Lprogram = lists:foldl(fun(Horn, Acc) -> compile(Source, Horn, Acc) end, #{}, Datalog),
   Fun = maps:get(Goal, Lprogram),
   fun(Env) ->
      (Fun(Env))( head_lits(Head) )
   end.

c_tuple(Source, [#goal{id = Goal, head = Head} | Datalog]) ->
   Lprogram = lists:foldl(fun(Horn, Acc) -> compile(Source, Horn, Acc) end, #{}, Datalog),
   Fun = maps:get(Goal, Lprogram),
   fun(Env) ->
      stream:map(
         fun(Tuple) -> erlang:list_to_tuple(Tuple) end,
         (Fun(Env))( head_lits(Head) )
      )
   end.

c_map(Source, [#goal{id = Goal, head = Head} | Datalog]) ->
   Lprogram = lists:foldl(fun(Horn, Acc) -> compile(Source, Horn, Acc) end, #{}, Datalog),
   Vars = case lists:keyfind(Goal, 2, Datalog) of
      {_, _, X, _} -> 
         X;
      #source{} ->
         Head
   end,
   Fun = maps:get(Goal, Lprogram),
   fun(Env) ->
      stream:map(
         fun(Tuple) -> maps:from_list( lists:zip(Vars, Tuple) ) end,
         (Fun(Env))( head_lits(Head) )
      )
   end.

compile(Source, #source{id = Id, head = Head}, Datalog) ->
   Datalog#{Id => datalog_vm:stream(fun Source:stream/3, Id, Head)};

compile(_, #horn{id = Id} = Horn, Datalog) ->
   Datalog#{Id => compile(Horn, Datalog)};

compile(_, #join{id = Id} = Join, Datalog) ->
   Datalog#{Id => compile(Join, Datalog)};

compile(_, #recc{id = Id} = Recc, Datalog) ->
   Datalog#{Id => compile(Recc, Datalog)}.


compile(#{'@' := {datalog, Fun}} = Sigma, _Datalog) ->
   Sigma#{'@' => datalog_lang:Fun(Sigma), '.' => pipe};

compile(#{'@' := Gen} = Sigma, Datalog) ->
   Sigma#{'@' => maps:get(Gen, Datalog, Gen)};

compile(#horn{head = Head, body = Body}, Datalog) ->
   datalog_vm:horn(Head, [compile(Sigma, Datalog) || Sigma <- Body]);

compile(#join{horn = Body}, Datalog) ->
   datalog_vm:union([compile(Sigma, Datalog) || Sigma <- Body]);

compile(#recc{horn = [I, #horn{body = Body} = Horn]}, Datalog) ->
   datalog_vm:recursion(
      compile(I, Datalog),
      Horn#horn{body = [compile(Sigma, Datalog) || Sigma <- Body]}
   ).

head_lits(Head) ->
   lists:map(
      fun(X) -> 
         case is_atom(X) of 
            true -> '_';
            false -> X 
         end
      end,
      Head
   ).
