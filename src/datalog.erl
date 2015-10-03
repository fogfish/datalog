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
%%   datalog evaluator
%%
%% @todo 
%%   * known limitation - support single horn clause only 
-module(datalog).
-include("datalog.hrl").

-export([
   q/3
  ,p/1
  ,test/1
]).
-export_type([datalog/0]).

%%
%% data-types
-type(datalog() :: {atom(), bind(), [horn()]}).
-type(horn()    :: {atom(), bind(), [pred()]}).
-type(pred()    :: {atom(), bind()}).
-type(bind()    :: [any()]).


%% 
%% evaluate datalog
-spec(q/3 :: (datalog(), any(), atom()) -> datum:stream()).

q(Datalog, Input, Mod) ->
   {_Head, Goal, Rules} = datalog_c:make(Datalog),
   Heap0 = heap_init(heap_size(Rules)),
   Heap1 = lists:foldl(fun heap_defs/2, Heap0, Goal),
   datalog_h:stream(hd(Rules), #datalog{mod = Mod, state=Input, heap = Heap1}).

%%
%% parse datalog
-spec(p/1 :: (string()) -> datalog()).

p(Datalog) ->
   try
      {ok, Lex, _} = datalog_leex:string(Datalog), 
      {ok, Req}    = datalog_yeec:parse(Lex),
      Req
   catch
   _:{badmatch, {error, {_, rds_parser, Reason}}} ->
      {error, Reason}; 
   _:{badmatch, {error, {_, rds_lexer,  Reason},_}} ->
      {error, Reason}; 
   _:{badmatch, Error} ->
      Error
   end. 

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
%%
heap_init(N) ->
   erlang:make_tuple(N, '_').

%%
%%
heap_defs('_', Heap) ->
   Heap;
heap_defs({I, X}, Heap) ->
   erlang:setelement(I, Heap, X).

%%
%% resolve required heap size of datalog program
heap_size(List)
 when is_list(List) ->
   lists:max([heap_size(X) || X <- List]);
heap_size(#h{body = List}) ->
   lists:max([heap_size(X) || X <- List]);
heap_size(#p{t = List}) ->
   lists:max([heap_size(X) || X <- List]);
heap_size(X)
 when is_tuple(X) ->
   lists:max([heap_size(Y) || Y <- erlang:tuple_to_list(X)]);
heap_size(X)
 when is_integer(X) ->
   X;
heap_size(_) ->
   0.

