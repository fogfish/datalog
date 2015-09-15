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
   new/1
  ,q/2
  ,parse/1
]).

-type(bind()    :: [integer()]).
-type(pred()    :: {atom(), bind()}).
-type(rule()    :: {atom(), bind(), [pred()]}).
-type(datalog() :: {atom(), bind(), [rule()]}).


%%
%%
new(IStream) ->
   #datalog{ns = IStream}.

%% 
%% 
q(Datalog, #datalog{ns = IStream}=State) ->
   {Head, Goal, Rules} = datalog_c:make(IStream, Datalog),
   Heap0 = heap_init(heap_size(Rules)),
   Heap1 = lists:foldl(fun heap_defs/2, Heap0, Goal),
   datalog_h:stream(hd(Rules), Heap1);

q(Datalog, IStream)
 when is_atom(IStream) ->
   q(Datalog, new(IStream)).

%%
%%
parse(Datalog) ->
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

