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
%%   a reference implementation of stream generators
-module(datalog_list).
-compile({parse_transform, category}).

-export([
   stream/2,
   f/1,
   f/2,
   f/3
]).

%%
%% example of stream generator, it produces a stream of tuple of integers
%%
%% a(x,y) :- .stream(...)
stream([], [X1]) -> f(X1);
stream([], [X1, X2]) -> f(X1, X2);
stream([], [X1, X2, X3]) -> f(X1, X2, X3);

stream([Len|Gen], SubQ) ->
   fun(_List, _) ->
      stream:zip([gen(Len, N, Filter) || {N, Filter} <- lists:zip(Gen, SubQ)])
   end.

gen(Len, N, Filter) ->
   [identity ||
      stream:build(N),
      stream:take(Len, _),
      filter(Filter, _)
   ].

filter(Filter, Stream) ->
   Fun  = datalog:filter(Filter),
   Fun(fun(X) -> X end, Stream).

%%
%% example stream processors, it produces a stream of tuples from list
%%
f(X1) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(1, _),
         filter(1, X1, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end.

f(X1, X2) ->
   fun(List) ->
      X = [identity ||
         stream:build(List),
         nary(2, _),
         filter(1, X1, _),
         filter(2, X2, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ],
      io:format("=[ list ]=> ~p ~p -> ~p~n", [X1, X2, stream:list(X)]),
      X
   end.

f(X1, X2, X3) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(3, _),
         filter(1, X1, _),
         filter(2, X2, _),
         filter(3, X3, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end.

%%
%%
nary(N, Stream) ->
   stream:filter(fun(X) -> is_tuple(X) andalso size(X) =:= N end, Stream).

filter(I, Filter, Stream) ->
   Fun  = datalog:filter(Filter),
   Fun(fun(X) -> erlang:element(I, X) end, Stream).
