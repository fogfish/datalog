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

-export([stream/3]).

%%
%% example of ground facts generator, it produces a stream of tuple from input list
%%
%% a(x,y).
stream(_, _, [X1]) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(1, _),
         filter(1, X1, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end;

stream(_, _, [X1, X2]) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(2, _),
         filter(1, X1, _),
         filter(2, X2, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end;

stream(_, _, [X1, X2, X3]) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(3, _),
         filter(1, X1, _),
         filter(2, X2, _),
         filter(3, X3, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end;

stream(_, _, [X1, X2, X3, X4]) ->
   fun(List) ->
      X = [identity ||
         stream:build(List),
         nary(4, _),
         filter(1, X1, _),
         filter(2, X2, _),
         filter(3, X3, _),
         filter(4, X4, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ],
      io:format("==> ~p ~p ~p~n", [X1, X2, stream:list(X)]),
      X
   end.

%%
%%
nary(N, Stream) ->
   stream:filter(fun(X) -> is_tuple(X) andalso size(X) =:= N end, Stream).

filter(I, Filter, Stream) ->
   Fun  = datalog:filter(Filter),
   Fun(fun(X) -> erlang:element(I, X) end, Stream).
