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
%%   reference implementation
-module(datalog_list).
-compile({parse_transform, category}).

-export([
   s/2,
   p/1,
   p/2,
   p/3,
   p/4,
   p/5
]).

s(X1, X2) ->
   io:format("=[ x1 ]=> ~p ~p~n", [X1, X2]),
   fun(List) ->
      io:format("=[ list ]=> ~p~n", [List]),
      stream:map(fun erlang:tuple_to_list/1, stream:build([{1, 10}, {2, 20}, {3, 30}]))
   end.

p(X1) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(1, _),
         filter(1, X1, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end.

p(X1, X2) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(2, _),
         filter(1, X1, _),
         filter(2, X2, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end.

p(X1, X2, X3) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(2, _),
         filter(1, X1, _),
         filter(2, X2, _),
         filter(3, X3, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end.

p(X1, X2, X3, X4) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(2, _),
         filter(1, X1, _),
         filter(2, X2, _),
         filter(3, X3, _),
         filter(4, X4, _),
         stream:map(fun erlang:tuple_to_list/1, _)
      ]
   end.

p(X1, X2, X3, X4, X5) ->
   fun(List) ->
      [identity ||
         stream:build(List),
         nary(2, _),
         filter(1, X1, _),
         filter(2, X2, _),
         filter(3, X3, _),
         filter(4, X4, _),
         filter(5, X5, _),
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




%%
%% sigma function, returns `datalog:pattern()` evaluator for lists
% -spec sigma( datalog:pattern() ) -> datalog:heap().

% sigma(Expr) ->
%    fun(X) ->
%       fun(Stream) ->
%          stream(X, stream:head(Stream), Expr)
%       end
%    end.


% stream(X, Heap, #{'_' := Head} = Expr) ->
%    %
%    % 3. output stream contains tuples with named values corresponding 
%    % to head definition. The head function normalize stream of 
%    % matched tuples and bind variables to values extracted from ground fact.
%    head(Head,
%       %
%       % 2. let's filter stream of tuples to match the pattern.
%       % The pattern is already re-written, all bound variables are defined 
%       filter(Head, datalog:bind(Heap, Expr), 
%          %  
%          % 1. the arity of head element define super set of tuples that 
%          % are matching the pattern. Let's build a stream of tuples 
%          % from list that satisfy given arity.
%          build(length(Head), X)
%       )
%    ).


%%
%% build stream of tuples from list
% build(Nary, List) ->
%    stream:filter(
%       fun(X) -> 
%          is_tuple(X) andalso size(X) =:= Nary
%       end,
%       stream:build(List)
%    ).

%%
%% build a stream filter for each bound variable or guard
% filter(Head, Heap, Stream) ->
%    filter(1, Head, Heap, Stream).

% filter(I, [H|T], Heap, Stream0) ->
%    Filter  = datalog:filter(H, Heap),
%    Stream1 = Filter(fun(X) -> erlang:element(I, X) end, Stream0),
%    filter(I + 1, T, Heap, Stream1);

% filter(_, [], _, Stream) ->
%    Stream.


% %%
% %% normalize stream and bind head variable to ground fact value(s)
% head(Head, Stream) ->
%    stream:map(
%       fun(X) ->
%          % The library uses `map()` as data structure for tuples.
%          % It allows efficiently bind deducted values to head variable.
%          % Each sigma function return stream of maps.
%          maps:from_list( 
%             lists:filter(
%                fun({Key, _}) -> is_atom(Key) end,
%                lists:zip(Head, tuple_to_list(X))
%             )
%          )
%       end,
%       Stream
%    ).
