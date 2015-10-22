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

-export([
   stream/1
]).

%%
%%
stream(Expr) ->
   fun(Heap) ->
      fun(X) ->
         stream(X, Heap, Expr)
      end
   end.

stream(X, Heap, #{'_' := Head} = Expr) ->
   Nary = length(Head),
   head(Head,
      match(Head, maps:merge(Expr, Heap), 
         list(Nary, X)
      )
   ).

%%
%% 
list(Nary, List) ->
   stream:filter(
      fun(X) -> 
         is_tuple(X) andalso size(X) =:= Nary
      end,
      stream:build(List)
   ).

%%
%%
head(Head, Stream) ->
   stream:map(
      fun(X) ->
         maps:from_list( 
            lists:filter(
               fun({Key, _}) -> is_atom(Key) end,
               lists:zip(Head, tuple_to_list(X))
            )
         )
      end,
      Stream
   ).

match(Head, Heap, Stream) ->
   match(1, Head, Heap, Stream).

match(I, [H|T], Heap, Stream)
 when is_atom(H) ->
   match(I + 1, T, Heap,
      pattern(maps:get(H, Heap, undefined), I, Stream)
   );
match(I, [H|T], Heap, Stream) ->
   match(I + 1, T, Heap,
      pattern(H, I, Stream)
   );

match(_, [], _, Stream) ->
   Stream.

%%
%%
pattern(undefined, _, Stream) ->
   Stream;

pattern(Filter, I, Stream)
 when is_list(Filter) ->
   lists:foldl(fun(F, Acc) -> filter(F, I, Acc) end, Stream, Filter);

pattern(Val, I, Stream) ->
   stream:filter(
      fun(X) -> 
         erlang:element(I, X) =:= Val 
      end, 
      Stream
   ).   
  
filter({F, Val}, I, Stream) ->
   stream:filter(
      fun(X) -> 
         check(F, erlang:element(I, X), Val) 
      end, 
      Stream
   ).

check('>',  A, B) -> A >  B;
check('>=', A, B) -> A >= B;
check('<',  A, B) -> A  < B;
check('=<', A, B) -> A =< B.
