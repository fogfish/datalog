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
   sigma/1
]).

%%
%% sigma function, returns `datalog:pattern()` evaluator for lists
-spec sigma( datalog:pattern() ) -> datalog:heap().

sigma(Expr) ->
   fun(Heap) ->
      fun(X) ->
         stream(X, Heap, Expr)
      end
   end.

stream(X, Heap, #{'_' := Head} = Expr) ->
   %
   % 3. output stream contains tuples with named values corresponding 
   % to head definition. The head function normalize stream of 
   % matched tuples and bind variables to values extracted from ground fact.
   head(Head,
      %
      % 2. let's filter stream of tuples to match the pattern.
      % The pattern is already re-written, all bound variables are defined 
      match(Head, datalog:bind(Heap, Expr), 
         %  
         % 1. the arity of head element define super set of tuples that 
         % are matching the pattern. Let's build a stream of tuples 
         % from list that satisfy given arity.
         build(length(Head), X)
      )
   ).


%%
%% build stream of tuples from list
build(Nary, List) ->
   stream:filter(
      fun(X) -> 
         is_tuple(X) andalso size(X) =:= Nary
      end,
      stream:build(List)
   ).

%%
%% build a stream filter for each bound variable or guard
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


%%
%% normalize stream and bind head variable to ground fact value(s)
head(Head, Stream) ->
   stream:map(
      fun(X) ->
         % The library uses `map()` as data structure for tuples.
         % It allows efficiently bind deducted values to head variable.
         % Each sigma function return stream of maps.
         maps:from_list( 
            lists:filter(
               fun({Key, _}) -> is_atom(Key) end,
               lists:zip(Head, tuple_to_list(X))
            )
         )
      end,
      Stream
   ).
