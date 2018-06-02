%%
%%   Copyright 2016 Dmitry Kolesnikov, All Rights Reserved
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
%%   datalog sigma evaluator
-module(datalog_sigma).

-include_lib("datum/include/datum.hrl").

-export([
   stream/1,
   % bind/2,
   filter/2
]).

stream(Predicate) ->
   fun(Env) ->
      fun(Stream) ->
         stream:unfold(fun unfold/1, {Predicate, Env, Stream})
      end
   end.

%%
%% 
unfold({_, _, ?stream()}) ->
   stream:new();

unfold({#{'_' := Head} = Predicate, Env, Stream}) ->
   Left  = stream:head(Stream),
   Tail  = stream:tail(Stream),
   Sigma = head(Head, (build(maps:merge(Predicate, Left)))(Env)),
   unfold({Predicate, Env, Sigma, Left, Tail});

unfold({Predicate, Env, ?stream(), _, Tail}) ->
   unfold({Predicate, Env, Tail});

unfold({Predicate, Env, Stream, Left, Tail}) ->
   {
      maps:merge(stream:head(Stream), Left),
      {Predicate, Env, stream:tail(Stream), Left, Tail}
   }.

%%
%% build a "tuple" stream using stream generator
build(#{'@' := Gen, '.' := Literal, '_' := Head} = Predicate) ->
   Gen(Literal, [term(X, Predicate) || X <- Head]);
build(#{'@' := Gen, '_' := [X1]} = Predicate) ->
   Gen(term(X1, Predicate));
build(#{'@' := Gen, '_' := [X1, X2]} = Predicate) ->
   Gen(term(X1, Predicate), term(X2, Predicate));
build(#{'@' := Gen, '_' := [X1, X2, X3]} = Predicate) ->
   Gen(term(X1, Predicate), term(X2, Predicate), term(X3, Predicate));
build(#{'@' := Gen, '_' := [X1, X2, X3, X4]} = Predicate) ->
   Gen(term(X1, Predicate), term(X2, Predicate), term(X3, Predicate), term(X4, Predicate)).

term(T, Predicate)
 when is_atom(T) ->
   case Predicate of
      #{T := Value} -> Value;
      _             -> '_'
   end;
term(T, _) ->
   T.

%%
%% normalize stream and bind head variable to ground fact value(s)
%% The library uses `map()` as data structure for tuples.
%% It allows efficiently bind deducted values to head variable.
%% Each sigma function return stream of tuples.
head(Head, Stream) ->
   stream:map(
      fun(Tuple) ->
         maps:from_list( lists:zip(Head, Tuple) )
      end,
      Stream
   ).


%%
%% build an in-line stream filter using predicate term and pattern  
-spec filter(_, datalog:pattern()) -> _.

filter(With, Pattern) ->
   case filter_spec(Pattern) of
      undefined ->
         fun(_, Stream) -> Stream end;
      Filters ->
         filter_fold(With, Filters)
   end.

filter_spec('_') ->
   undefined;

filter_spec(Pattern)
 when is_list(Pattern) ->
   % the term is variable: p(..., X, ...), X > 5
   Pattern;

filter_spec(Pattern) ->
   % the term is in-line value
   [{'=:=', Pattern}].

filter_fold(With, Filters) ->
   fun(Lens, Stream) ->
      lists:foldl(fun(Filter, Acc) -> filter_with(With, Lens, Filter, Acc) end, Stream, Filters)
   end.

filter_with(With, Lens, Filter, Stream) ->
   With(fun(X) -> filter_check(Lens(X), Filter) end, Stream).

filter_check(B, {'>',   A}) -> B >  A;
filter_check(B, {'>=',  A}) -> B >= A;
filter_check(B, {'<',   A}) -> B  < A;
filter_check(B, {'=<',  A}) -> B =< A;
filter_check(B, {'=:=', A}) -> B =:= A;
filter_check(B, {'=/=', A}) -> B =/= A.

