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
%%   datalog sigma helper
-module(datalog_sigma).

-include_lib("datum/include/datum.hrl").

-export([
   stream/1,
   bind/2,
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

unfold({Predicate, Env, Stream}) ->
   Head = stream:head(Stream),
   Tail = stream:tail(Stream),
   unfold({Predicate, Env, (build(maps:merge(Predicate, Head)))(Env), Head, Tail});

unfold({Predicate, Env, ?stream(), Head, Tail}) ->
   unfold({Predicate, Env, Tail});

unfold({Predicate, Env, Stream, Head, Tail}) ->
   {
      maps:merge(stream:head(Stream), Head),
      {Predicate, Env, stream:tail(Stream), Head, Tail}
   }.


%%
%% build a "tuple" stream using stream generator
build(#{'@' := Gen, '_' := Head = [A]} = Predicate) ->
   Gen(Head, term(A, Predicate));
build(#{'@' := Gen, '_' := Head = [A, B]} = Predicate) ->
   Gen(Head, term(A, Predicate), term(B, Predicate));
build(#{'@' := Gen, '_' := Head = [A, B, C]} = Predicate) ->
   Gen(Head, term(A, Predicate), term(B, Predicate), term(C, Predicate));
build(#{'@' := Gen, '_' := Head = [A, B, C, D]} = Predicate) ->
   Gen(Head, term(A, Predicate), term(B, Predicate), term(C, Predicate), term(D, Predicate)).

term(T, Predicate) ->
   case Predicate of
      #{T := Value} -> Value;
      _             -> '_'
   end.

%%
%% bind pattern with resolved variable from heap
-spec bind(map(), datalog:pattern()) -> datalog:pattern().

bind(Heap, Pattern) ->
   % maps:merge(...) provides out-of-implementation for the library
   % it merges two maps into a single map. if two keys (variables) exists in both maps 
   % the value in Pattern will be superseded by the value in Heap.
   % Thus any BIF are converted to pattern match
   maps:merge(Pattern, Heap).

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

