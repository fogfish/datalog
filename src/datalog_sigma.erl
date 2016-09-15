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

-export([
   bind/2,
   filter/3
]).

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
-spec filter(_, _, datalog:pattern()) -> _.

filter(With, Term, Pattern) ->
   case filter_spec(Term, Pattern) of
      undefined ->
         fun(_, Stream) -> Stream end;
      Filters ->
         filter_fold(With, Filters)
   end.

filter_spec(Term, Pattern)
 when is_atom(Term) ->
   % the term is variable: p(..., X, ...), X > 5
   case Pattern of
      #{Term := Filter} when is_list(Filter) ->
         Filter;
      #{Term := Filter} ->
         [{'=:=', Filter}];
      _ ->
         undefined
   end;

filter_spec(Term, _Pattern) ->
   % the term is in-line value
   [{'=:=', Term}].

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

