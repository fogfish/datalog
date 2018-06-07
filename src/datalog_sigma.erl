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
   filter/2
]).


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

filter_spec(undefined) ->
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

