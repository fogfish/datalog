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
   filter/2,
   takewhile/2
]).

%%
%% bind sigma pattern with resolved variable (resolved previously by sigma)  
-spec bind(map(), datalog:pattern()) -> datalog:pattern().

bind(Heap, Pattern) ->
   % maps:merge(...) provides out-of-implementation for the library
   % it merges two maps into a single map. if two keys (variables) exists in both maps 
   % the value in Pattern will be superseded by the value in Heap.
   % Thus any BIF are converted to pattern match
   maps:merge(Pattern, Heap).


%%
%% build a in-line stream filter(s)
-spec filter(_, datalog:pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).
-spec takewhile(_, datalog:pattern()) -> fun( (_, datum:stream()) -> datum:stream() ).

filter(X, Pattern) ->
   streamwith(fun stream:filter/2, X, Pattern).

takewhile(X, Pattern) ->
   streamwith(fun stream:takewhile/2, X, Pattern).

%%
streamwith(Fun, X, Pattern)
 when is_atom(X) ->
   %% guard term p(..., X, ...), X > 5
   case Pattern of
      #{X := Filter} when is_list(Filter) ->
         streamwith(Fun, Filter);
      #{X := Value} ->
         streamwith(Fun, [{'=:=', Value}]);
      _ ->
         fun(_, Stream) -> Stream end
   end;

streamwith(Fun, Value, _) ->
   %% in-line term p(..., 5, ...)
   streamwith(Fun, [{'=:=', Value}]).

streamwith(Fun, Filters) ->
   fun(Lens, Stream) ->
      lists:foldl(fun(Filter, Acc) -> filter(Fun, Filter, Lens, Acc) end, Stream, Filters)
   end.

filter(Fun, {F, Val}, Lens, Stream) ->
   Fun(fun(X) -> check(F, Lens(X), Val) end, Stream).

check('>',   A, B) -> A >  B;
check('>=',  A, B) -> A >= B;
check('<',   A, B) -> A  < B;
check('=<',  A, B) -> A =< B;
check('=:=', A, B) -> A =:= B.
