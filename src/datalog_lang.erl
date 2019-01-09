%%
%%   Copyright 2014 - 2016 Dmitry Kolesnikov, All Rights Reserved
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
%%   build-in datalog primitives
-module(datalog_lang).

-include_lib("datum/include/datum.hrl").

-export([
   filter/2,
   unique/1,
   flat/1,
   eq/1, ne/1, lt/1, gt/1, le/1, ge/1
]).

%%
%% scalable bloom filter definition
-define(SBF, sbf:new(128, 0.0001)).

%%
%% in-line stream filter(s),
%% helper function to apply predicate terms and patterns on stream of tuples 
-spec filter(_, datalog:pattern()) -> _.

filter(_With, '_') ->
   fun(_Lens, Stream) -> Stream end;

filter(_With, undefined) ->
   fun(_Lens, Stream) -> Stream end;

filter(With, Pattern)
 when is_list(Pattern) ->
   % pattern is guard condition: p(..., X, ...), X > 5, X < 10
   fun(Lens, Stream) ->
      lists:foldl(fun(Filter, Acc) -> filter(With, Lens, Filter, Acc) end, Stream, Pattern)
   end;

filter(With, Pattern) ->
   filter(With, [{'=:=', Pattern}]).


filter(With, Lens, Filter, Stream) ->
   With(fun(X) -> filter_check(Lens(X), Filter) end, Stream).

filter_check(B, {'>',   A}) -> B >  A;
filter_check(B, {'>=',  A}) -> B >= A;
filter_check(B, {'<',   A}) -> B  < A;
filter_check(B, {'=<',  A}) -> B =< A;
filter_check(B, {'=:=', A}) -> B =:= A;
filter_check(B, {'=/=', A}) -> B =/= A.


%%
%% a predicate ensures unique terms within the stream
%% ```
%% h(x,z) :- a(x,y), .unique(y), b(y,z) . 
%% ``` 
-spec unique(datalog:predicate()) -> _.

unique(#{'_' := Head}) ->   
   fun(_) ->
      fun(Stream) ->
         stream:unfold(fun uniq/1, {?SBF, Head, Stream})
      end
   end.

uniq({_, _, ?stream()}) ->
   stream:new();  

uniq({Sbf0, Head, Stream}) ->
   Item = maps:with(Head, stream:head(Stream)),
   Sbf1 = sbf:add(Item, Sbf0),
   Tail = stream:dropwhile(fun(X) -> sbf:has(maps:with(Head, X), Sbf1) end, Stream),
   {stream:head(Stream), {Sbf1, Head, Tail}}.

%%
%% a predicate flatmap identity over stream 
%% ```
%% h(x,z) :- a(x,y), .flat(y), b(y,z) . 
%% ``` 
-spec flat(datalog:predicate()) -> _.

flat(#{'_' := [Term]}) ->
   fun(_) ->
      fun(Stream) ->
         stream:unfold(fun flatten/1, {[], Term, Stream})
      end
   end.

flatten({_, _, ?stream()}) ->
   stream:new();

flatten({[], Term, Stream}) ->
   case stream:head(Stream) of
      #{Term := X} when is_list(X) ->
         flatten({X, Term, Stream});
      Head ->
         {Head, {[], Term, stream:tail(Stream)}}
   end;

flatten({[H], Term, Stream}) ->
   Head = stream:head(Stream),
   {Head#{Term => H}, {[], Term, stream:tail(Stream)}};

flatten({[H|T], Term, Stream}) ->
   Head = stream:head(Stream),
   {Head#{Term => H}, {T, Term, Stream}}.

%%
%% comparison predicates
%% ```
%% h(x,z) :- a(x,y), b(y,z), .eq(x,z). 
%% ``` 
-spec eq(datalog:predicate()) -> _.

eq(#{'_' := [A, B]}) ->
   fun(_, _) ->
      fun(Stream) ->
         stream:filter(fun(#{A := Ax, B := Bx}) -> Ax =:= Bx end, Stream)
      end
   end.

-spec ne(datalog:predicate()) -> _.

ne(#{'_' := [A, B]}) ->
   fun(_, _) ->
      fun(Stream) ->
         stream:filter(fun(#{A := Ax, B := Bx}) -> Ax =/= Bx end, Stream)
      end
   end.

-spec lt(datalog:predicate()) -> _.

lt(#{'_' := [A, B]}) ->
   fun(_) ->
      fun(Stream) ->
         stream:filter(fun(#{A := Ax, B := Bx}) -> Ax < Bx end, Stream)
      end
   end.

-spec gt(datalog:predicate()) -> _.

gt(#{'_' := [A, B]}) ->
   fun(_) ->
      fun(Stream) ->
         stream:filter(fun(#{A := Ax, B := Bx}) -> Ax > Bx end, Stream)
      end
   end.

-spec le(datalog:predicate()) -> _.

le(#{'_' := [A, B]}) ->
   fun(_) ->
      fun(Stream) ->
         stream:filter(fun(#{A := Ax, B := Bx}) -> Ax =< Bx end, Stream)
      end
   end.

-spec ge(datalog:predicate()) -> _.

ge(#{'_' := [A, B]}) ->
   fun(_) ->
      fun(Stream) ->
         stream:filter(fun(#{A := Ax, B := Bx}) -> Ax >= Bx end, Stream)
      end
   end.




