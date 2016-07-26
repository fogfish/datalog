%%
%%   Copyright 2014 Dmitry Kolesnikov, All Rights Reserved
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
%%   horn clause evaluator
-module(datalog_horn).
-include("datalog.hrl").

-export([stream/2]).
-export([stream/3]).

%% scalable bloom filter config
-define(SBF, sbf:new(128, 0.0001)).

%%
%%
stream(Head, Horn) ->
   fun(Heap) ->
      fun(X) ->
         unique(
            ?SBF,
            stream:map(
               fun(Y) -> maps:with(Head, Y) end,
               stream1(X, Heap, lists:reverse(Horn))
            )
         )
      end
   end.

stream(Id, Head, Horn) ->
   fun(Heap) ->
      fun(X) ->
         unique(
            ?SBF,
            stream:map(
               fun(Y) -> (maps:with(Head, Y))#{'@type' => Id} end,
               stream1(X, Heap, lists:reverse(Horn))
            )
         )
      end
   end.


%%
%% remove duplicated elements
unique(Sbf0, {s, Head, _}=Stream) ->
   Sbf1 = sbf:add(Head, Sbf0),
   stream:new(Head, 
      fun() ->
         unique(Sbf1, stream:dropwhile(fun(X) -> sbf:has(X, Sbf1) end, Stream))
      end
   );
unique(_, {}) ->
   stream:new().

%%
%% apply horn clause to heap and context
stream1(X, Heap0, Horn0) ->
   try
      {Heap1, Horn1} = eval(X, Heap0, Horn0),
      stream:new(Heap1, fun() -> stream1(X, Heap1, Horn1) end)
   catch _:eos ->
      stream:new()
   end.

%%
%% evaluate horn clause head
eval(_, Heap, [{Stream0, Keys, Fun}]) ->
   case stream:tail(Stream0) of
      ?NULL   ->
         throw(eos);
      Stream1 ->
         {heap(Heap, Stream1), [{Stream1, Keys, Fun}]}
   end;

eval(X, Heap, [Fun]) ->
   Stream = f(X, Heap, Fun),
   Keys   = keys(Heap, Stream),
   {heap(Heap, Stream), [{Stream, Keys, Fun}]};

eval(X, Heap, [{Stream0, Keys, Fun} | Tail]) ->
   case stream:tail(Stream0) of
      ?NULL   ->
         %% withdraw keys
         eval(X, maps:without(Keys, Heap), [Fun | Tail]);
      Stream1 ->
         {heap(Heap, Stream1), [{Stream1, Keys, Fun} | Tail]}
   end;

eval(X, Heap0, [Fun | Tail0]) ->
   {Heap1, Tail1} = eval(X, Heap0, Tail0),
   case f(X, Heap1, Fun) of
      ?NULL  ->
         eval(X, Heap1, [Fun | Tail1]);
      Stream ->
         Keys   = keys(Heap1, Stream),
         {heap(Heap1, Stream), [{Stream, Keys, Fun} | Tail1]}
   end.

%%
%% update heap
heap(Heap, Stream) ->
   maps:merge(Heap, stream:head(Stream)).

%%
%% captures set of key defined by expression 
keys(Heap, Stream) ->
   maps:keys(
      maps:without(
         maps:keys(Heap), 
         stream:head(Stream)
      )
   ).

%%
%%
f(X, Heap, Fun) ->
   Fun1 = Fun(Heap),
   Fun1(X).


