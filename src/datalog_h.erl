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
%%   horn clause evaluator
-module(datalog_h).
-include("datalog.hrl").

-export([
   stream/2
]).

%%
%% evaluate horn clause to stream
stream([_ | Body], Heap) ->
   eval(lists:reverse(stack(Body)), Heap).

stack(Body) ->
   [{Fun, Pat, Filter, stream:new()} || {Fun, Pat, Filter} <- Body].

eval(Stack0, Heap0) ->
   case run(Stack0, Heap0) of
      {eof,      _} ->
         stream:new();
      {#{heap := H} = Heap1, Stack1} ->
         stream:new(H, fun() -> eval(Stack1, Heap1) end)
   end.

%%
%% evaluate 
run([Head | Tail], Heap) ->
   accept(ingress(Head, Heap), Tail, Heap);

run([], _Heap) ->
   {eof, []}.

%%
%%
accept({_, _, _, ?NULL}=Head, Tail0, Heap0) ->
   case run(Tail0, Heap0) of
      {eof,   Tail1} ->
         {eof, [Head | Tail1]};
      {Heap1, Tail1} ->
         accept(ingress(Head, Heap1), Tail1, Heap1)
   end;

accept({_, _, _, Stream}=Head, Tail, #{heap := H} = Heap) ->
   {Heap#{heap => maps:merge(H, stream:head(Stream))}, [Head | Tail]}.

%%
%% evaluate predicate stream (ingress data stream to heap)
ingress({Fun, Pat, Filter, {s, _, _} = Stream}, _Heap) ->
   {Fun, Pat, Filter, stream:tail(Stream)};

ingress({Fun, Pat, Filter, _} = E, #{funct := {Mod, State}, heap := Heap}) ->
   try
      %% input is current heap value, each expected variable is bound with its value 
      %% and extended with possible filters
      Stream = Mod:Fun(maps:merge(Filter, maps:with(Pat, Heap)), State),
      {Fun, Pat, Filter, Stream}
   catch throw:undefined ->
      E
   end.
