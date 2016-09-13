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
%%   build-in datalog primitives
-module(datalog_lang).

-export([
   unique/1,
   put/1
]).

%%
%% scalable bloom filter definition
-define(SBF, sbf:new(128, 0.0001)).


%%
%% a predicate ensures unique terms within the stream
-spec unique(datalog:predicate()) -> _.

unique(#{'_' := Head}) ->   
   fun(_) ->
      fun(Stream) ->
         [pipe|stream:unfold(fun uniq/1, {?SBF, Head, Stream})]
      end
   end.

uniq({_, _, {}}) ->
   stream:new();  

uniq({Sbf0, Head, Stream}) ->
   Item = maps:with(Head, stream:head(Stream)),
   Sbf1 = sbf:add(Item, Sbf0),
   Tail = stream:dropwhile(fun(X) -> sbf:has(maps:with(Head, X), Sbf1) end, Stream),
   {stream:head(Stream), {Sbf1, Head, Tail}}.


%%
%% a predicate assign value to stream
-spec put(datalog:predicate()) -> _.

put(#{'_' := [X,Y]}) ->
   fun(_) ->
      fun(Stream) ->
         [pipe|stream:map(fun(Head) -> assign(X, Y, Head) end, Stream)]
      end
   end.

assign(X, Y, Head)
 when not is_atom(Y) ->
   Head#{X => Y};

assign(X, Y, Head) ->
   Head#{X => maps:get(Y, Head)}.

