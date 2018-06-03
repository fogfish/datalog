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
-include_lib("datum/include/datum.hrl").

-export([stream/2]).

%%
%%
stream(Head, Horn) ->
   fun(Env) ->
      Rule = [Fun(Env) || Fun <- Horn],
      fun(Stream) ->
         head(Head, join(Stream, Rule))
      end
   end.

join(?stream(), _) ->
   stream:new();
join(Stream, [Predicate | Horn]) ->
   join(Predicate(Stream), Horn);
join(Stream, []) ->
   Stream.

%%
%% applies head projection to output stream
head(Head, Stream) ->
   stream:map(fun(X) -> maps:with(Head, X) end, Stream).

