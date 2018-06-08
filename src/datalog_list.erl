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
%%   a reference implementation of stream generators
-module(datalog_list).
-compile({parse_transform, category}).

-export([
   stream/2
]).

%%
%% a(x,y) :- .stream(...)
stream([Len|Gen], SubQ) ->
   fun(List) ->
      stream:zip([gen(Len, N, Filter) || {N, Filter} <- lists:zip(Gen, SubQ)])
   end.

gen(Len, N, Filter) ->
   [identity ||
      stream:build(N),
      stream:take(Len, _),
      filter(Filter, _)
   ].

filter(Filter, Stream) ->
   Fun  = datalog:filter(Filter),
   Fun(fun(X) -> X end, Stream).

