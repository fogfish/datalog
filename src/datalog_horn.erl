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
-include_lib("datum/include/datum.hrl").

-export([stream/2]).

%%
%%
stream([Head | Horn0], Program) ->
   Horn1 = [maps:put('@', maps:get(Gen, Program), Predicate) || #{'@' := Gen} = Predicate <- Horn0],
   fun(Env) ->
      Horn2 = [(datalog_sigma:stream(Predicate))(Env) || Predicate <- Horn1],
      fun(Stream) ->
         head(Head, join(Stream, Horn2))
      end
   end.

join(?stream(), _) ->
   stream:new();
join(Stream, [Predicate | Horn]) ->
   join(Predicate(Stream), Horn);
join(Stream, []) ->
   Stream.


%%
%% compose sigma evaluator to stream 
compose(_, ?stream()) ->
   stream:new();
compose(Fun, Stream) ->
   compose(Fun(Stream), Fun, Stream).

compose([pipe|Egress], _, _) ->
   % evaluator function is stream modifier, it produce a new egress stream to output
   Egress;

compose(_, _, ?stream()) ->
   stream:new();

compose(Egress, Fun, Stream) ->
   % evaluator function produce an egress stream to join with ingress stream
   stream:unfold(fun unfold/1, {Egress, Fun, stream:head(Stream), stream:tail(Stream)}).

%%
%% 
unfold({?stream(), _, _, ?stream()}) ->
   stream:new();

unfold({?stream(), Fun, _, Stream}) ->
   unfold({Fun(Stream), Fun, stream:head(Stream), stream:tail(Stream)});

unfold({#stream{} = Egress, Fun, Head, Stream}) ->
   {
      maps:merge(stream:head(Egress), Head), 
      {stream:tail(Egress), Fun, Head, Stream}
   }.

%%
%% applies head projection to output stream
head(Head, Stream) ->
   stream:map(fun(X) -> maps:with(Head, X) end, Stream).

