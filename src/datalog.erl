%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
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
%%   datalog evaluator
-module(datalog).
-include("datalog.hrl").

-export([
   new/1
  ,q/2
]).

-type(bind()    :: [integer()]).
-type(pred()    :: {atom(), bind()}).
-type(rule()    :: {atom(), bind(), [pred()]}).
-type(datalog() :: {atom(), bind(), [rule()]}).


%%
%%
new(Query) ->
   #datalog{q = Query}.

%%
%%
q({Head, Goal, Rules}, #datalog{}=State) ->
   {_, Bind, _} = lists:keyfind(Head, 1, Rules),
   Heap0 = heap_init(heap_size(Rules)),
   Heap1 = lists:foldl(fun heap_defs/2, Heap0, lists:zip(Goal, Bind)),
   eval(Rules, State#datalog{heap = Heap1});

q(Datalog, Query)
 when is_atom(Query) ->
   q(Datalog, new(Query)).

   

%%%----------------------------------------------------------------------------
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------

%%
%% evaluate predicate
eval({Uid, Bind}, #datalog{q = Q, heap = Heap}) ->
   erlang:apply(Q, Uid, bind(Bind, Heap));

eval({Uid, _, Pred}, State) ->
   eval(hd(Pred), State);

eval([Rule], State) ->
   eval(Rule, State).

%%
%% bind head intent status to predicate variables
bind(Bind, Head) ->
   [erlang:element(X, Head) || X <- Bind].

%%
%% resolve required heap size of datalog program
heap_size({_, Bind, List}) ->
   erlang:max(heap_size(Bind), heap_size(List));
heap_size({_, Bind}) ->
   heap_size(Bind);
heap_size(X)
 when is_integer(X) ->
   X;
heap_size(List)
 when is_list(List) ->
   lists:max([heap_size(X) || X <- List]);
heap_size(_) ->
   0.

%%
%%
heap_init(N) ->
   erlang:make_tuple(N, '_').

%%
%%
heap_defs({A, _}, Heap)
 when is_integer(A) ->
   Heap;
heap_defs({A, B}, Heap) ->
   erlang:setelement(B, Heap, A).


% -export([behaviour_info/1]).

% %%%----------------------------------------------------------------------------
% %%%
% %%% datalog query interface
% %%%
% %%%----------------------------------------------------------------------------

% %%
% %%
% behaviour_info(callbacks) ->
%    [
%       %%
%       %% xxx
%       %%
%       %% -spec(xxx/1 :: (xxx(), xxx()) -> datum:stream()).
%       % {q,   1}
%    ];
% behaviour_info(_) ->
%    undefined.

