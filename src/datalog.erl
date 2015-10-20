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
%%   datalog evaluator
%%
%% @todo 
%%   * known limitation - support single horn clause only 
-module(datalog).
-include("datalog.hrl").

-export([
   q/3
  ,p/1
  ,test/1
]).
-export([
   head/2,
   unit/3,
   list/1,
   horn/1,
   eval/2
   % sigma/2,
   % sigma/1
   % bind/3,
   % apply/2
]).
-export_type([datalog/0]).

head(Head, Stream) ->
   stream:map(
      fun(X) -> 
         maps:with(Head, X) 
      end,
      Stream
   ).

unit(Heap, Head, Stream) ->
   stream:map(
      fun(X) ->
         maps:merge(Heap, stream:head(Stream))
      end,
      Stream
   ).

horn(List) ->
   datalog_horn:stream(List).

%%
%%
eval(X, Expr) ->
   Fun = Expr(#{}),
   Fun(X).

%%%----------------------------------------------------------------------------
%%%
%%% primitive data types
%%%
%%%----------------------------------------------------------------------------

list(#{'_' := Head}) ->
   Len = length(Head),
   fun(Heap) ->
      %% how to exclude from binding input / output variables ?
      io:format("==> ~p  ~p~n", [Head, Heap]),
      %% @todo: binding
      fun(List) ->
         stream:map(
            fun(X) ->
               maps:from_list( lists:zip(Head, tuple_to_list(X)) )
            end,
            stream:filter(
               fun(X) -> 
                  is_tuple(X) andalso size(X) =:= Len
               end,
               stream:build(List)
            )
         )
      end
   end.






%%
%% data-types
-type(datalog() :: {atom(), bind(), [horn()]}).
-type(horn()    :: {atom(), bind(), [pred()]}).
-type(pred()    :: {atom(), bind()}).
-type(bind()    :: [any()]).

%%
%%
sigma(Head, Pattern) ->
   lists:foldl(
      fun({Key, Val}, Acc) ->
         maps:put(Key, Val, Acc)
      end,
      #{'_' => Head},
      Pattern
   ).

sigma(Head) ->
   sigma(Head, []).   


%% 
%% evaluate datalog
-spec(q/3 :: (datalog(), any(), any()) -> datum:stream()).

q(Datalog, #{'?' := Entry} = Goal, Funct) ->
   Heap  = maps:fold(
      fun
      (_,   '_', Acc) -> Acc ; 
      ('?',   _, Acc) -> Acc ; 
      (Key, Val, Acc) -> maps:put(Key, Val, Acc)
      end,
      #{},
      Goal
   ),
   datalog_h:stream(maps:get(Entry, Datalog), #{funct => Funct, heap => Heap}).


% q(Datalog, Input, Mod) ->
%    {_Head, Goal, Rules} = datalog_c:make(Datalog),
%    Heap0 = heap_init(heap_size(Rules)),
%    Heap1 = lists:foldl(fun heap_defs/2, Heap0, Goal),
%    datalog_h:stream(hd(Rules), #datalog{mod = Mod, state=Input, heap = Heap1}).

%%
%% parse datalog
-spec(p/1 :: (string()) -> datalog()).

p(Datalog) ->
   try
      {ok, Lex, _} = datalog_leex:string(Datalog), 
      {ok, Req}    = datalog_yeec:parse(Lex),
      datalog(Req)
   catch
   _:{badmatch, {error, {_, rds_parser, Reason}}} ->
      {error, Reason}; 
   _:{badmatch, {error, {_, rds_lexer,  Reason},_}} ->
      {error, Reason}; 
   _:{badmatch, Error} ->
      Error
   end. 

%%
%% helper function to kick unit test
test(Spec) ->
   ct:run_test([{spec, Spec}]).


%%%----------------------------------------------------------------------------
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------

%%
%% convert list of horn clauses to datalog map
datalog(Horns) ->
   lists:foldl(
      fun({Horn, Head, Body}, Acc) ->
         {Pattern, Filter} = lists:partition(fun(X) -> size(X) =:= 2 end, Body),
         maps:put(Horn, [Head | horn(Pattern, Filter)], Acc)
      end,
      #{},
      Horns 
   ).

horn(Pattern, Filter) ->
   [pattern(X, Filter) || X <- Pattern].

pattern({Id, Pattern}, Filter) ->
   {Id, Pattern, lists:foldl(fun(X, Acc) -> filter(X, Filter, Acc) end, #{}, Pattern)}.


filter(I, Filter, Acc) ->
   case 
      lists:filter(fun(X) -> erlang:element(2, X) =:= I end, Filter)
   of
      [] -> 
         Acc;
      Fs ->
         case lists:keyfind('=:=', 1, Fs) of
            false ->
               maps:put(I, [{P, F} || {P, _, F} <- Fs], Acc);
            {'=:=', _, F} ->
               maps:put(I, F, Acc)
         end
   end.

% %%
% %%
% heap_init(N) ->
%    erlang:make_tuple(N, '_').

% %%
% %%
% heap_defs('_', Heap) ->
%    Heap;
% heap_defs({I, X}, Heap) ->
%    erlang:setelement(I, Heap, X).

% %%
% %% resolve required heap size of datalog program
% heap_size(List)
%  when is_list(List) ->
%    lists:max([heap_size(X) || X <- List]);
% heap_size(#h{body = List}) ->
%    lists:max([heap_size(X) || X <- List]);
% heap_size(#p{t = List}) ->
%    lists:max([heap_size(X) || X <- List]);
% heap_size(X)
%  when is_tuple(X) ->
%    lists:max([heap_size(Y) || Y <- erlang:tuple_to_list(X)]);
% heap_size(X)
%  when is_integer(X) ->
%    X;
% heap_size(_) ->
%    0.

