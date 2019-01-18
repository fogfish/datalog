%%
%%   Copyright 2014 - 2018 Dmitry Kolesnikov, All Rights Reserved
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
%%   the library parser uses leex / yeec to convert query (datalog syntax) to 
%%   abstract syntax tree (set of horn classes). The abstract syntax is not optimal
%%   for evaluation. It has to be adapted to `-type datalog:q()`.
-module(datalog_q).

-include("datalog.hrl").

-export([
   native/1
]).

%%
%%
-spec native([{_, _, _}]) -> datalog:q().

native(Datalog) ->
   join(lists:map(fun optimise/1, Datalog)).

%%
optimise(#horn{body = Body} = Horn) ->
   % horn clause body consists of predicates and infix predicates.
   % infix predicates are converted to stream range filters 
   {Pattern, Infix} = lists:partition(fun(X) -> size(X) =:= 2 end, Body),
   Horn#horn{body = ast_to_body(Pattern, Infix)};

optimise(Clause) ->
   Clause.

%%
join([#horn{id = Id, head = Head} | _] = Datalog) ->
   case 
      lists:partition(fun(#horn{id = X}) -> X =:= Id; (_) -> false end, Datalog)
   of
      {[Horn], Other} ->
         [Horn | join(Other)];
      {Joints, Other}  ->
         case is_recc(Id, Joints) of
            false ->
               [#join{id = Id, head = Head, horn = Joints} | join(Other)];
            true  ->
               [#recc{id = Id, head = Head, horn = Joints} | join(Other)]
         end
   end; 

join([Head | Tail]) ->
   [Head | join(Tail)];

join([]) ->
   [].

%%
is_recc(_, []) ->
   false;
is_recc(Horn, [#horn{body = Body} | T]) ->
   case lists:dropwhile(fun(#{'@' := X}) -> X /= Horn end, Body) of
      [] ->
         is_recc(Horn, T);
      _  ->
         true
   end.

%%
%%
ast_to_body(Pattern, Infix) ->
   [ast_to_pattern(X, Infix) || X <- Pattern].

%%
%%
ast_to_pattern({Id, Pattern}, Filter) ->
   %% check each pattern variable for BIF constrains 
   lists:foldl(
      fun(X, Acc) -> ast_to_filter(X, Filter, Acc) end, 
      #{'@' => Id, '_' => Pattern},
      Pattern
   ).
   
%%
%% interleave pattern variable with filters
ast_to_filter(Variable, Filter, Pattern) ->
   case 
      lists:filter(
         fun(X) -> 
            erlang:element(2, X) =:= Variable
         end, 
         Filter
      )
   of
      %% no BIF constrain
      [] -> 
         Pattern;

      %% BIF constrain variable
      Fs ->
         case lists:keyfind('=:=', 1, Fs) of
            false ->
               maps:put(Variable, [{P, F} || {P, _, F} <- Fs], Pattern);
            {'=:=', _, F} ->
               maps:put(Variable, F, Pattern)
         end
   end.


