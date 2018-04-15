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
%%   the library parser uses leex / yeec to convert query (datalog syntax) to 
%%   abstract syntax tree (set of horn classes). The abstract syntax is not optimal
%%   for evaluation. It has to be adapted to `-type datalog:q()`.
-module(datalog_q).

-export([
   native/1,
   native_flat/1
]).

%%
%%
-spec native([{_, _, _}]) -> datalog:q().

native(Horns) ->
   lists:foldl(
      fun({Horn, Head, Body}, Datalog) ->
         % datalog complaint body consists of predicates and built-in filters.
         % predicate is converted to `-type pattern()`. The filters are interleaved 
         % into pattern so that sigma function can construct stream of ground facts.
         {Pattern, Filter} = lists:partition(fun(X) -> size(X) =:= 2 end, Body),
         maps:put(Horn, [Head | ast_to_body(Pattern, Filter)], Datalog);

      ({Horn, Head}, Datalog) ->
         maps:put('?', #{'@' => Horn, '_' => Head}, Datalog)
      end,
      #{},
      Horns 
   ).

%%
%%
-spec native_flat([{_, _, _}]) -> datalog:q().

native_flat(Horns) ->
   lists:map(
      fun({Horn, Head, Body}) ->
         {Pattern, Filter} = lists:partition(fun(X) -> size(X) =:= 2 end, Body),
         [Head | ast_to_body(Pattern, Filter)];
      ({Horn, Head}) ->
         #{'_' => Head}
      end,
      Horns
   ).

%%
%%
ast_to_body(Pattern, Filter) ->
   [ast_to_pattern(X, Filter) || X <- Pattern].

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

