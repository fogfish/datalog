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
%%   reference implementation
-module(datalog_list).

-export([
   like/2
  %  like/3
  % ,like/4
]).

like(Pattern, List) ->
   maps:fold(fun match/3, stream:build(List), Pattern).
 
match(Key, Filter, Stream)
 when is_list(Filter) ->
   lists:foldl(fun(F, Acc) -> filter(F, Key, Acc) end, Stream, Filter);

match(Key, Val, Stream) ->
   stream:filter(
      fun(X) -> 
         maps:get(Key, X) =:= Val 
      end, 
      Stream
   ).   
  
filter({F, Val}, Key, Stream) ->
   stream:filter(
      fun(X) -> 
         check(F, maps:get(Key, X), Val) 
      end, 
      Stream
   ).

check('>',  A, B) -> A >  B;
check('>=', A, B) -> A >= B;
check('<',  A, B) -> A  < B;
check('=<', A, B) -> A =< B.
