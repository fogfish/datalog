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
%%   evaluates a logical program
-module(datalog_vm).

-compile({parse_transform, partial}).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").

-export([
   union/2,
   horn/2,
   stream/1
]).

%%
%%
union(A, B) ->
   fun(Env, Lp) ->
      HornA = A(Env, Lp),
      HornB = B(Env, Lp),
      fun(SubQ) ->
         stream:'++'(HornA(SubQ), HornB(SubQ))
      end
   end.


%%
%%
horn(Head, Horn) ->
   io:format("=[ horn ]=> ~p~n", [Horn]),
   fun(Env, Lp) ->
      [HHorn | THorn] = [Spec#{'@' => env_call(F, Lp, Env)} || #{'@' := F} = Spec <- Horn],
      fun(SubQ) ->
         Heap   = maps:from_list(lists:zip(Head, SubQ)),
         Stream = join(eval(Heap, HHorn), THorn),
         stream:map(
            fun(X) ->
               [maps:get(K, X) || K <- Head]
            end,
            Stream
         )
      end
   end.

env_call(F, Lp, Env) when is_atom(F) ->
   Fun = maps:get(F, Lp),
   fun(SubQ) ->
      ( Fun(Env, Lp) )(SubQ)
   end;
env_call(F, Lp, Env) ->
   F(Env, Lp).

join(Stream, [#{'.' := pipe, '@' := Pipe} | THorn]) ->
   join(Pipe(Stream), THorn);

join(Stream, [HHorn | THorn]) ->
   join(
      stream:flat(
         stream:map(fun(Heap) -> eval(Heap, HHorn) end, Stream)
      ),
      THorn
   );

join(Stream, []) ->
   Stream.

eval(Heap, #{'_' := Head, '@' := Fun} = Spec) ->
   SubQ = [term(T, Spec, Heap) || T <- Head],
   stream:map(
      fun(Tuple) ->
         %% Note: we need to give a priority to existed heap values, unless '_'
         %% maps:merge(Heap, maps:from_list( lists:zip(Head, Tuple) ))
         Prev = maps:filter(fun(_, X) -> X /= '_' end, Heap),
         This = maps:from_list( lists:zip(Head, Tuple) ),
         maps:merge(Heap, maps:merge(This, Prev))
      end,
      Fun(SubQ)
   ).

%%
%%
term(T, Spec, Heap) ->
   [undefined || term(T, Spec), term(T, Heap)].

term(T, Predicate)
 when is_atom(T) ->
   case Predicate of
      #{T := Value} -> Value;
      _             -> undefined
   end;
term(T, _) ->
   T.

%%
%% evaluate stream 
stream(#{'.' := Keys, '>' := Spec, '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun(SubQ) -> (Gen(Keys, SubQ, Spec))(Env) end
   end;

stream(#{'.' := Keys, '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun(SubQ) -> (Gen(Keys, SubQ))(Env) end
   end;

stream(#{'_' := [_], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1]) -> (Gen(X1))(Env) end
   end;

stream(#{'_' := [_, _], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2]) -> (Gen(X1, X2))(Env) end
   end;

stream(#{'_' := [_, _, _],'@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2, X3]) -> (Gen(X1, X2, X3))(Env) end
   end;

stream(#{'_' := [_, _, _, _], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2, X3, X4]) -> (Gen(X1, X2, X3, X4))(Env) end
   end;

stream(#{'_' := [_, _, _, _, _], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2, X3, X4, X5]) -> (Gen(X1, X2, X3, X4, X5))(Env) end
   end.

