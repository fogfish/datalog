%% @doc
%%   evaluates a logical program
-module(datalog_vm).

-compile({parse_transform, partial}).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").

-export([
   horn/2,
   stream/1
]).


%%
%%
horn(Head, Horn) ->
   fun(Env) ->
      [HHorn | THorn] = [Spec#{'@' => F(Env)} || #{'@' := F} = Spec <- Horn],
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
         maps:merge(Heap, maps:from_list( lists:zip(Head, Tuple) ))
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
   fun(Env) ->
      fun(SubQ) -> (Gen(Keys, SubQ, Spec))(Env) end
   end;

stream(#{'.' := Keys, '@' := Gen}) ->
   fun(Env) ->
      fun(SubQ) -> (Gen(Keys, SubQ))(Env) end
   end;

stream(#{'_' := [_], '@' := Gen}) ->
   fun(Env) ->
      fun([X1]) -> (Gen(X1))(Env) end
   end;

stream(#{'_' := [_, _], '@' := Gen}) ->
   fun(Env) ->
      fun([X1, X2]) -> (Gen(X1, X2))(Env) end
   end;

stream(#{'_' := [_, _, _],'@' := Gen}) ->
   fun(Env) ->
      fun([X1, X2, X3]) -> (Gen(X1, X2, X3))(Env) end
   end;

stream(#{'_' := [_, _, _, _], '@' := Gen}) ->
   fun(Env) ->
      fun([X1, X2, X3, X4]) -> (Gen(X1, X2, X3, X4))(Env) end
   end;

stream(#{'_' := [_, _, _, _, _], '@' := Gen}) ->
   fun(Env) ->
      fun([X1, X2, X3, X4, X5]) -> (Gen(X1, X2, X3, X4, X5))(Env) end
   end.

