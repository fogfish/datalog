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
flatmap(Fun, Stream) ->
   stream:unfold(fun flatmap/1, {Fun, Stream}).

flatmap({_, ?stream()}) ->
   stream:new();

flatmap({Fun, Stream}) ->
   flatmap({Fun(stream:head(Stream)), Fun, stream:tail(Stream)});

flatmap({?stream(), Fun, Stream}) ->
   flatmap({Fun, Stream});

flatmap({SubStream, Fun, Stream}) ->
   {stream:head(SubStream), {stream:tail(SubStream), Fun, Stream}}.


%%
%%
horn(Head, Horn) ->
   fun(Env) ->
      [HHorn | THorn] = [Spec#{'@' => F(Env)} || #{'@' := F} = Spec <- Horn],
      fun(SubQ) ->
         Heap = maps:from_list(lists:zip(Head, SubQ)),
         % io:format("=> heap => ~p~n", [Heap]),
         Stream = join(eval(Heap, HHorn), THorn),
         stream:map(
            fun(X) ->
               [maps:get(K, X) || K <- Head]
            end,
            Stream
         )
      end
   end.

join(Stream, [#{'_' := Head} = HHorn | THorn]) ->
   flatmap(
      fun(Heap) ->
         % stream:map(maps:merge(_, Heap), eval(Heap, HHorn))
         % stream:map(
         %    fun(X) ->
         %       io:format("-[ merge ] ->~p~n", [Heap]),
         %       io:format("-[ with  ] ->~p~n", [X]),
         %       maps:merge(X, Heap)
         %    end,
         %    eval(Heap, HHorn)
         % )
         eval(Heap, HHorn)
      end,
      Stream
   );

join(Stream, []) ->
   [].

eval(Heap, #{'_' := Head, '@' := Fun} = Spec) ->
   % io:format("-> heap -> ~p~n", [Heap]),
   SubQ = [term(T, Spec, Heap) || T <- Head],
   io:format("-> subq -> ~p~n", [SubQ]),
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
stream(#{'_' := Head, '.' := Keys, '@' := Gen}) ->
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

stream(#{'_' := [_, _, _],'@' :=  Gen}) ->
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






% stream(Head, Spec) ->
%    fun(SubQ) ->
%       io:format("-[ stream ]-> ~p~n", [SubQ]),
%       % io:format("-[ stream ]-> ~p ~p~n", [Spec, Heap]),
%       Stream = stream:build([[1, 10], [2, 20], [3, 30]])
%       % stream:map(
%       %    fun(Tuple) ->
%       %       maps:from_list( lists:zip(Head, Tuple) )
%       %    end,
%       %    Stream
%       % )
%    end.


%    % fun(Env) ->
%    %    % Rule = [Fun(Env) || Fun <- Horn],
%    %    % fun(Stream) ->
%    %    %    hhead(Head, join(Stream, Rule))
%    %    % end
%    % end.

% join(?stream(), _) ->
%    stream:new();
% join(Stream, [Predicate | Horn]) ->
%    join(Predicate(Stream), Horn);
% join(Stream, []) ->
%    Stream.

% %%
% %% applies head projection to output stream
% hhead(Head, Stream) ->
%    stream:map(fun(X) -> maps:with(Head, X) end, Stream).


% %%
% %% ~ sigma definition
% call(Predicate) ->
%    fun(Env) ->
%       fun(Stream) ->
%          stream:unfold(fun unfold/1, {Stream, Predicate, Env})
%       end
%    end.

% %%
% %% 
% unfold({?stream(), _, _}) ->
%    stream:new();

% unfold({Stream, Fun, Env}) ->
%    Heap = stream:head(Stream),
%    Tail = stream:tail(Stream),
%    SubQ = (Fun(term(X1, Predicate, Heap), term(X2, Predicate, Heap)))(Env),
%    unfold({Heap, Tail, head(Head, SubQ), Predicate, Env});

% unfold({_, Tail, ?stream(), Predicate, Env}) ->
%    unfold({Tail, Predicate, Env});

% unfold({Heap, Tail, Stream, Predicate, Env}) ->
%    {
%       maps:merge(stream:head(Stream), Heap),
%       {Heap, Tail, stream:tail(Stream), Predicate, Env}
%    }.


% %%
% %%
% head(Head, Stream) ->
%    stream:map(
%       fun(Tuple) ->
%          maps:from_list( lists:zip(Head, Tuple) )
%       end,
%       Stream
%    ).

