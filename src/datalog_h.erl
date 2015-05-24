%% @doc
%%   horn clause evaluator
-module(datalog_h).
-include("datalog.hrl").

-export([
   stream/2
]).

%%
%% evaluate horn clause to stream
stream(#h{body = [Head | Tail]}, Heap) ->
   stream(lists:reverse([protect(Head, Heap) | Tail]), Heap);

stream(Body0, Heap0) ->
   case eval(Body0, Heap0) of
      {eof,       _} ->
         stream:new();
      {Heap1, Body1} ->
         stream:new(Heap1, fun() -> stream(Body1, Heap1) end)
   end.

%%
%% evaluate 
eval([Head | Tail], Heap) ->
   accept(ingress(Head, Heap), Tail, Heap);

eval([], _Heap) ->
   {eof, []}.

%%
%%
accept(#p{s = ?NULL, t=Vx}=Head, Tail0, Heap0) ->
   case eval(Tail0, reset(Vx, Heap0)) of
      {eof,   Tail1} ->
         {eof, [Head | Tail1]};
      {Heap1, Tail1} ->
         accept(ingress(Head, Heap1), Tail1, Heap1)
   end;

accept(#p{s = Stream, t = Vx}=Head, Tail, Heap) ->
   {heap(Vx, stream:head(Stream), Heap), [Head | Tail]}.


%%
%% evaluate predicate stream 
ingress(#p{s = {s, _, _} = Stream}=X, _Heap) ->
   X#p{s = stream:tail(Stream)};

ingress(#p{ns = Ns, id = Id, t = Vx}=X, Heap) ->
   %% stream is not defined if any of (-) arguments is '_'
   try
      X#p{s = erlang:apply(Ns, Id, in(Vx, Heap))}
   catch throw:undefined ->
      X
   end.

in([{H,in}|T], Heap) ->
   case erlang:element(H, Heap) of
      '_' ->
         throw(undefined);
      V   ->
         [V | in(T, Heap)]
   end; 

in([{_,eg}|T], Heap) ->
   ['_' | in(T, Heap)];

in([{_,F}|T], Heap) ->
   [F | in(T, Heap)];

in([], _Heap) ->
   [].


%%
%%
vx({X, _}, Heap) ->
   erlang:element(X, Heap);

vx(X, Heap) ->
   erlang:element(X, Heap).

vx({X, _}, Val, Heap) ->
   erlang:setelement(X, Heap, Val).

heap(Vx, Head, Heap) ->
   erlang:element(2,
      lists:foldl(
         fun(X, {I, Acc}) ->
            {I + 1, vx(X, vx(I, Head), Acc)}
         end,
         {1, Heap},
         Vx 
      )
   ). 

reset(Vx, Heap) ->
   lists:foldl(
      fun(X, Acc) when X < 0 -> erlang:setelement(-X, Acc, '_'); (_, Acc) -> Acc end,
      Heap,
      Vx
   ).


%%
%%
protect(#p{t = Vx}=X, Heap) ->
   X#p{t = [ case erlang:element(I, Heap) of '_' -> T ; _ -> {I, in} end || {I, _} = T <- Vx ]}.

