%% @doc
%%   horn clause evaluator
-module(datalog_h).
-include("datalog.hrl").

-export([
   stream/2
]).

%%
%% evaluate horn clause to stream
stream(#h{body = Body0}, Heap) ->
   Body1 = [X#p{t = datalog_t:rewrite(T, Heap)} || X = #p{t = T} <- Body0],
   stream(lists:reverse(Body1), Heap);

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
accept(#p{s = ?NULL}=Head, Tail0, Heap0) ->
   case eval(Tail0, Heap0) of
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

ingress(#p{ns = Ns, id = Id, t = Tx}=X, Heap) ->
   try
      % stream is not defined if any of ingress arguments is not defined (eq '=')
      X#p{s = erlang:apply(Ns, Id, datalog_t:input(Tx, Heap))}
   catch throw:undefined ->
      X
   end.

%%
%% update heap
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
