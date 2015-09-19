%% @doc
%%   horn clause evaluator
-module(datalog_h).
-include("datalog.hrl").

-export([
   stream/2
]).

%%
%% evaluate horn clause to stream
stream(#h{body = Body0}, Datalog) ->
   Body1 = [X#p{t = datalog_t:rewrite(T, Datalog)} || X = #p{t = T} <- Body0],
   stream(lists:reverse(Body1), Datalog);

stream(Body0, Datalog0) ->
   case eval(Body0, Datalog0) of
      {eof,       _} ->
         stream:new();
      {#datalog{heap = Heap} = Datalog1, Body1} ->
         stream:new(Heap, fun() -> stream(Body1, Datalog1) end)
   end.

%%
%% evaluate 
eval([Head | Tail], Datalog) ->
   accept(ingress(Head, Datalog), Tail, Datalog);

eval([], _Datalog) ->
   {eof, []}.

%%
%%
accept(#p{s = ?NULL}=Head, Tail0, Datalog0) ->
   case eval(Tail0, Datalog0) of
      {eof,   Tail1} ->
         {eof, [Head | Tail1]};
      {Datalog1, Tail1} ->
         accept(ingress(Head, Datalog1), Tail1, Datalog1)
   end;

accept(#p{s = Stream, t = Vx}=Head, Tail, #datalog{heap=Heap0}=Datalog) ->
   Heap1 = heap(Vx, stream:head(Stream), Heap0),
   {Datalog#datalog{heap =Heap1}, [Head | Tail]}.


%%
%% evaluate predicate stream 
ingress(#p{s = {s, _, _} = Stream}=X, _Heap) ->
   X#p{s = stream:tail(Stream)};

ingress(#p{id = Id, t = Tx}=X, #datalog{mod = Mod, state = State} = Heap) ->
   try
      % stream is not defined if any of ingress arguments is not defined (eq '=')
      % @todo: get module from Heap
      X#p{s = erlang:apply(Mod, Id, [State | datalog_t:input(Tx, Heap)])}
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
