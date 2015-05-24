%% @doc
%%   abstraction : predicate term
-module(datalog_t).
-include("datalog.hrl").

-export([
   compile/1
  ,prepare/1
]).

%%
%% compile query, replace predicate term with abstract syntax
compile(Query) ->
   erlang:element(1, 
      lists:mapfoldl(fun compile/2, dict:new(), Query)
   ).

compile({Id, Term0}, Acc0) ->
   {Term1, Acc1} = lists:mapfoldl(fun compile/2, Acc0, Term0),
   {{Id, Term1}, Acc1};   

compile(#h{head = Head0, body = Body0}=X, Acc0) ->
   {Body1, Acc1} = lists:mapfoldl(fun compile/2, Acc0, Body0),
   Head1 = [dict:fetch(H, Acc1) || H <- Head0],
   {X#h{head = Head1, body = Body1}, Acc1};

compile(#p{ns = filter, t = Term0}=X, Acc0) ->
   Term1 = [dict:fetch(T, Acc0) || T <- Term0],
   {X#p{t = Term1}, Acc0};

compile(#p{t = Term0}=X, Acc0) ->
   {Term1, Acc1} = lists:mapfoldl(fun compile/2, Acc0, Term0),
   {X#p{t = Term1}, Acc1};

compile(Id, Acc0)
 when is_atom(Id) ->
   % term is variable
   case dict:find(Id, Acc0) of
      {ok, I} ->
         {{I, '_'}, Acc0};
      error   ->
         I = dict:size(Acc0) + 1,
         {{I, '_'}, dict:store(Id, I, Acc0)}
   end;

compile(Id, Acc0) ->
   % term is literal
   {{'_', {'=', Id}}, Acc0}.

%%
%% prepare query, replace predicate term with evaluation order
prepare(Query) ->
   erlang:element(1, 
      lists:mapfoldl(fun prepare/2, [], Query)
   ).

prepare(#h{body = Body}=X, Acc0) ->
   {Pred, Cond} = lists:partition(fun(#p{ns = Ns}) -> Ns =/= filter end, Body),
   {Body1,   _} = lists:mapfoldl(fun prepare/2, [], Pred),
   {X#h{body = filter(Body1, Cond)}, Acc0};

prepare(#p{t = Term0}=X, Acc0) ->
   {Term1, Acc1} = lists:mapfoldl(fun prepare/2, Acc0, Term0),
   {X#p{t = Term1}, Acc1};   

prepare({'_', _}=T, Acc0) ->
   {T, Acc0};

prepare({I, '_'}, Acc0) ->
   case lists:member(I, Acc0) of
      true  ->
         {{I, in}, Acc0};
      false ->
         {{I, eg}, [I|Acc0]}
   end;
   
prepare(Any, Acc) ->
   {Any, Acc}.

%%
%% optimize query, inject filters to egress terms
filter({I, eg}, Cond) ->
   case lists:keyfind([I], #p.t, Cond) of
      false ->
         {I, eg};
      #p{id = Id, s = Value} ->
         {I, {Id, Value}}
   end;

filter({_, _}=Term, _Cond) ->
   Term;

filter(#p{t = Term}=X, Cond) ->
   X#p{t = [filter(T, Cond) || T <- Term]};

filter(Body, Cond) ->
   [filter(Pred, Cond) || Pred <- Body].




