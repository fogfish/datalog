%% @doc
%%   datalog compiler
%%   
%%   It replaces variables with integer id. The variable is replaced with 
%%   positive int if predicate egress variable or with negative int if variable 
%%   is consumed by predicates e.g.
%%   c(X, Z) :- a(X, Y), b(Y, Z), d(Y, F)
%%     1  3      +1 +2    -2 +3    -2 +4
-module(datalog_c).
-include("datalog.hrl").

-export([
   make/2
]).

%%
%% compile datalog query
make(Ns, Query) ->
   % filter( assign( variable( compile(Query, Ns) ) ) ).
   % assign( variable( compile(Query, Ns) ) ).
   datalog_t:prepare(
      datalog_t:compile(
         compile(Query, Ns)
      )
   ).

%%
%% compile datalog to abstract structures
compile({Id, Term}, Ns) -> 
   #p{ns = Ns, id = Id, t = Term};

%% compile built-in filter
compile({'>', Id, Term}, _Ns) ->
   #p{ns = filter, id = '>', t = [Id], s = Term};

%% compile horn clause
compile({Id, Head, Body}, Ns) ->
   #h{id = Id, head = Head, body = [compile(X, Ns) || X <- Body]};

compile(Query, Ns) ->
   [compile(X, Ns) || X <- Query].


%%
%% build assignment plan for variable rules
% assign(Query) ->
%    erlang:element(1, 
%       lists:mapfoldl(fun assign/2, [], Query)
%    ).

% assign(#h{body = Body0}=X, Acc0) ->
%    {Body1, _} = lists:mapfoldl(fun assign/2, [], Body0),
%    {X#h{body = Body1}, Acc0};

% assign(#p{t = T0}=X, Acc0) ->
%    {T1, Acc1} = lists:mapfoldl(fun assign/2, Acc0, T0),
%    {X#p{t = T1}, Acc1};   

% assign({I, '_'}, Acc0) 


% assign(Id, Acc0)
%  when is_integer(Id) ->
%    case lists:member(Id, Acc0) of
%       true  ->
%          {0 - Id, Acc0};
%       false ->
%          {Id, [Id|Acc0]}
%    end;

% assign(Id, Acc0) ->
%    {Id, Acc0}.

%%
%% reduce filters to predicate query
filter(Query) ->
   erlang:element(1, 
      lists:mapfoldl(fun filter/2, [], Query)
   ).

% filter(#h{body = Body}=X, Acc0) ->
%    {Pred, Cond} = lists:partition(fun(#f{}) -> false; (_) -> true end, Body),
%    % P = lists:map(
%    %    fun(#p{vx = Vx}=Px) ->
%    %       Lx = 
%    %    end,
%    %    Pred
%    % ),
%    {X#h{body = Pred}, Acc0};

filter(Any, Acc) ->
   {Any, Acc}.

   

