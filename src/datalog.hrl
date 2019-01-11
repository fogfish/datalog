%% @doc
%%   abstract syntax tree 

%%
%% Erlang Native Format



-type id()     :: atom() | {atom(), atom()} | {iri, binary(), binary()}.
-type head()   :: [_].
-type body()   :: [#{}].

-record(goal, {
   id   = undefined        :: id()
,  head = undefined        :: head()
}).

-record(horn, {
   id   = undefined        :: id()
,  head = undefined        :: head()
,  body = undefined        :: body()
}).

-record(join, {
   id   = undefined        :: id()
,  head = undefined        :: head()
,  horn = undefined        :: [#horn{}]
}).

-record(recc, {
   id   = undefined        :: id()
,  head = undefined        :: head()
,  horn = undefined        :: [#horn{}]
}).

-record(source, {
   id   = undefined        :: id()
,  head = undefined        :: head()
}).