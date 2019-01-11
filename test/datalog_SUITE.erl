-module(datalog_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).

-export([all/0]).
-export([
   predicate_stream_arity_1/1
,  predicate_stream_arity_2/1
,  predicate_stream_arity_3/1
,  predicate_horn_1/1

   % product/1,
   % join/1,
   % eq_2/1,
   % lt_2/1,
   % le_2/1,
   % gt_2/1,
   % ge_2/1,
   % ne_2/1
]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      NAry =:= 1
   ].

datalog(Datalog, Input, Expect) ->
   Expect = [identity ||
      datalog:p(Datalog),
      datalog:c(datalog_list, _, [{return, tuple}]),
      datalog:q(_, Input),
      stream:list(_)
   ].

%%
%%
predicate_stream_arity_1(_) ->
   datalog(
      "?- p(_). p(x).",
      [{1}, {2}, {3}],
      [{1}, {2}, {3}]
   ).

predicate_stream_arity_2(_) ->
   datalog(
      "?- p(_, _). p(x, y).",
      [{1, 2}, {2, 3}, {3, 4}],
      [{1, 2}, {2, 3}, {3, 4}]
   ).

predicate_stream_arity_3(_) ->
   datalog(
      "?- p(_, _, _). p(x, y, z).",
      [{1, 2, 3}, {2, 3, 4}, {3, 4, 5}],
      [{1, 2, 3}, {2, 3, 4}, {3, 4, 5}]
   ).

%%
%%
predicate_horn_1(_) ->
   datalog(
      "?- a(_, _). p(x, y). a(x, y) :- p(x, y).",
      [{1, 2}, {2, 3}, {3, 4}],
      [{1, 2}, {2, 3}, {3, 4}]
   ).




% product(_) ->
%    datalog(
%       "?- h(_,_). a(x) :- .stream(3, 1). h(x,y) :- a(x), a(y).",
%       [
%          [1, 1], [1, 2], [1, 3],
%          [2, 1], [2, 2], [2, 3],
%          [3, 1], [3, 2], [3, 3]
%       ]
%    ).

% join(_) ->
%    datalog(
%       "?- h(_,_). a(x,y) :- .stream(3, 1, 1). h(x,z) :- a(x,y), a(y,z).",
%       [
%          [1, 1], [2, 1], [3, 1]
%       ]
%    ).

% eq_2(_) ->
%    datalog(
%       "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x = 2.",
%       [
%          [2]
%       ]
%    ).

% lt_2(_) ->
%    datalog(
%       "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x < 2.",
%       [
%          [1]
%       ]
%    ).

% le_2(_) ->
%    datalog(
%       "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x =< 2.",
%       [
%          [1], [2]
%       ]
%    ).

% gt_2(_) ->
%    datalog(
%       "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x > 2.",
%       [
%          [3]
%       ]
%    ).

% ge_2(_) ->
%    datalog(
%       "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x >= 2.",
%       [
%          [2], [3]
%       ]
%    ).

% ne_2(_) ->
%    datalog(
%       "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x != 2.",
%       [
%          [1], [3]
%       ]
%    ).

