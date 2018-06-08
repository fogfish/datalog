-module(datalog_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).

-export([all/0]).
-export([
   product/1,
   join/1,
   eq_2/1,
   lt_2/1,
   le_2/1,
   gt_2/1,
   ge_2/1,
   ne_2/1
]).

all() ->
   [
      product,
      join,
      eq_2,
      lt_2,
      le_2,
      gt_2,
      ge_2,
      ne_2
   ].

datalog(Datalog, Expect) ->
   Expect = [identity ||
      datalog:p(Datalog),
      datalog:c(datalog_list, _),
      datalog:q(_, []),
      stream:list(_)
   ].


product(_) ->
   datalog(
      "?- h(_,_). a(x) :- .stream(3, 1). h(x,y) :- a(x), a(y).",
      [
         [1, 1], [1, 2], [1, 3],
         [2, 1], [2, 2], [2, 3],
         [3, 1], [3, 2], [3, 3]
      ]
   ).

join(_) ->
   datalog(
      "?- h(_,_). a(x,y) :- .stream(3, 1, 1). h(x,z) :- a(x,y), a(y,z).",
      [
         [1, 1], [2, 1], [3, 1]
      ]
   ).

eq_2(_) ->
   datalog(
      "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x = 2.",
      [
         [2]
      ]
   ).

lt_2(_) ->
   datalog(
      "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x < 2.",
      [
         [1]
      ]
   ).

le_2(_) ->
   datalog(
      "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x =< 2.",
      [
         [1], [2]
      ]
   ).

gt_2(_) ->
   datalog(
      "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x > 2.",
      [
         [3]
      ]
   ).

ge_2(_) ->
   datalog(
      "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x >= 2.",
      [
         [2], [3]
      ]
   ).

ne_2(_) ->
   datalog(
      "?- h(_). a(x) :- .stream(3, 1). h(x) :- a(x), x != 2.",
      [
         [1], [3]
      ]
   ).

