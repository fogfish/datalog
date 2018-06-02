-module(datalog_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).

-export([all/0]).
-export([
   unary_product/1,
   unary_join/1,
   unary_eq_2/1,
   unary_lt_2/1,
   unary_le_2/1,
   unary_gt_2/1,
   unary_ge_2/1,
   unary_ne_2/1,

   binary_product/1,
   binary_join/1
]).

all() ->
   [
      unary_product,
      unary_join,
      unary_eq_2,
      unary_lt_2,
      unary_le_2,
      unary_gt_2,
      unary_ge_2,
      unary_ne_2,

      binary_product,
      binary_join
   ].

datalog(Datalog, Fixture, Expect) ->
   Expect = [identity ||
      datalog:p(Datalog),
      datalog:c(h, datalog_list, _),
      datalog:q(_, Fixture),
      stream:list(_)
   ].


unary_product(_) ->
   datalog(
      "h(a,b) :- p(a), p(b).",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_product()
   ).

unary_join(_) ->
   datalog(
      "h(a) :- p(a), p(a).",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_join()
   ).

unary_eq_2(_) ->
   datalog(
      "h(a) :- p(a), a = 2.",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_eq_2()
   ).

unary_lt_2(_) ->
   datalog(
      "h(a) :- p(a), a < 2.",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_lt_2()
   ).

unary_le_2(_) ->
   datalog(
      "h(a) :- p(a), a =< 2.",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_le_2()
   ).

unary_gt_2(_) ->
   datalog(
      "h(a) :- p(a), a > 2.",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_gt_2()
   ).

unary_ge_2(_) ->
   datalog(
      "h(a) :- p(a), a >= 2.",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_ge_2()
   ).

unary_ne_2(_) ->
   datalog(
      "h(a) :- p(a), a != 2.",
      datalog_FIXTURE:unary(),
      datalog_FIXTURE:unary_ne_2()
   ).


binary_product(_) ->
   datalog(
      "h(a,b) :- p(a, x), p(b, y).",
      datalog_FIXTURE:binary(),
      datalog_FIXTURE:binary_product()
   ).

binary_join(_) ->
   datalog(
      "h(a,b) :- p(a, x), p(x, b).",
      datalog_FIXTURE:binary(),
      datalog_FIXTURE:binary_join()
   ).
