-module(datalog_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).

-export([all/0]).
-export([
   predicate_stream_arity_1/1
,  predicate_stream_arity_2/1
,  predicate_stream_arity_3/1
,  predicate_single_horn_1/1
,  predicate_single_horn_2/1
,  predicate_single_horn_3/1
,  predicate_horn_2/1
,  predicate_horn_3/1
,  cartesian_product/1
,  infix_eq/1
,  infix_lt/1
,  infix_le/1
,  infix_gt/1
,  infix_ge/1
,  infix_ne/1
,  union_2/1
,  union_3/1
,  recursion_1/1
,  recursion_2/1
,  recursion_3/1
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
predicate_single_horn_1(_) ->
   datalog(
      "?- a(_, _). p(x, y). a(x, y) :- p(x, y).",
      [{1, 2}, {2, 3}, {3, 4}, {4, 5}],
      [{1, 2}, {2, 3}, {3, 4}, {4, 5}]
   ).

predicate_single_horn_2(_) ->
   datalog(
      "?- a(_, _). p(x, y). a(x, y) :- p(x, z), p(z, y).",
      [{1, 2}, {2, 3}, {3, 4}, {4, 5}],
      [{1, 3}, {2, 4}, {3, 5}]
   ).

predicate_single_horn_3(_) ->
   datalog(
      "?- a(_, _). p(x, y). a(x, y) :- p(x, z), p(z, f), p(f, y).",
      [{1, 2}, {2, 3}, {3, 4}, {4, 5}],
      [{1, 4}, {2, 5}]
   ).


predicate_horn_2(_) ->
   datalog(
      "?- b(_, _). p(x, y). a(x, y) :- p(x, y). b(x, y) :- a(x, z), p(z, y).",
      [{1, 2}, {2, 3}, {3, 4}, {4, 5}],
      [{1, 3}, {2, 4}, {3, 5}]
   ).

predicate_horn_3(_) ->
   datalog(
      "?- c(_, _). p(x, y). a(x, y) :- p(x, y). b(x, y) :- a(x, z), p(z, y). c(x, y) :- b(x, z), p(z, y).",
      [{1, 2}, {2, 3}, {3, 4}, {4, 5}],
      [{1, 4}, {2, 5}]
   ).

cartesian_product(_) ->
   datalog(
      "?- h(_,_). p(x). h(x,y) :- p(x), p(y).",
      [{1}, {2}, {3}],
      [{1,1}, {1,2}, {1,3}, {2,1}, {2,2}, {2,3}, {3,1}, {3,2}, {3,3}]
   ).

%%
%%
infix_eq(_) ->
   datalog(
      "?- h(_). p(x). h(x) :- p(x), x = 2.",
      [{1}, {2}, {3}, {4}],
      [{2}]
   ).

infix_lt(_) ->
   datalog(
      "?- h(_). p(x). h(x) :- p(x), x < 3.",
      [{1}, {2}, {3}, {4}],
      [{1}, {2}]
   ).

infix_le(_) ->
   datalog(
      "?- h(_). p(x). h(x) :- p(x), x =< 2.",
      [{1}, {2}, {3}, {4}],
      [{1}, {2}]
   ).

infix_gt(_) ->
   datalog(
      "?- h(_). p(x). h(x) :- p(x), x > 2.",
      [{1}, {2}, {3}, {4}],
      [{3}, {4}]
   ).

infix_ge(_) ->
   datalog(
      "?- h(_). p(x). h(x) :- p(x), x >= 3.",
      [{1}, {2}, {3}, {4}],
      [{3}, {4}]
   ).

infix_ne(_) ->
   datalog(
      "?- h(_). p(x). h(x) :- p(x), x != 2.",
      [{1}, {2}, {3}, {4}],
      [{1}, {3}, {4}]
   ).


%%
%%
union_2(_) ->
   datalog(
      "?- a(_, _). p(x,y). a(x,y) :- p(x,y), x > 2. a(x,y) :- p(x,y).",
      [{1,2}, {2,3}, {3,4}, {4,5}],
      [{3,4}, {4,5}, {1,2}, {2,3}, {3,4}, {4,5}]
   ).

union_3(_) ->
   datalog(
      "?- a(_, _). p(x,y). a(x,y) :- p(x,y), x > 2. a(x,y) :- p(x,y), x < 3. a(x,y) :- p(x,y).",
      [{1,2}, {2,3}, {3,4}, {4,5}],
      [{3,4}, {4,5}, {1,2}, {2,3}, {1,2}, {2,3}, {3,4}, {4,5}]
   ).

%%
%%
recursion_1(_) ->
   datalog(
      "?- a(_, _). p(x,y). a(x, y) :- p(x, y). a(x, y) :- p(x, z), a(z, y).",
      [{1,2}, {2,3}, {3,2}],
      [{1,2}, {2,3}, {1,3}, {3,3}, {3,2}, {2,2}]
   ).

recursion_2(_) ->
   datalog(
      "?- a(_, _). p(x,y). a(x, y) :- p(x, y). a(x, y) :- p(x, z), a(z, y).",
      [{1,2}, {2,3}, {3,4}, {4,5}],
      [{1,2}, {2,3}, {1,3}, {3,4}, {2,4}, {1,4}, {4,5}, {3,5}, {2,5}, {1,5}]
   ).

recursion_3(_) ->
   datalog(
      "?- a(_, _). p(x,y). a(x, y) :- p(x, y). a(x, y) :- p(x, z), a(z, y).",
      [{1,2}, {2,3}, {2,5}, {3,4}, {4,2}, {5,4}],
      [{1,2}, {2,3}, {1,3}, {4,3}, {3,3}, {5,3}, {2,5}, {1,5}, {4,5}, {3,5}, {5,5}, {3,4}, {2,4}, {1,4}, {4,4}, {5,4}, {4,2}, {3,2}, {2,2}, {5,2}]
   ).

