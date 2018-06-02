-module(datalog_FIXTURE).

-export([
   unary/0,
   unary_product/0,
   unary_join/0,
   unary_eq_2/0,
   unary_lt_2/0,
   unary_le_2/0,
   unary_gt_2/0,
   unary_ge_2/0,
   unary_ne_2/0,

   binary/0,
   binary_product/0,
   binary_join/0
]).

unary() ->
   [
      {1}, {2}, {3}
   ].

unary_product() ->
   [
      #{a => 1, b => 1}, #{a => 1, b => 2}, #{a => 1, b => 3},
      #{a => 2, b => 1}, #{a => 2, b => 2}, #{a => 2, b => 3},
      #{a => 3, b => 1}, #{a => 3, b => 2}, #{a => 3, b => 3}
   ].

unary_join() ->
   [
      #{a => 1}, #{a => 2}, #{a => 3}
   ].

unary_eq_2() ->
   [
      #{a => 2}
   ].

unary_lt_2() ->
   [
      #{a => 1}
   ].

unary_le_2() ->
   [
      #{a => 1}, #{a => 2}
   ].

unary_gt_2() ->
   [
      #{a => 3}
   ].

unary_ge_2() ->
   [
      #{a => 2}, #{a => 3}
   ].

unary_ne_2() ->
   [
      #{a => 1}, #{a => 3}
   ].


binary() ->
   [
      {1, 2}, {2, 3}, {3, 4}
   ].

binary_product() ->
   [
      #{a => 1, b => 1}, #{a => 1, b => 2}, #{a => 1, b => 3},
      #{a => 2, b => 1}, #{a => 2, b => 2}, #{a => 2, b => 3},
      #{a => 3, b => 1}, #{a => 3, b => 2}, #{a => 3, b => 3}
   ].

binary_join() ->
   [
      #{a => 1, b => 3},
      #{a => 2, b => 4}
   ].
