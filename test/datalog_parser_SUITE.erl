%%
%%   Copyright 2014 - 2015 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   datalog parser test suite
-module(datalog_parser_SUITE).
-include_lib("common_test/include/ct.hrl").

%% common test
-export([all/0]).
-export([
   goal/1,
   stream/1,
   stream_native/1,
   stream_semantic/1,
   horn_1/1, 
   horn_n/1,
   horn_0/1,
   inline_string/1, 
   inline_int/1, 
   inline_float/1,
   anything/1,
   guard_eq_string/1, 
   guard_eq_tuple_of_string/1,
   guard_eq_int/1, 
   guard_eq_float/1,
   guard_gt_string/1, 
   guard_gt_int/1, 
   guard_gt_float/1,         
   guard_string/1, 
   guard_number/1,
   bif_datalog/1, 
   bif_external/1,
   semantic_web/1,
   jsonld/1,
   constant_xsd_absolute_uri/1,
   constant_xsd_compact_uri/1,
   constant_xsd_string/1,
   constant_xsd_lang_string/1,
   constant_xsd_integer/1,
   constant_xsd_decimal/1,
   constant_xsd_boolean/1,
   constant_xsd_datetime/1,
   constant_xsd_date/1,
   constant_xsd_yearmonth/1,
   constant_xsd_monthdate/1,
   constant_xsd_time/1,
   constant_xsd_seq/1,
   constant_xsd_map/1,
   constant_xsd_list/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      NAry =:= 1
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

%%
%%
goal(_) ->
   [
      {goal, h, [x,y]}
   ] = datalog:p("?- h(x, y).").

%%
%%
stream(_) ->
   [
      {source, p, [x, y]},
      {source, p, []}
   ] = datalog:p("p(x, y). p().").

stream_native(_) ->
   [
      {source, {datalog, a}, []},
      {source, {mod, a}, [<<"x">>, <<"y">>]}
   ] = datalog:p(".a(). mod.a(\"x\", \"y\").").

stream_semantic(_) ->
   [
      {source, {iri, <<"foaf">>, <<"person">>}, [x, y]}
   ] = datalog:p("foaf:person(x, y).").

%%
%%
horn_1(_Config) ->
   [
      {horn, h, [x,y], [ 
         #{'@' := a, '_' := [x,y]}
      ]}
   ] = datalog:p("h(x,y) :- a(x, y).").

horn_n(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z).").


horn_0(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y]},
         #{'@' := b, '_' := []}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b().").

%%
%%
inline_string(_Config) ->
   [
      {horn, h, [z], [ 
         #{'@' := a, '_' := [<<"x">>,y]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(z) :- a(\"x\", y), b(y, z).").

inline_int(_Config) ->
   [
      {horn, h, [z], [ 
         #{'@' := a, '_' := [100,y]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(z) :- a(100, y), b(y, z).").

inline_float(_Config) ->
   [
      {horn, h, [z], [ 
         #{'@' := a, '_' := [1.0,y]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(z) :- a(1.0, y), b(y, z).").

%%
%%
anything(_Config) ->
   [
      {horn, h, [z], [ 
         #{'@' := a, '_' := ['_',y]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(z) :- a(_, y), b(y, z).").

%%
%%
guard_eq_string(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := <<"x">>},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x = \"x\".").

guard_eq_tuple_of_string(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := {<<"x">>, <<"y">>, <<"z">>}},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x = (\"x\", \"y\", \"z\").").


guard_eq_int(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := 100},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x = 100.").

guard_eq_float(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := 1.0},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x = 1.0.").

%%
%%
guard_gt_string(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := [{'>', <<"x">>}]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x > \"x\".").

guard_gt_int(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := [{'>', 100}]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x > 100.").

guard_gt_float(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := [{'>', 1.0}]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x > 1.0.").


%%
%%
guard_string(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := [{'>=', <<"x">>}, {'=<', <<"z">>}]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x >= \"x\", x =< \"z\" .").

guard_number(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := a, '_' := [x,y], x := [{'>=', 1.0}, {'=<', 100}]},
         #{'@' := b, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- a(x, y), b(y, z), x >= 1.0, x =< 100.").


%%
%%
bif_datalog(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := {datalog, a}, '_' := [x,y]},
         #{'@' := {datalog, b}, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- .a(x, y), .b(y, z).").

bif_external(_Config) ->
   [
      {horn, h, [x,z], [ 
         #{'@' := {mod, a}, '_' := [x,y]},
         #{'@' := {mod, b}, '_' := [y,z]}
      ]}
   ] = datalog:p("h(x,z) :- mod.a(x, y), mod.b(y, z).").

%%
%%
semantic_web(_Config) ->
   [
      {horn, 
         {iri, <<"foaf">>, <<"person">>}, 
         [{iri, <<"rdf">>, <<"id">>}, {iri, <<"foaf">>, <<"name">>}],
         [
            #{
               '@' := {iri, <<"foaf">>, <<"person">>},
               '_' := [{iri, <<"rdf">>, <<"id">>}, {iri, <<"foaf">>, <<"name">>}], 
               {iri, <<"foaf">>, <<"name">>} := [{'>', <<"A">>}, {'<', <<"B">>}]
            }
         ]
      }
   ] = datalog:p("foaf:person(rdf:id, foaf:name) :- foaf:person(rdf:id, foaf:name), foaf:name > \"A\", foaf:name < \"B\".").

%%
%%
jsonld(_Config) ->
   [
      {horn, h, ['@id',z], [ 
         #{'@' := a, '_' := ['@id', '@type']},
         #{'@' := b, '_' := ['@type', z]}
      ]}
   ] = datalog:p("h(@id,z) :- a(@id, @type), b(@type, z).").

%%
%%
constant_xsd_absolute_uri(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {iri, <<"http://example.com/a">>}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = <http://example.com/a>.").

constant_xsd_compact_uri(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {iri, <<"example">>, <<"a">>}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = example:a.").

constant_xsd_string(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := <<"example">>}
      ]}
   ] = datalog:p("h(x) :- a(x), x = \"example\".").

constant_xsd_lang_string(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := <<"example">>}
      ]}
   ] = datalog:p("h(x) :- a(x), x = \"example\"@en.").

constant_xsd_integer(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := 123}
      ]}
   ] = datalog:p("h(x) :- a(x), x = 123.").

constant_xsd_decimal(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := 12.3}
      ]}
   ] = datalog:p("h(x) :- a(x), x = 12.3.").

constant_xsd_boolean(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x, y], x := true, y := false}
      ]}
   ] = datalog:p("h(x) :- a(x, y), x = true, y = false.").

constant_xsd_datetime(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {1175,783410,0}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = 2007-04-05T14:30:10Z.").

constant_xsd_date(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {{2007,4,5},{0,0,0}}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = 2007-04-05.").

constant_xsd_yearmonth(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {{2007,4,0},{0,0,0}}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = 2007-04.").

constant_xsd_monthdate(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {{0,4,5},{0,0,0}}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = --04-05.").

constant_xsd_time(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {{0,0,0},{14,30,10}}}
      ]}
   ] = datalog:p("h(x) :- a(x), x = 14:30:10Z.").

constant_xsd_seq(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := {154, 7623, 23} }
      ]}
   ] = datalog:p("h(x) :- a(x), x = (154, 7623, 23).").

constant_xsd_map(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := #{<<"type">> := <<"Point">>, <<"coordinates">> := [21.3, 60.2]} }
      ]}
   ] = datalog:p("h(x) :- a(x), x = {type:\"Point\", coordinates:[21.3, 60.2]}.").

constant_xsd_list(_) ->
   [
      {horn, h, [x], [
         #{'@' := a, '_' := [x], x := [1, <<"a">>, 3.2] }
      ]}
   ] = datalog:p("h(x) :- a(x), x = [1, \"a\", 3.2].").


