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
   jsonld/1
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

