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
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).
-export([
   horn_1/1, horn_n/1,
   inline_string/1, inline_int/1, inline_float/1,
   anything/1,
   guard_eq_string/1, guard_eq_set_of_string/1, guard_eq_int/1, guard_eq_float/1,
   guard_gt_string/1, guard_gt_int/1, guard_gt_float/1,         
   guard_string/1, guard_number/1,
   bif_datalog/1, bif_external/1,
   semantic_web/1,
   jsonld/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, datalog}
   ].

groups() ->
   [
      {datalog, [parallel], [
         horn_1, horn_n,
         inline_string, inline_int, inline_float,
         anything,
         guard_eq_string, guard_eq_set_of_string, guard_eq_int, guard_eq_float,
         guard_gt_string, guard_gt_int, guard_gt_float,         
         guard_string, guard_number,
         bif_datalog, bif_external,
         semantic_web,
         jsonld
      ]}
   ].


%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

%%
%%
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%%   
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

%%
%%
horn_1(_Config) ->
   #{
      h := [[x,y], 
         #{'@' := a, '_' := [x,y]}
      ]
   } = datalog:p("h(x,y) :- a(x, y).").

horn_n(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z).").

%%
%%
inline_string(_Config) ->
   #{
      h := [[z], 
         #{'@' := a, '_' := [<<"x">>,y]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(z) :- a(\"x\", y), b(y, z).").

inline_int(_Config) ->
   #{
      h := [[z], 
         #{'@' := a, '_' := [100,y]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(z) :- a(100, y), b(y, z).").

inline_float(_Config) ->
   #{
      h := [[z], 
         #{'@' := a, '_' := [1.0,y]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(z) :- a(1.0, y), b(y, z).").

%%
%%
anything(_Config) ->
   #{
      h := [[z], 
         #{'@' := a, '_' := ['_',y]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(z) :- a(_, y), b(y, z).").

%%
%%
guard_eq_string(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := <<"x">>},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x = \"x\".").

guard_eq_set_of_string(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := [<<"x">>, <<"y">>, <<"z">>]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x = (\"x\", \"y\", \"z\").").


guard_eq_int(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := 100},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x = 100.").

guard_eq_float(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := 1.0},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x = 1.0.").

%%
%%
guard_gt_string(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := [{'>', <<"x">>}]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x > \"x\".").

guard_gt_int(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := [{'>', 100}]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x > 100.").

guard_gt_float(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := [{'>', 1.0}]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x > 1.0.").


%%
%%
guard_string(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := [{'>=', <<"x">>}, {'=<', <<"z">>}]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x >= \"x\", x =< \"z\" .").

guard_number(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := a, '_' := [x,y], x := [{'>=', 1.0}, {'=<', 100}]},
         #{'@' := b, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- a(x, y), b(y, z), x >= 1.0, x =< 100.").


%%
%%
bif_datalog(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := {datalog, a}, '_' := [x,y]},
         #{'@' := {datalog, b}, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- .a(x, y), .b(y, z).").

bif_external(_Config) ->
   #{
      h := [[x,z], 
         #{'@' := {mod, a}, '_' := [x,y]},
         #{'@' := {mod, b}, '_' := [y,z]}
      ]
   } = datalog:p("h(x,z) :- mod.a(x, y), mod.b(y, z).").

%%
%%
semantic_web(_Config) ->
   #{
      'foaf:person' := [['rdf:id', 'foaf:name'], 
         #{'@' := 'foaf:person', '_' := ['rdf:id', 'foaf:name'], 'foaf:name' := [{'>', <<"A">>}, {'<', <<"B">>}]}
      ]
   } = datalog:p("foaf:person(rdf:id, foaf:name) :- foaf:person(rdf:id, foaf:name), foaf:name > \"A\", foaf:name < \"B\".").

%%
%%
jsonld(_Config) ->
   #{
      h := [['@id',z], 
         #{'@' := a, '_' := ['@id', '@type']},
         #{'@' := b, '_' := ['@type', z]}
      ]
   } = datalog:p("h(@id,z) :- a(@id, @type), b(@type, z).").

