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
   basic_all/1
  ,imdb_person_1/1
  ,imdb_person_2/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, basic}, {group, imdb}
   ].

groups() ->
   [
      {basic, [], [
         basic_all
      ]}
     ,{imdb,  [], [
         imdb_person_1
        ,imdb_person_2
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

%%
%%
end_per_suite(_Config) ->
   ok.

%%   
%%
init_per_group(_, Config) ->
   Config.

%%
%%
end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

basic_all(_) ->
   {all, [x,y], 
      [
         {all, [x,y], [ {like, [x,y]} ]}
      ]
   } = datalog:p("
      ?- all(X,Y).
      all(X,Y) :- like(X,Y).
   ").

imdb_person_1(_) ->
   {id, [x,name, <<"Ridley Scott">>],
      [
         {id, [x,y,z], [ {like, [x,y,z]} ]}
      ]
   } = datalog:p("
      ?- id(X,name,\"Ridley Scott\").
      id(X,Y,Z) :- like(X,Y,Z).
   ").

imdb_person_2(_) ->
   {id, [x,<<"Ridley Scott">>],
      [
         {id, [x,z], [ {like, [x,name,z]} ]}
      ]
   } = datalog:p("
      ?- id(X,\"Ridley Scott\").
      id(X,Z) :- like(X,name,Z).
   ").
