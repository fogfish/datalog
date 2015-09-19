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
%%   basic datalog test suite
-module(datalog_basic_SUITE).
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
   application:start(datalog),
   Config.

%%
%%
end_per_suite(_Config) ->
   application:stop(datalog).

%%   
%%
init_per_group(imdb, Config) ->
   {ok, List} = file:consult(code:where_is_file("imdb.config")),
   [{imdb, List} | Config];

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
-define(LIST, [
   {a,1}, {b,1}, {c,2}, {d,2}, {e,3}, {f,3}, {g,4}, {h,4}, {i,5}, {j,5}
]).


%%
%%
basic_all(_) ->
   ?LIST = stream:list( datalog:q( datalog_all(), ?LIST, datalog_list) ).


%%
%%
imdb_person_1(Config) ->
   Actor =  <<"Ridley Scott">>,
   [{<<"urn:person:137">>, name, Actor}] = stream:list(
      datalog:q( datalog_person_1(Actor), ?config(imdb, Config), datalog_list )
   ).

imdb_person_2(Config) ->
   Actor =  <<"Ridley Scott">>,
   [{<<"urn:person:137">>, name, Actor}] = stream:list(
      datalog:q( datalog_person_2(Actor), ?config(imdb, Config), datalog_list )
   ).


datalog_all() ->
   {all, [x,y], 
      [
         {all, [x,y], [ {like, [x,y]} ]}
      ]
   }.

datalog_person_1(Actor) ->
   {id, [x,name, Actor],
      [
         {id, [x,y,z], [ {like, [x,y,z]} ]}
      ]
   }.

datalog_person_2(Actor) ->
   {id, [x,Actor],
      [
         {id, [x,z], [ {like, [x,name,z]} ]}
      ]
   }.

