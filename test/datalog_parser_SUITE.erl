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
  ,jsonld_support/1
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
         basic_all,
         jsonld_support
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
init_per_group(basic, Config) ->
   List = [{a,1}, {b,1}, {c,2}, {d,2}, {e,3}, {f,3}, {g,4}, {h,4}, {i,5}, {j,5}],
   [{list, List} | Config];

init_per_group(imdb, Config) ->
   {ok, List} = file:consult(filename:join([code:priv_dir(datalog), "imdb.config"])),
   [{list, List} | Config];

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
   #{
      all := [[x,y], 
         #{'@' := like, '_' := [x,y]}
      ]
   } = datalog:p("all(X,Y) :- like(X,Y).").

imdb_person_1(_) ->
   #{
      id := [[x,name, <<"Ridley Scott">>],
         #{'@' := like, '_' := [x,y,z]}
      ]
   } = datalog:p("id(X,name,\"Ridley Scott\") :- like(X,Y,Z).").

imdb_person_2(_) ->
   #{
      id := [[x,<<"Ridley Scott">>],
         #{'@' := like, '_' := [x,name,z]}
      ]
   } = datalog:p("id(X,\"Ridley Scott\") :- like(X,name,Z).").

jsonld_support(_) ->
   #{
      id := [['@id', '@type'],
         #{'@' := like, '_' := ['@id',name,'@type']}
      ]
   } = datalog:p("id(@id, @type) :- like(@id,name,@type).").
   
urn_support(_) ->
   #{
      id := [[x,y],
         #{'@' := 'urn:type:like',  '_' := [x,'urn:type:name',y]}
      ]
   } = datalog:p("id(X, Y) :- urn:type:like(X,urn:type:name,Y).").
   
