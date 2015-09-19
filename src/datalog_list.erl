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
%%   reference implementation
-module(datalog_list).

-export([
   like/4
]).


like(List, '_', '_', '_') ->
   stream:build(List);

like(List,   A, '_', '_') ->
   stream:filter(fun({X, _, _}) -> X =:= A end, stream:build(List));
like(List, '_',   B, '_') ->
   stream:filter(fun({_, Y, _}) -> Y =:= B end, stream:build(List));
like(List, '_', '_',   C) ->
   stream:filter(fun({_, _, Z}) -> Z =:= C end, stream:build(List));

like(List,   A,   B, '_') ->
   stream:filter(fun({X, Y, _}) -> X =:= A andalso Y =:= B end, stream:build(List));
like(List,   A,  '_',  C) ->
   stream:filter(fun({X, _, Z}) -> X =:= A andalso Z =:= C end, stream:build(List));
like(List, '_', B,  C) ->
   stream:filter(fun({_, Y, Z}) -> Y =:= B andalso Z =:= C end, stream:build(List)).
   

