%%
%%   Copyright 2014 - 2018 Dmitry Kolesnikov, All Rights Reserved
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
%%   evaluates a logical program
-module(datalog_vm).

-compile({parse_transform, partial}).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").
-include("datalog.hrl").

-export([
   union/1,
   recursion/2,

   % union/2,
   % union/3,
   horn/2,
   stream/1,
   stream/2
]).

%%
%% 
union(Horns) ->
   fun(Env) ->
      HHorns = [F(Env) || F <- Horns],
      fun(SubQ) ->
         stream:unfold(fun funion/1, {?stream(), SubQ, HHorns})
      end
   end.

funion({?stream(), _, []}) ->
   ?stream();
funion({?stream(), SubQ, [HHorn | THorn]}) ->
   funion({HHorn(SubQ), SubQ, THorn});
funion({Stream, SubQ, HHorns}) ->
   {stream:head(Stream), {stream:tail(Stream), SubQ, HHorns}}.


%%
%%
recursion(I, #horn{head = Head, body = Body}) ->
   fun(Env) ->
      IEnv  = I(Env),
      {Horns, [#{'_' := XHead}]} = lists:splitwith(fun(#{'@' := F}) -> is_function(F) end, Body),
      THorns =  [Spec#{'@' => F(Env)} || #{'@' := F} = Spec <- Horns],
      fun(SubQ) ->
         stream:unfold(fun frecc/1, {[IEnv(SubQ)], XHead, THorns, Head, sbf:new(128, 0.0001)})
      end
   end.

frecc({[undefined], _, _, _, _}) ->
   undefined;

frecc({[undefined | Stack], XHead, THorn, Head, Sbf}) ->
   frecc({Stack, XHead, THorn, Head, Sbf});

frecc({[H | T], XHead, THorn, Head, Sbf}) ->
   Tuple = stream:head(H),
   case not sbf:has(Tuple, Sbf) of
      true ->
         spinoff(Tuple, {[stream:tail(H) | T], XHead, THorn, Head, sbf:add(Tuple, Sbf)});
      false ->
         frecc({[stream:tail(H) | T], XHead, THorn, Head, Sbf})
   end.

spinoff(Cell, {Stack, XHead, [HHorn | THorn] = Horns, Head, Sbf}) ->
   Heap   = maps:from_list(lists:zip(XHead, Cell)),
   Stream = join(eval(Heap, HHorn), THorn),
   New = stream:map(
      fun(a) -> a; (X) ->
         [maps:get(K, X) || K <- Head]
      end,
      Stream
   ),
   {Cell, {[New | Stack], XHead, Horns, Head, Sbf}}.


% horn_recc_1(XHead, undefined, THorn, Head) ->
%    undefined;

% horn_recc_1(XHead, Prev, THorn, Head) ->
%    StreamP = stream:map(
%       fun(Tuple) -> maps:from_list( lists:zip(XHead, Tuple) ) end,
%       Prev
%    ),
%    Stream = join(StreamP, THorn),
%    New = stream:map(
%       fun(a) -> a; (X) ->
%          [maps:get(K, X) || K <- Head]
%       end,
%       Stream
%    ),
%    io:format("=[ xxxx ]=> ~p~n", [stream:list(New)]),
%    horn_recc_1(XHead, New, THorn, Head).
%    % horn_recc_1(XHead, filt(New), THorn, Head).




%%
%%
% union(A, B) ->
%    fun(Env, Lp) ->
%       HornA = A(Env, Lp),
%       % HornB = B(Env, Lp#{a => A}),
%       HornB = B(Env, Lp),
%       fun(SubQ) ->
%          HornB(SubQ, HornA(SubQ))
%          % HornB(SubQ)

%          %% vvvv -- union

%          % cc(HornA(SubQ), stream:new(a, fun() -> HornB(SubQ) end))
%          % HornA(SubQ)

%          % cc(stream:new(a, fun() -> HornA(SubQ) end), stream:new(x, fun() -> HornB(SubQ) end))
%          % cc(stream:new(a, fun() -> HornA(SubQ) end), stream:new(a, fun() -> HornB(SubQ) end))
%          % cc(HornA(SubQ), stream:new(x, fun() -> HornB(SubQ) end))
%       end
%    end.

% union(A, B, C) ->
%    fun(Env, Lp) ->
%       HornA = A(Env, Lp),
%       HornB = B(Env, Lp),
%       HornC = C(Env, Lp),
%       fun(SubQ) ->
%          cc(
%             HornA(SubQ), 
%             stream:new(undefined, 
%                fun() -> 
%                   cc(HornB(SubQ), stream:new(undefined, fun() -> HornC(SubQ) end))
%                end
%             )
%          )
%       end
%    end.

% % % uniz(Stream) ->
% % %    % {_, MasterOfS} = stream:splitwhile(fun(X) -> X /= a end, Stream),
% % %    % {A, B} = stream:splitwhile(fun(X) -> X /= a end, stream:tail(MasterOfS)),
% % %    {A, B} = stream:splitwhile(fun(X) -> X /= a end, Stream),
% % %    case filt(A) of
% % %       undefined ->
% % %          undefined;
% % %          % {C, D} = stream:splitwhile(fun(X) -> X /= a end, stream:tail(B)),
% % %          % case filt(C) of
% % %          %    undefined ->
% % %          %       undefined;
% % %          %    NewC ->
% % %          %       cc(NewC, stream:new(x, fun() -> unizz(D) end))
% % %          % end;
% % %       NewA ->
% % %          cc(NewA, stream:new(x, fun() -> unizz(B) end))
% % %    end.

% % % unizz(Stream) ->
% % %    uniz(stream:tail(Stream)).

% % filt(Stream) ->
% %    stream:filter(
% %       fun(X) ->
% %          case not sbf:has(X, erlang:get(sbf)) of
% %             true ->
% %                io:format("==> lift ~p~n", [X]),
% %                erlang:put(sbf, sbf:add(X, erlang:get(sbf))),
% %                true;
% %             false ->
% %                io:format("==> ignore ~p~n", [X]),
% %                false
% %          end
% %       end,
% %       Stream
% %    ).

% cc(?None, {stream, x, _} = StreamB) ->
%    stream:tail(StreamB);

% cc(?None, StreamB) ->
%    StreamB;
% cc(StreamA, ?None) ->
%    StreamA;
% cc(StreamA, StreamB) ->
%    stream:new(stream:head(StreamA), fun() -> cc(stream:tail(StreamA), StreamB) end).

% unique(Stream) ->
%    stream:unfold(fun uniq/1, Stream).

% uniq(?stream()) ->
%    stream:new();  

% uniq(Stream) ->
%    Item = stream:head(Stream),
%    Sbf1 = sbf:add(Item, erlang:get(sbf)),
%    erlang:put(sbf, Sbf1),
%    Tail = stream:filter(fun(X) -> not sbf:has(X, erlang:get(sbf)) end, Stream),
%    {stream:head(Stream), Tail}.


%%
%%
horn(Head, Horn) ->
   fun(Env) ->
      [HHorn | THorn] = [Spec#{'@' => F(Env)} || #{'@' := F} = Spec <- Horn, is_function(F)],
      fun(SubQ) ->
         Heap   = maps:from_list(lists:zip(Head, SubQ)),
         Stream = join(eval(Heap, HHorn), THorn),
         stream:map(
            fun(a) -> a; (X) ->
               [maps:get(K, X) || K <- Head]
            end,
            Stream
         )
      end
   end.


% env_call(a, Lp, Env) ->
%    Fun = maps:get(a, Lp),
%    fun(SubQ) ->
%       io:format("=[ qq ]=> ~p~n", [SubQ]),
%       % stream:new(a, fun() -> ( Fun(Env, Lp) )(SubQ) end)
%       Stream = ( Fun(Env, Lp) )(SubQ),
%       % % io:format("=[ data ]=> ~p~n", [stream:tail(Stream)]),
%       Stream
%    end;

% env_call(F, Lp, Env) when is_atom(F) ->
%    Fun = maps:get(F, Lp),
%    fun(SubQ) ->
%       ( Fun(Env, Lp) )(SubQ)
%    end;
% env_call(F, Lp, Env) ->
%    F(Env, Lp).

join(Stream, [#{'.' := pipe, '@' := Pipe} | THorn]) ->
   join(Pipe(Stream), THorn);

join(Stream, [HHorn | THorn]) ->
   join(
      stream:flat(
         stream:map(fun(Heap) -> eval(Heap, HHorn) end, Stream)
      ),
      THorn
   );

join(Stream, []) ->
   Stream.

eval(Heap, #{'_' := Head, '@' := Fun} = Spec) ->
   SubQ = [term(T, Spec, Heap) || T <- Head],
   stream:map(
      fun(a) -> a ; (Tuple) ->
         %% Note: we need to give a priority to existed heap values, unless '_'
         %% maps:merge(Heap, maps:from_list( lists:zip(Head, Tuple) ))
         Prev = maps:filter(fun(_, X) -> X /= '_' end, Heap),
         This = maps:from_list( lists:zip(Head, Tuple) ),
         maps:merge(Heap, maps:merge(This, Prev))
      end,
      Fun(SubQ)
   ).

%%
%%
term(T, Spec, Heap) ->
   [undefined || term(T, Spec), term(T, Heap)].

term(T, Predicate)
 when is_atom(T) ->
   case Predicate of
      #{T := Value} -> Value;
      _             -> undefined
   end;
term(T, _) ->
   T.

%%
%% evaluate stream 
stream(#{'.' := Keys, '>' := Spec, '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun(SubQ) -> (Gen(Keys, SubQ, Spec))(Env) end
   end;

stream(#{'.' := Keys, '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun(SubQ) -> (Gen(Keys, SubQ))(Env) end
   end;

stream(#{'_' := [_], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1]) -> (Gen(X1))(Env) end
   end;

stream(#{'_' := [_, _], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2]) -> (Gen(X1, X2))(Env) end
   end;

stream(#{'_' := [_, _, _],'@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2, X3]) -> (Gen(X1, X2, X3))(Env) end
   end;

stream(#{'_' := [_, _, _, _], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2, X3, X4]) -> (Gen(X1, X2, X3, X4))(Env) end
   end;

stream(#{'_' := [_, _, _, _, _], '@' := Gen}) ->
   fun(Env, _Lp) ->
      fun([X1, X2, X3, X4, X5]) -> (Gen(X1, X2, X3, X4, X5))(Env) end
   end.

%%
%%
stream(Gen, Head) ->
   fun(Env) ->
      fun(SubQ) -> (Gen(Head, SubQ))(Env) end
   end.

