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
%%   datalog
Definitions.

LC    = [a-z_@:]
UC    = [A-Z_@:]
DIGIT = [0-9]
WS    = ([\000-\s]|%.*)

%%
%%
Rules.

{LC}+   :
   {token, {symbol, TokenLine, list_to_atom(TokenChars)}}.
{UC}+{LC}*  :
   {token, {var, TokenLine, list_to_atom(string:to_lower(TokenChars))}}.

%%
%% literals
\"[^\"]*\" :
   {token, {lit, TokenLine, erlang:list_to_binary(strip(TokenChars,TokenLen))}}.

{DIGIT}+ :
   {token, {lit, TokenLine, erlang:list_to_integer(TokenChars)}}.

(-|\+)?{DIGIT}+\.{DIGIT}+ :
   {token, {lit, TokenLine, erlang:list_to_float(TokenChars)}}.

\?\- :
   {token,{list_to_atom(TokenChars),TokenLine}}.

\:\- :
   {token,{list_to_atom(TokenChars),TokenLine}}.

[()\[\]_<=>!.,] :
   {token,{list_to_atom(TokenChars),TokenLine}}.

{WS}+  : skip_token.

%%
%%
Erlang code.

strip(TokenChars,TokenLen) ->
   lists:sublist(TokenChars, 2, TokenLen - 2).
