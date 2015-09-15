%% @doc
%%   datalog
Definitions.

LC    = [a-z]
UC    = [A-Z]
DIGIT = [0-9]
WS    = ([\000-\s]|%.*)

%%
%%
Rules.

{LC}+   :
   {token, {symbol,  TokenLine, list_to_atom(TokenChars)}}.
{UC}+   :
   {token, {symbol,  TokenLine, list_to_atom(TokenChars)}}.

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
