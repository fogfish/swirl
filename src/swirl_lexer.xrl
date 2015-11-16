%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
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
Definitions.

WSS = [\x20\x09\x0A\x0D]+
LIT = [^{}]+
ANY = [^{]+
VAR = [a-zA-Z._@]+

Rules.

%%
%% escaped
\\{{LIT}} : {token, {text, TokenLine, lists:sublist(TokenChars, 2, TokenLen - 1)}}.

%%
%% if statement
{if{WSS}not{WSS}{VAR}} : {token, {'ifnot', TokenLine, pp_ifnot(TokenChars)}}.
{if{WSS}{VAR}} : {token, {'if', TokenLine, pp_if(TokenChars)}}.
{else}         : {token, {'else',  TokenLine}}.
{/if}          : {token, {'endif', TokenLine}}.

%%
%% for statement
{for{WSS}{VAR}{WSS}in{WSS}{VAR}} : {token, {'for',    TokenLine, pp_for(TokenChars)}}.
{/for}                           : {token, {'endfor', TokenLine}}.

%%
%% partials and assigment
{>{VAR}} : {token, {'&&', TokenLine, pp_par(TokenChars)}}.
{{VAR}}  : {token, {':=', TokenLine, pp_ref(TokenChars)}}.
{\.{LIT}} : {token, {'$$', TokenLine, pp_fun(TokenChars)}}.


%%
%% literals
{LIT}    : {token, {text, TokenLine, TokenChars}}.
{+       : {token, {text, TokenLine, TokenChars}}.
}+       : {token, {text, TokenLine, TokenChars}}.
   
%%
%%
Erlang code.

%%
%% pre-processes reference
pp_ref(Tkn) -> 
   dotbind(
      string:strip(
         lists:sublist(Tkn, 2, length(Tkn) - 2),
         both,
         16#20
      )
   ).

%%
%% pre-process if statement
pp_if(Tkn) ->
   dotbind(
      string:strip(
         lists:sublist(Tkn, 4, length(Tkn) - 4),
         both,
         16#20
      )
   ).

pp_ifnot(Tkn) ->
   ["not", Prop] = string:tokens(
      string:strip(
         lists:sublist(Tkn, 4, length(Tkn) - 4),
         both,
         16#20
      ),
      " "
   ),
   dotbind(Prop).


%%
%% pre-process for statement
pp_for(Tkn) ->
   [Var, "in", Prop] = string:tokens(
      string:strip(
         lists:sublist(Tkn, 5, length(Tkn) - 5),
         both,
         16#20
      ),
      " "
   ),
   [list_to_atom(Var), dotbind(Prop)].


%%
%% pre-processes partial
pp_par(Tkn) -> 
   dotbind(
      string:strip(
         lists:sublist(Tkn, 3, length(Tkn) - 3),
         both,
         16#20
      )
   ).

%%
%% pre-processes function call
pp_fun(Tkn) -> 
   funbind(
      string:strip(
         lists:sublist(Tkn, 3, length(Tkn) - 3),
         both,
         16#20
      )
   ).


%%
%% parses dot-bind notation of key
dotbind(Var) ->
   lists:reverse(
      lists:map(
         fun list_to_atom/1,
         string:tokens(Var, ".")
      )
   ).

%%
%% parses fun-bind notation of key
funbind(Var) ->
   lists:reverse(Var).
