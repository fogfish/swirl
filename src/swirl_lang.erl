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
%% @doc
%%   language primitive interface (used from .yrl)
-module(swirl_lang).

-export([
   include/2,
   foreach/4,
   eval/2
]).


%%
%% partial  {>...}
include(Key, X) ->
   case pair:x(Key, X) of
      undefined ->
         <<>>;
      Val when is_list(Val) ->
         swirl:apply(Val, X);
      
      Val when is_atom(Val) ->
         Fun = pair:x(['>', Val], X),
         Fun(X);
      
      {Mod, Fun} ->
         Mod:Fun(X)
   end. 

%%
%% apply inner template function for each element in list
%% {for var in key} ... {/for}
foreach(Key, Var, Fun, X) ->
   foreach1(pair:x(Key, X), Var, Fun, X).

foreach1([Head], Var, Fun, X) ->
   X1 = context(['@tail'|Var], true, 
      context(['@head'|Var], true,
         context(Var, Head, X)
      )
   ),
   [Fun(X1)];

foreach1([Head|Tail], Var, Fun, X) ->
   X1 = context(['@head'|Var], true,
      context(Var, Head, X)
   ),
   [Fun(X1) | foreach2(Tail, Var, Fun, X)].

foreach2([Head], Var, Fun, X) ->
   X1 = context(['@tail'|Var], true,
      context(Var, Head, X)
   ),
   [Fun(X1)];

foreach2([Head|Tail], Var, Fun, X) ->
   [Fun(context(Var, Head, X)) | foreach2(Tail, Var, Fun, X)].


%%
%% update context
context([Key], Val, undefined) ->
   maps:put(Key, Val, #{});
context([Key], Val, X)
 when is_map(X) ->
   maps:put(Key, Val, X);
context([Key], Val, X)
 when is_list(X) ->
   lists:keystore(Key, 1, X, {Key, Val});

context([Prefix|Tail], Val, X)
 when is_map(X) ->
   Value = case pair:x(Prefix, X) of
      undefined ->
         context(Tail, Val, #{});
      Child     ->
         context(Tail, Val, Child)
   end,
   maps:put(Prefix, Value, X);

context([Prefix|Tail], Val, X)
 when is_list(X) ->
   Value = case pair:x(Prefix, X) of
      undefined ->
         context(Tail, Val, #{});
      Child     ->
         context(Tail, Val, Child)
   end,
   lists:keystore(Prefix, 1, X, {Prefix, Value}).


%%
%% Note: dangerous (used for evaluate purpose only)
eval(Expr, X) ->
   eval(pair:x('@eval', X), Expr, X).

eval(undefined, _Expr, _X) ->
   [];
eval(true, Expr, _X) ->
   {ok, Scanned, _} = erl_scan:string(Expr),
   {ok, Parsed} = erl_parse:parse_exprs(Scanned),
   case erl_eval:exprs(Parsed, []) of
      {value, Value, _} when is_list(Value) orelse is_binary(Value) ->
         Value;
      _ ->
         []
   end.
