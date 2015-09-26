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
-module(swirl_context).

-export([eval/2, get/2, has/2, set/3, to_list/1]).

%%
%% Note: dangerous (used for evaluate purpose only)
eval(Expr, _List) ->
   {ok, Scanned, _} = erl_scan:string(Expr),
   {ok, Parsed} = erl_parse:parse_exprs(Scanned),
   case erl_eval:exprs(Parsed, []) of
      {value, Value, _} when is_list(Value) orelse is_binary(Value) ->
         Value;
      _ ->
         []
   end.

%%
%% get value from context
get([Key], List) ->
   io:format("==> ~p~n", [Key]),
   case lists:keyfind(Key, 1, List) of
   	false    -> undefined;
      {_, Val} -> Val
   end;

get([Key|T], List) ->
   io:format("==> ~p~n", [Key]),
   case lists:keyfind(Key, 1, List) of
   	false    -> [];
      {_, Val} -> get(T, Val)
   end.

%%
%% check value in context
has([Key], List) ->
   case lists:keyfind(Key, 1, List) of
   	false      -> false;
      {_, Val}   -> to_bool(Val)
   end;

has([Key|T], List) ->
   case lists:keyfind(Key, 1, List) of
   	false      -> false;
      {_, Val}   -> has(T, Val)
   end.

%%
%% 
set(Key, Val, List) ->
   case lists:keytake(Key, 1, List) of
   	false      -> [{Key, Val} | List];
   	{_,_, New} -> [{Key, Val} |  New]
   end.

%%
%%
to_list(false) ->
   [];
to_list(undefined) ->
   [];
to_list(X) 
 when is_atom(X) ->
   atom_to_list(X);

to_list(X)
 when is_integer(X) ->
   integer_to_list(X);

to_list(X)
 when is_binary(X) ->
   binary_to_list(X);

to_list(X)
 when is_list(X) ->
   X.

%%
%%
to_bool(false) ->
   false;
to_bool(undefined) ->
   false;
to_bool([]) ->
   false;
to_bool(0) ->
   false;
to_bool(_) ->
   true.



