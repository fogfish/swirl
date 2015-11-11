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
%%   helper utility used by parser to render context
-module(swirl_context).

% -export([
%    get/2,

%    ref/2,
%    s/1
% ]).

% -export([eval/2, has/2, set/3]).

% -type(key() :: [atom()]).

% %%
% %% get value from context
% -spec(get/2 :: (key(), list() | map()) -> any()).

% get(Key, X) ->
%    try getv(Key, X) catch _:_ -> undefined end.

% getv([Key],   X)
%  when is_map(X) ->
%    maps:get(Key, X);
% getv([Key],   X)
%  when is_list(X) ->
%    {value, Val} = lists:keyfind(Key, 1, X),
%    Val;
% getv([Key|T], X) ->
%    mget(T, maps:get(Key, X)).

% lget([Key|T], X) ->
%    lget(T, lists:keyfind(Key, 1, X)).   


%%
%% reference template
% ref(Ref, X) ->
%    erlang:apply()

%    Fun = maps:get(Ref, X),
%    Fun(X).



% %%
% %% Note: dangerous (used for evaluate purpose only)
% eval(Expr, _List) ->
%    {ok, Scanned, _} = erl_scan:string(Expr),
%    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
%    case erl_eval:exprs(Parsed, []) of
%       {value, Value, _} when is_list(Value) orelse is_binary(Value) ->
%          Value;
%       _ ->
%          []
%    end.


% %%
% %% check value in context
% has([Key], List) ->
%    case lists:keyfind(Key, 1, List) of
%    	false      -> false;
%       {_, Val}   -> to_bool(Val)
%    end;

% has([Key|T], List) ->
%    case lists:keyfind(Key, 1, List) of
%    	false      -> false;
%       {_, Val}   -> has(T, Val)
%    end.

% %%
% %% 
% set(Key, Val, List) ->
%    case lists:keytake(Key, 1, List) of
%    	false      -> [{Key, Val} | List];
%    	{_,_, New} -> [{Key, Val} |  New]
%    end.


% %%
% %% scalar to string
% -spec(s/1 :: (any()) -> binary()).

% s(undefined)            -> [];
% s(X) when is_binary(X)  -> btos(X);
% s(X) when is_atom(X)    -> atos(X);
% s(X) when is_list(X)    -> ltos(X);
% s(X) when is_integer(X) -> itos(X);
% s(X) when is_float(X)   -> ftos(X).

% btos(X) -> X.
% atos(X) -> atom_to_binary(X, utf8).
% ltos(X) -> iolist_to_binary(X).
% itos(X) -> ltos(itol(X)).
% ftos(X) -> ltos(io_lib:format("~.9f", [X])).
% itol(X) -> integer_to_list(X).


% % %%
% % %%
% % to_list(false) ->
% %    [];
% % to_list(undefined) ->
% %    [];
% % to_list(X) 
% %  when is_atom(X) ->
% %    atom_to_list(X);

% % to_list(X)
% %  when is_integer(X) ->
% %    integer_to_list(X);

% % to_list(X)
% %  when is_binary(X) ->
% %    X;
% %    % binary_to_list(X);

% % to_list(X)
% %  when is_list(X) ->
% %    X.

% %%
% %%
% to_bool(false) ->
%    false;
% to_bool(undefined) ->
%    false;
% to_bool([]) ->
%    false;
% to_bool(0) ->
%    false;
% to_bool(_) ->
%    true.



