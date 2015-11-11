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
-module(swirl).

-export([
   f/1,
   c/1,
   x/2,
   apply/2,
   apply/3
]).


% -export([main/1]).
% -export([c/2, cc/1, cc/2, r/2]).

%%
%% define template evaluator
-spec(f/1 :: (list()) -> function()).

f(T)
 when is_binary(T) orelse is_list(T) ->
   {ok, Fun} = swirl:c(T),
   fun(Scope) ->
      fun(X) ->
         eval(Fun, Scope,  X)
      end
   end;

f(T) ->
   fun(Id) ->
      Funs = maps:map(fun(Key, Fun) -> Fun(Key) end, maps:without([Id], T)),
      Root = maps:get(Id, T),
      fun(X) ->
         swirl:apply(Root, maps:put('>', Funs, X))
      end
   end.



%%
%% compile template to abstract syntax tree 
-spec(c/1 :: (list()) -> {ok, any()}).

c(T) ->
   {ok, Lex, _} = swirl_lexer:string(scalar:c(T)),
   swirl_parser:parse(Lex).

%%
%%
x([undefined|Path], X) ->
   pair:x(Path, X);
x(Path, X) ->
   pair:x(Path, X).


%%
%% r(Str, Ctx) -> Val
%%
%% render template with given context
% r(Str, Ctx) ->
%    Bind = erl_eval:add_binding('C', Ctx,
%       erl_eval:new_bindings()
%    ),
%    {_, Val, _} = erl_eval:exprs(cc_code(Str), Bind),
%    Val.

%%
%% helper evaluate template
apply(Fun, X) ->
   swirl:apply(Fun, undefined, X).

apply(Fun, Y, X)
 when is_function(Fun) ->
   Fun1 = Fun(Y),
   Fun1(X);

apply(T, Y, X) ->
   swirl:apply(swirl:f(T), Y, X).



% %%
% %% swirl -o Dir INPUT
% main(["-o", Dir, Input]) ->
%    % compile template into beam file
%    cc(Input, [{outdir, Dir}]);

% main(["-j", Json, Input]) ->
%    % bind a template with json
%    {ok, S} = file:read_file(Input),
%    {ok, J} = file:read_file(Json),
%    R = r(
%       binary_to_list(S), 
%       jsx:decode(J, [{labels, atom}])
%    ),
%    io:format("~s", [R]);

% main([]) ->
%    io:format("Usage: swirl -o Dir INPUT\n").



% %%
% %% r(Str, Ctx) -> Val
% %%
% %% render template with given context
% r(Str, Ctx) ->
%    Bind = erl_eval:add_binding('C', Ctx,
%       erl_eval:new_bindings()
%    ),
%    {_, Val, _} = erl_eval:exprs(cc_code(Str), Bind),
%    Val.

% %%
% %% c(Uid, Str) -> ok
% %%
% %% compiles template from string
% c(Uid, Str) ->
%    {ok, Mod, Bin} = cc_mod(Uid, Str),
%    {module, Uid}  = code:load_binary(Mod, [], Bin),
%    ok.

% %%
% %% cc(File, Opts) -> ok
% %%
% %% compiles template from file
% cc(File) ->
%    cc(File, []).

% cc(File, Opts) ->
%    {ok, Bstr} = file:read_file(File),
%    Uid = list_to_atom(uid(File, Opts)),
%    {ok, Mod, Bin} = cc_mod(Uid, binary_to_list(Bstr)),
%    {module, Uid}  = code:load_binary(Mod, [], Bin),
%    case beam(File, Opts) of
%       undefined -> ok;
%       Beam      -> file:write_file(Beam, Bin)
%    end.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% evaluate abstract syntax tree
eval(Fun, undefined, X) ->
   {value, Val, _} = erl_eval:exprs(Fun, 
      erl_eval:add_binding('P', undefined,
         erl_eval:add_binding('C', X, 
            erl_eval:new_bindings()
         )
      )
   ),
   erlang:iolist_to_binary(Val);
   
eval(Fun, Scope, X) ->
   {value, Val, _} = erl_eval:exprs(Fun, 
      erl_eval:add_binding('P', Scope,
         erl_eval:add_binding('C', X, 
            erl_eval:new_bindings()
         )
      )
   ),
   Val.


% %%
% %%
% uid(File, Opts) ->
%    case proplists:get_value(module, Opts) of
%       % name space is not defined, uid is filename
%       undefined -> 
%          filename:basename(File, filename:extension(File));
%       Mod when is_atom(Mod) -> 
%          atom_to_list(Mod);
%       Mod when is_tuple(Mod) ->
%          string:join(
%             lists:map(
%                fun
%                (X) when is_atom(X) -> atom_to_list(X);
%                (X) when is_list(X) -> X
%                end,
%                tuple_to_list(Mod)
%             ),
%             "_"
%          )
%    end.

% %%
% %%
% beam(File, Opts) ->
%    case proplists:get_value(outdir, Opts) of
%       undefined -> undefined;
%       Dir       -> filename:join([Dir, uid(File, Opts) ++ ".beam"])
%    end.


% %%
% %% cc_mod(Uid, Str) -> {ok, Mod, Bin}
% %%
% %% compiles template into module
% cc_mod(Uid, Str) ->
%    compile:forms(
%       [
%          {attribute, 0, module, Uid},
%          {attribute, 0, compile, export_all},
%          cc_fun(render, Str),
%          {eof, 0}
%       ], 
%       []
%    ).

% %%
% %% compiles template into function
% cc_fun(Name, Str) ->
%    {function,  0, Name, 1,
%       [{clause, 0, 
%       [{var, 0, 'C'}], [],
%          cc_code(Str)
%       }]
%    }.



