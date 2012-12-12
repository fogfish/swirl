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

-export([main/1]).
-export([c/2, cc/1, cc/2, r/2]).

%%
%% swirl -o Dir INPUT
main(["-o", Dir, Input]) ->
   % compile template into beam file
   cc(Input, [{outdir, Dir}]);

main(["-j", Json, Input]) ->
   % bind a template with json
   {ok, S} = file:read_file(Input),
   {ok, J} = file:read_file(Json),
   R = r(
      binary_to_list(S), 
      jsx:decode(J, [{labels, atom}])
   ),
   io:format("~s", [R]);

main([]) ->
   io:format("Usage: swirl -o Dir INPUT\n").



%%
%% r(Str, Ctx) -> Val
%%
%% render template with given context
r(Str, Ctx) ->
   Bind = erl_eval:add_binding('C', Ctx,
      erl_eval:new_bindings()
   ),
   {_, Val, _} = erl_eval:exprs(cc_code(Str), Bind),
   Val.

%%
%% c(Uid, Str) -> ok
%%
%% compiles template from string
c(Uid, Str) ->
   {ok, Mod, Bin} = cc_mod(Uid, Str),
   {module, Uid}  = code:load_binary(Mod, [], Bin),
   ok.

%%
%% cc(File, Opts) -> ok
%%
%% compiles template from file
cc(File) ->
   cc(File, []).

cc(File, Opts) ->
   {ok, Bstr} = file:read_file(File),
   Uid = list_to_atom(uid(File, Opts)),
   {ok, Mod, Bin} = cc_mod(Uid, binary_to_list(Bstr)),
   {module, Uid}  = code:load_binary(Mod, [], Bin),
   case beam(File, Opts) of
      undefined -> ok;
      Beam      -> file:write_file(Beam, Bin)
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
uid(File, Opts) ->
   case proplists:get_value(namespace, Opts) of
      % name space is not defined, uid is filename
      undefined -> 
         filename:basename(File, filename:extension(File));
      Ns        -> 
         Ns ++ "_" ++ filename:basename(File, filename:extension(File))
   end.

%%
%%
beam(File, Opts) ->
   case proplists:get_value(outdir, Opts) of
      undefined -> undefined;
      Dir       -> filename:join([Dir, uid(File, Opts) ++ ".beam"])
   end.


%%
%% cc_mod(Uid, Str) -> {ok, Mod, Bin}
%%
%% compiles template into module
cc_mod(Uid, Str) ->
   compile:forms(
      [
         {attribute, 0, module, Uid},
         {attribute, 0, compile, export_all},
         cc_fun(render, Str),
         {eof, 0}
      ], 
      []
   ).

%%
%% compiles template into function
cc_fun(Name, Str) ->
   {function,  0, Name, 1,
      [{clause, 0, 
      [{var, 0, 'C'}], [],
         cc_code(Str)
      }]
   }.

%%
%% compiles template code
cc_code(Str) ->
   {ok, Raw, _} = swirl_lexer:string(Str),
   {ok, Fun}    = swirl_parser:parse(Raw),
   Fun.



