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
   c/2,
   apply/2,
   apply/3,
   main/1
]).


%%
%% return partial application from input template  
-spec f(list()) -> function().

f(T)
 when is_binary(T) orelse is_list(T) ->
   {ok, Fun} = swirl:c(T),
   fun(Scope) ->
      fun(X) ->
         eval(Fun, Scope,  X)
      end
   end;

f(T)
 when is_map(T) ->
   fun(Id) ->
      Funs = maps:map(fun(Key, Fun) -> Fun(Key) end, T),
      Root = maps:get(Id, T),
      fun(X) ->
         swirl:apply(Root, Id, pair:insert('>', Funs, X))
      end
   end.

%%
%% compile template to abstract syntax tree 
-spec c(list()) -> {ok, any()}.
-spec c(atom(), list()) -> {ok, atom(), binary()}.

c(T)
 when is_binary(T) orelse is_list(T) ->
   {ok, Lex, _} = swirl_lexer:string(scalar:c(T)),
   swirl_parser:parse(Lex).

c(Id, T)
 when is_map(T) ->
   cc_mod(Id, maps:map(fun cc_fun/2, T)).

%%
%% helper evaluate template
-spec apply(function(), any()) -> binary().
-spec apply(function(), atom() | [atom()], any()) -> binary().

apply(Fun, X) ->
   swirl:apply(Fun, undefined, X).

apply(Fun, Y, X)
 when is_function(Fun) ->
   Fun1 = Fun(Y),
   Fun1(X);

apply(T, Y, X) ->
   swirl:apply(swirl:f(T), Y, X).


%%
%% command line utility
-spec main(list()) -> ok.

main(Args) ->
   swirl_tool:main(Args).

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
   erlang:iolist_to_binary(Val).

%%
%% compiles template into function
cc_fun(f,  T) ->
   {ok, Fun} = swirl:c(T),
   {function,  0, f, 1,
      [{clause, 0,  
         [{var, 0, 'C'}], [],
         [
            {match,0,{var,0,'P'},{atom,0,undefined}},
            {call, 0,
               {remote, 0,{atom,0,erlang},{atom,0,iolist_to_binary}},
               Fun
            }
         ]
      }]
   };
cc_fun(Id, T) ->
   {ok, Fun} = swirl:c(T),
   {function,  0, Id, 1,
      [{clause, 0,  
         [{var, 0, 'C'}], [],
         [
            {match,0,{var,0,'P'},{atom,0,Id}},
            {call, 0,
               {remote, 0,{atom,0,erlang},{atom,0,iolist_to_binary}},
               Fun
            }
         ]
      }]
   }.

%%
%% compiles template into module
cc_mod(Id, T) ->
   Mod = 
      [
         {attribute, 0, module, Id},
         {attribute, 0, compile, export_all}
      ] ++
      [X || {_, X} <- maps:to_list(T)] ++
      [
         {eof, 0}
      ],
   compile:forms(Mod, []).





