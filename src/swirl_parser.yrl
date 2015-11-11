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
Nonterminals	template expressions expression.
Terminals		text ':=' 'if' 'ifnot' 'else' 'endif' 'for' 'endfor' '&&' '$$'.
Rootsymbol		template.


template  -> expressions : 
   template('$1').

expressions -> expression : 
   {cons, 0, '$1', {nil,1}}.
expressions -> expression expressions : 
   {cons, 0, '$1', '$2'}.

expression  -> text : lit('$1').
expression  -> ':=' : val('$1').
expression  -> '&&' : include('$1').
expression  -> '$$' : eval('$1').

expression  -> 'if' expressions 'endif' : 
   ifelse('$1', '$2', {nil, line('$1')}).
expression  -> 'if' expressions 'else' expressions 'endif' : 
   ifelse('$1', '$2', '$4').

expression  -> 'ifnot' expressions 'endif' : 
   ifelse('$1', {nil, line('$1')}, '$2').
expression  -> 'ifnot' expressions 'else' expressions 'endif' : 
   ifelse('$1', '$4', '$2').


expression  -> 'for' expressions 'endfor' :
   for('$1', '$2', {nil, line('$1')}).
expression  -> 'for' expressions 'else' expressions 'endfor' :
   for('$1', '$2', '$4').

%%
%%
Erlang code.

%%
%% get token value
value(Tkn)
 when is_tuple(Tkn) -> 
   element(3, Tkn).

%%
%% get token line
line(Tkn)  
 when is_tuple(Tkn) ->
   element(2, Tkn).

%%
%% parses variable into key
key(Tkn)
 when is_list(Tkn) ->
   Key = lists:foldl(
      fun(X, Acc) -> {cons, 1, {string, 1, X}, Acc} end,
      {nil, 1},
      Tkn
   ),
   {cons,1,{var,1,'P'},Key};

key(Tkn)
 when is_tuple(Tkn) ->
    key(value(Tkn)).


%%
%% output template clause definition
template(Mod) ->
   [{call, 0, 
      {remote,0, {atom,0, lists}, {atom,0, flatten}},
      [Mod]
   }].


%%
%% literal
lit(Lit) ->
   {string, line(Lit), scalar:s(value(Lit))}.

%%
%% get key from context
val(Key) ->
   {call, line(Key),
      {remote,line(Key),{atom,line(Key),scalar},{atom,line(Key),s}},
      [{call, line(Key), 
         {remote,line(Key),{atom,line(Key),swirl},{atom,line(Key),x}},
         [key(Key),{var,line(Key),'C'}]
      }]
   }.

%%
%% evaluate expression
eval(Key) ->
   {call, line(Key),
         {remote,line(Key),{atom,line(Key),swirl_context},{atom,line(Key),eval}},
         [key(Key),{var,0,'C'}]
   }.


%%
%%
ifelse(Key, True, False) ->
   {'case', line(Key),
      {call, line(Key), 
         {remote,line(Key),{atom,line(Key),swirl},{atom,line(Key),a}},
         [key(Key),{var,0,'C'}]
      },
      [
         {clause, line(Key),[{atom,line(Key),true}], [], [True]},
         {clause, line(Key),[{atom,line(Key),false}],[], [False]}
      ]
   }.


%%
%%
for(For, Loop, Empty) ->
   [V, K] = value(For),
   Var    = setelement(3, For, V),
   Key    = setelement(3, For, K), 
   ifelse(Key, loop(Var, Key, Loop), Empty). 

%%
%%
loop(Var, Key, Loop) ->
   {call, line(Var),
      {remote,line(Var),{atom,line(Var),lists},{atom,line(Var),map}},
      [
         {'fun', line(Var),
            {clauses, [
               {clause, line(Var), 
                  [{var,line(Var),'X'}], [],
                  [
                     {match, line(Var),
                        {var, line(Var), 'F'},
                        {'fun', line(Var), 
                           {clauses, [
                              {clause,line(Var),[{var,line(Var),'C'}],[],[Loop]}
                           ]}
                        }
                     },
                     {call, line(Var),
                        {var,line(Var),'F'},
                        [{call,line(Var),
                           {remote,line(Var),{atom,line(Var),swirl_context},{atom,line(Var),set}},
                           [{atom,line(Var),value(Var)},{var,line(Var),'X'},{var,line(Var),'C'}]
                        }]
                     }
                  ] 
               }
            ]}
         },
         {call, line(Var), 
            {remote,line(Var),{atom,line(Var),swirl},{atom,line(Var),x}},
            [key(Key),{var,0,'C'}]
         }
      ]
   }.

%%
%%
include(Key) ->
   {'case', line(Key),
      {call, line(Key), 
         {remote,line(Key),{atom,line(Key),swirl},{atom,line(Key),x}},
         [key(Key),{var,0,'C'}]
      },
      [
         {clause, line(Key),
            [{atom,line(Key),undefined}],
            [],
            [{nil,line(Key)}]
         },
%         {clause, line(Key),
%            [{var,line(Key),'X'}],
%            [[{call,line(Key),{atom,line(Key),is_list},[{var,line(Key),'X'}]}]], 
%            [{call, line(Key),
%               {remote,line(Key),{atom,line(Key),swirl},{atom,line(Key),r}},
%               [{var,line(Key),'X'},{var,line(Key),'C'}]
%            }]
%         },
         %%
         %% key value is atom, reference to template function  
         {clause, line(Key),
            [{var,line(Key),'X'}], 
            [[{call,line(Key),{atom,line(Key),is_atom},[{var,line(Key),'X'}]}]],
            [
               {match,line(Key),
                  {var,line(Key),'F'},
                  {call,line(Key),
                     {remote,line(Key),{atom,line(Key),swirl},{atom,line(Key),x}},
                     [{cons, 1, {atom,1,'>'},{var,line(Key),'X'}},{var,line(Key),'C'}]
                  }
               },
               {call,line(Key),{var,line(Key),'F'},[{var,line(Key),'C'}]}
            ]
         }
      ]
   }.
