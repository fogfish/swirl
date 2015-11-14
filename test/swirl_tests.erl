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
-module(swirl_tests).
-include_lib("eunit/include/eunit.hrl").

%%
%%
chars_1_test() -> 
   <<"text">> = swirl:apply(<<"text">>, []).

chars_2_test() -> 
   <<"\\">> = swirl:apply(<<"\\">>, []).

chars_3_test() -> 
   <<"\'">> = swirl:apply(<<"\'">>, []).

char_4_test() ->
   <<"swirl\nerlang">> = swirl:apply(<<"swirl\nerlang">>, []).


%%
%%
var_1_test() ->
   <<"erlang">> = swirl:apply(<<"{lang}">>, [{lang,  "erlang"}]),
   <<"erlang">> = swirl:apply(<<"{lang}">>, #{lang =>"erlang"}).

var_2_test() ->
   <<"\\\'erlang">> = swirl:apply(<<"\\\'{lang}">>, [{lang, "erlang"}]).

var_3_test() ->
   <<"erlang">> = swirl:apply("{l_ang}", [{l_ang, "erlang"}]).

var_4_test() ->
   <<"erlang">> = swirl:apply("{dev.lang}", [{dev, [{lang, "erlang"}]}]).

% @todo: module
% compile_test() ->
%    ok = swirl:c(tpl,
%    	"{abc} and {def} and {for x in num}{x}{/for}"
%    ),
%    List = [
%       {abc, "abc"},
%       {def, "def"},
%       {num, [1,2,3]}
%    ],
%    ?assert(
%    	"abc and def and 123" =:= tpl:render(List)
%    ).

%%
%%
escape_1_test() ->
   <<"{lang}">> = swirl:apply(<<"\\{lang}">>, []).

escape_2_test() ->
   <<"{bob.bloss}">> = swirl:apply(<<"\\{bob.bloss}">>, []).

escape_3_test() ->
   <<"{>reindeer}">> = swirl:apply(<<"\\{>reindeer}">>, []).

escape_4_test() ->
   <<"{for anger in mgmt}">> = swirl:apply(<<"\\{for anger in mgmt}">>, []).

escape_5_test() ->
   <<"{/for}">> = swirl:apply(<<"\\{/for}">>, []).

escape_6_test() ->
   <<"{if then}">> = swirl:apply(<<"\\{if then}">>, []).

escape_7_test() ->
   <<"{if not now}">> = swirl:apply(<<"\\{if not now}">>, []).

escape_8_test() ->
   <<"{else}">> = swirl:apply(<<"\\{else}">>, []).

escape_9_test() ->
   <<"{/if}">> = swirl:apply(<<"\\{/if}">>, []).


%%
%%
-define(FALSY,  [
   {false, false},
   {empty, ""},
   {null,  undefined},
   {zero,  0}
]).

falsy_1_test() ->
   <<"false">> = swirl:apply(<<"{false}">>, ?FALSY).

falsy_2_test() ->
   <<>> = swirl:apply(<<"{empty}">>, ?FALSY).

falsy_3_test() ->
   <<>> = swirl:apply(<<"{null}">>, ?FALSY).

falsy_4_test() ->
   <<>> = swirl:apply(<<"{undefined}">>, ?FALSY).

falsy_5_test() ->
   <<>> = swirl:apply(<<"{undefined.undefined}">>, ?FALSY).

falsy_6_test() ->
   <<>> = swirl:apply(<<"{if false}x{/if}">>, ?FALSY).

falsy_7_test() ->
   <<>> = swirl:apply(<<"{if empty}x{/if}">>, ?FALSY).

falsy_8_test() ->
   <<>> = swirl:apply(<<"{if null}x{/if}">>, ?FALSY).

falsy_9_test() ->
   <<>> = swirl:apply(<<"{if undefined}x{/if}">>, ?FALSY).

falsy_10_test() ->
   <<>> = swirl:apply(<<"{if undefined.undefined}x{/if}">>, ?FALSY).

falsy_11_test() ->
   <<"x">> = swirl:apply(<<"{if not false}x{/if}">>, ?FALSY).

falsy_12_test() ->
   <<"x">> = swirl:apply(<<"{if not empty}x{/if}">>, ?FALSY).

falsy_13_test() ->
   <<"x">> = swirl:apply(<<"{if not null}x{/if}">>, ?FALSY).

falsy_14_test() ->
   <<"x">> = swirl:apply(<<"{if not undefined}x{/if}">>, ?FALSY).

falsy_15_test() ->
   <<"x">> = swirl:apply(<<"{if not undefined.undefined}x{/if}">>, ?FALSY).

%%
%%
for_1_test() ->
   <<"">> = swirl:apply(<<"{for x in arr}{x}{/for}">>, []).

for_2_test() ->
   <<"123">> = swirl:apply(<<"{for x in arr}{x}{/for}">>, [{arr, [1,2,3]}]).

for_3_test() ->
   <<"string">> = swirl:apply(<<"{for x in arr}{x}{/for}">>, [{arr, ["s","t","r","i","n","g"]}]).

for_4_test() ->
   <<"">> = swirl:apply(<<"{for x in arr}{x.y}{/for}">>, []).

for_5_test() ->
   <<"123">> = swirl:apply(<<"{for x in arr}{x.y}{/for}">>, [{arr, [[{y,1}],[{y,2}],[{y,3}]]}]).

for_6_test() ->
     <<"123">> = swirl:apply("{for x in arr}{for y in x}{y.z}{/for}{/for}", [{arr, [[[{z,1}]],[[{z,2}]],[[{z,3}]]]}]).

%%
%%
if_1_test() ->
   <<"bar">> = swirl:apply(<<"{if foo}{foo}{/if}">>, [{foo, "bar"}]).

if_2_test() ->
   <<"">> = swirl:apply(<<"{if biz}{foo}{/if}">>, [{foo, "bar"}]).

if_3_test() ->
   <<"">> = swirl:apply(<<"{if not foo}{foo}{/if}">>, [{foo, "bar"}]).

if_4_test() ->
   <<"bar">> = swirl:apply(<<"{if not biz}{foo}{/if}">>, [{foo, "bar"}]).

if_5_test() ->
   <<"bar">> = swirl:apply(<<"{if biz}blah{else}{foo}{/if}">>, [{foo, "bar"}]).

%%
%%
partials_1_test() ->
   <<"title: foo">> = swirl:apply(<<"{>head}">>, [{head, "title: {title}"}, {title, "foo"}]).

partials_2_test() ->
   Fun = swirl:f(#{
      main => swirl:f("{>head}"),
      body => swirl:f("title: {title}")
   }),
   <<"title: foo">> = swirl:apply(Fun, main, [
      {main, [{head, body}]}, 
      {body, [{title, "foo"}]}
   ]).

partials_3_test() ->
   <<"">> = swirl:apply(<<"{>head}">>, [{title, "foo"}]).
   
partials_4_test() ->
   Fun = swirl:f(#{
      main => swirl:f("{>head}"),
      body => swirl:f("{for x in list}{x}{/for}")
   }),
   <<"123">> = swirl:apply(Fun, main, #{
      main => #{head => body},
      body => #{list => [1,2,3]}
   }).



