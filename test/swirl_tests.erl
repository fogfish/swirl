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


chars_1_test() -> 
   "text" = swirl:r("text", []).

chars_2_test() -> 
   "\\" = swirl:r("\\", []).

chars_3_test() -> 
   "\'" = swirl:r("\'", []).

char_4_test() ->
   "swirl\nerlang" = swirl:r("swirl\nerlang", []).


var_1_test() ->
   "erlang" = swirl:r("{lang}", [{lang, "erlang"}]).

var_2_test() ->
   "\\\'erlang" = swirl:r("\\\'{lang}", [{lang, "erlang"}]).

var_3_test() ->
   "erlang" = swirl:r("{l_ang}", [{l_ang, "erlang"}]).

var_4_test() ->
   "erlang" = swirl:r("{dev.lang}", [{dev, [{lang, "erlang"}]}]).


compile_test() ->
   ok = swirl:c(tpl,
   	"{abc} and {def} and {for x in num}{x}{/for}"
   ),
   List = [
      {abc, "abc"},
      {def, "def"},
      {num, [1,2,3]}
   ],
   ?assert(
   	"abc and def and 123" =:= tpl:render(List)
   ).


escape_1_test() ->
   "{lang}" = swirl:r("\\{lang}", []).

escape_2_test() ->
   "{bob.bloss}" = swirl:r("\\{bob.bloss}", []).

escape_3_test() ->
   "{>reindeer}" = swirl:r("\\{>reindeer}", []).

escape_4_test() ->
   "{for anger in mgmt}" = swirl:r("\\{for anger in mgmt}", []).

escape_5_test() ->
   "{/for}" = swirl:r("\\{/for}", []).

escape_6_test() ->
   "{if then}" = swirl:r("\\{if then}", []).

escape_7_test() ->
   "{if not now}" = swirl:r("\\{if not now}", []).

escape_8_test() ->
   "{else}" = swirl:r("\\{else}", []).

escape_9_test() ->
   "{/if}" = swirl:r("\\{/if}", []).


-define(FALSY,  [
   {false, false},
   {empty, ""},
   {null,  undefined},
   {zero,  0}
]).

falsy_1_test() ->
   "" = swirl:r("{false}", ?FALSY).

falsy_2_test() ->
   "" = swirl:r("{empty}", ?FALSY).

falsy_3_test() ->
   "" = swirl:r("{null}", ?FALSY).

falsy_4_test() ->
   "" = swirl:r("{undefined}", ?FALSY).

falsy_5_test() ->
   "" = swirl:r("{undefined.undefined}", ?FALSY).

falsy_6_test() ->
   "" = swirl:r("{if false}x{/if}", ?FALSY).

falsy_7_test() ->
   "" = swirl:r("{if empty}x{/if}", ?FALSY).

falsy_8_test() ->
   "" = swirl:r("{if null}x{/if}", ?FALSY).

falsy_9_test() ->
   "" = swirl:r("{if undefined}x{/if}", ?FALSY).

falsy_10_test() ->
   "" = swirl:r("{if undefined.undefined}x{/if}", ?FALSY).

falsy_11_test() ->
   "x" = swirl:r("{if not false}x{/if}", ?FALSY).

falsy_12_test() ->
   "x" = swirl:r("{if not empty}x{/if}", ?FALSY).

falsy_13_test() ->
   "x" = swirl:r("{if not null}x{/if}", ?FALSY).

falsy_14_test() ->
   "x" = swirl:r("{if not undefined}x{/if}", ?FALSY).

falsy_15_test() ->
   "x" = swirl:r("{if not undefined.undefined}x{/if}", ?FALSY).


for_1_test() ->
   "" = swirl:r("{for x in arr}{x}{/for}", []).

for_2_test() ->
   "123" = swirl:r("{for x in arr}{x}{/for}", [{arr, [1,2,3]}]).

for_3_test() ->
   "string" = swirl:r("{for x in arr}{x}{/for}", [{arr, ["string"]}]).

for_4_test() ->
   "" = swirl:r("{for x in arr}{x.y}{/for}", []).

for_5_test() ->
   "123" = swirl:r("{for x in arr}{x.y}{/for}", [{arr, [[{y,1}],[{y,2}],[{y,3}]]}]).

%for_6_test() ->
%      "123" = swirl:r("{for x in arr}{for y in x}{y.z}{/for}{/for}", [{arr, [[[{y,1}]],[[{y,2}]],[[{y,3}]]]}]).

if_1_test() ->
   "bar" = swirl:r("{if foo}{foo}{/if}", [{foo, "bar"}]).

if_2_test() ->
   "" = swirl:r("{if biz}{foo}{/if}", [{foo, "bar"}]).

if_3_test() ->
   "" = swirl:r("{if not foo}{foo}{/if}", [{foo, "bar"}]).

if_4_test() ->
   "bar" = swirl:r("{if not biz}{foo}{/if}", [{foo, "bar"}]).

if_5_test() ->
   "bar" = swirl:r("{if biz}blah{else}{foo}{/if}", [{foo, "bar"}]).


partials_1_test() ->
   "title: foo" = swirl:r("{>head}", [{head, "title: {title}"}, {title, "foo"}]).

partials_2_test() ->
   ok = swirl:c(head, "title: {title}"),
   "title: foo" = swirl:r("{>head}", [{head, head}, {title, "foo"}]).

partials_3_test() ->
   "" = swirl:r("{>head}", [{title, "foo"}]).
   


