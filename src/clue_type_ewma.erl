%%
%%   Copyright (c) 2012, Dmitry Kolesnikov
%%   All Rights Reserved.
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
%%
-module(clue_type_ewma).
-include("clue.hrl").

-export([
   new/3,
   value/1,
   update/1
]).


%%
%%
-spec new(_, _, _) -> #clue{}.

new(A, Key, TTL) ->
   #clue{
      type      = ?MODULE
     ,key       = Key
     ,val       = 0
     ,time      = clue_type:usec()
     ,ttl       = clue_type:tinc(clue_type:usec(), TTL)
     ,state     = {A, 0}
   }.

%%
%%
-spec value(#clue{}) -> {_, #clue{}}.

value(#clue{val=Val, time=T, state = {W, Last}, ttl = infinity} = State) ->
   N = os:timestamp(),
   case timer:now_diff(N, T) div 1000000 of
      0 ->
         {false, Last, State};

      I ->
         A = math:exp(-I / W),
         Q = Val / I,
         DVal = Q + A * (Last - Q),
         {true, DVal, State#clue{val = 0, time = N, state = {W, DVal}}}
   end.

%%
%%
-spec update(#clue{}) -> [_].

update(#clue{time = T, state = IState, val = IVal}) ->
   [{#clue.time, T}, {#clue.state, IState}, {#clue.val, IVal}].
