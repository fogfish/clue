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
-module(clue_type_counter).
-include("clue.hrl").

-export([
   new/2,
   value/1,
   update/1
]).


%%
%%
-spec new(_, _) -> #clue{}.

new(Key, TTL) ->
   #clue{
      type      = ?MODULE
     ,key       = Key
     ,val       = 0
     ,time      = clue_type:usec()
     ,ttl       = clue_type:tinc(clue_type:usec(), TTL)
     ,state     = 0 
   }.

%%
%%
-spec value(#clue{}) -> {_, #clue{}}.

value(#clue{val = Val, ttl = infinity} = State) ->
   {false, Val, State};

value(#clue{val = Val, time = T, ttl = TTL, state = Last} = State) ->
   case os:timestamp() of
      %% TTL is not expired, current value is not flushed
      X when X < TTL ->
         {false, Val - Last, State};

      %% TTL is expired shift current value
      X ->
         NTTL = clue_type:tinc(X, timer:now_diff(TTL, T)),
         {true, Val - Last, State#clue{time = X, ttl = NTTL, state = Val}}
   end.

%%
%%
-spec update(#clue{}) -> [_].

update(#clue{time = T, ttl = NTTL, state = IState}) ->
   [{#clue.time, T}, {#clue.ttl, NTTL}, {#clue.state, IState}].
