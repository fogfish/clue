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
%% @description
%%   system health api
-module(health).

-export([
   list/0
  ,check/1
  ,raise/1
  ,raise/2
  ,reset/1
]).

%%
%% list health status
-spec(list/0 :: () -> [{any(), any()}]).

list() ->
   gen_event:call(alarm_handler, clue_health_handler, get_alarms).

%%
%% check health
-spec(check/1 :: (any()) -> any() | undefined).

check(Id) ->
   gen_event:call(alarm_handler, clue_health_handler, {check_alarm, Id}).


%%
%% raise health issue
-spec(raise/1 :: (any()) -> ok).
-spec(raise/2 :: (any(), any()) -> ok).

raise(Id) ->
   raise(Id, os:timestamp()).

raise(Id, Val) ->
   alarm_handler:set_alarm({Id, Val}).

%%
%% reset health issue
-spec(reset/1 :: (any()) -> ok).

reset(Id) ->
   alarm_handler:clear_alarm(Id).

