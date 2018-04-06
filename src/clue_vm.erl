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
%%   Virtual Machine statistics
-module(clue_vm).

-export([
   start_link/0,
   keys/0
]).

start_link() ->
   T    = application:get_env(clue, vm, 20000),
   Keys = keys(),
   lists:foreach(fun(X) -> clue:define(gauge, X) end, Keys),
   clue:spawn(T, fun update_vm_stats/1, Keys).

keys() ->
   [
      {vm, mem, total},
      {vm, mem, processes},
      {vm, mem, processes, used},
      {vm, mem, system},
      {vm, mem, atom},
      {vm, mem, atom, used},
      {vm, mem, binary},
      {vm, mem, code},
      {vm, mem, ets}
   ].

update_vm_stats(_) ->
   lists:foreach(fun update_vm_key/1, erlang:memory()).

update_vm_key({total, X}) ->
   clue:put({vm, mem, total}, X);

update_vm_key({processes, X}) ->
   clue:put({vm, mem, processes}, X);

update_vm_key({processes_used, X}) ->
   clue:put({vm, mem, processes, used}, X);

update_vm_key({system, X}) ->
   clue:put({vm, mem, system}, X);

update_vm_key({atom, X}) ->
   clue:put({vm, mem, atom}, X);

update_vm_key({atom_used, X}) ->
   clue:put({vm, mem, atom, used}, X);

update_vm_key({binary, X}) ->
   clue:put({vm, mem, binary}, X);

update_vm_key({code, X}) ->
   clue:put({vm, mem, code}, X);

update_vm_key({ets, X}) ->
   clue:put({vm, mem, ets}, X);

update_vm_key(_) ->
   ok.
