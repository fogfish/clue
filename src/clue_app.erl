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
-module(clue_app).
-behaviour(application).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include("clue.hrl").

-export([
   start/2
  ,stop/1
]).

start(_Type, _Args) ->
   create_db(),
   define_db(),
   clue_sup:start_link().

stop(_State) ->
   ok.

%%
%% statistic and counter table
create_db() ->
   _  = ets:new(clue, [
      public
     ,named_table 
     ,ordered_set 
     ,{write_concurrency, true}
     ,{read_concurrency,  true}
     ,{keypos,       #clue.key}
   ]).

%%
%% define build-in counters
define_db() ->
   lists:foreach(
      fun({Type, Key}) -> clue:define(Type, Key); ({Type, Key, TTL}) -> clue:define(Type, Key, TTL) end,
      opts:val(sensors, [], clue)
   ).   





