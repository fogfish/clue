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
-module(clue_sup).
-behaviour(supervisor).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include("clue.hrl").
-export([
   start_link/0, 
   init/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   {ok,   _} = logger(),
   {ok, Sup}.

   
init([]) ->
   {ok,
      {
         {one_for_one, 4, 1800},
         [
            ?CHILD(supervisor, clue_sensor_sup)
         ]
      }
   }.

%%
%%
logger() ->
   T = application:get_env(clue, logger, 0),
   supervisor:start_child(clue_sensor_sup, [clue_logger, T, fun logger/1, []]).

logger(Report) ->
   log([X || {_, Value} = X <- Report, Value > 0]).

log([]) ->
   ok;
log(Report) ->
   Now = tempus:encode(os:timestamp()),
   error_logger:info_msg("~n~n====[ ~s ]====", [Now]),
   lists:foreach(
      fun(X) -> error_logger:info_report([X]) end,
      Report
   ).
