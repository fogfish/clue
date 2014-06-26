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
%%   alarm handler - substitute sasl default one
-module(clue_health_handler).

-export([start/0]).
-export([
   init/1
  ,terminate/2
  ,handle_event/2
  ,handle_call/2
  ,handle_info/2
]).

%% internal state
-record(health, {
   heap = undefined :: any() %% set of alarms
}).

%%
%% install health handler
start() ->
   gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {?MODULE, []}).


%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

%%
%%
init(_) -> 
   {ok, 
      #health{
         heap = dict:new()
      }
   }.

%%
%%
terminate(swap, State) ->
   {alarm_handler, dict:to_list(State#health.heap)};

terminate(_, _) ->
   ok.
    
%%%----------------------------------------------------------------------------   
%%%
%%% handler
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_event({set_alarm, {Id, Val}}, State) ->
   {ok, 
      State#health{
         heap = dict:store(Id, Val, State#health.heap)
      }
   };

handle_event({clear_alarm, Id}, State) ->
   {ok, 
      State#health{
         heap = dict:erase(Id, State#health.heap)
      }
   };

handle_event(_, State)->
    {ok, State}.

%%
%%
handle_info(_, State) -> 
   {ok, State}.

%%
%%
handle_call(get_alarms, State) -> 
   {ok, dict:to_list(State#health.heap), State};

handle_call({check_alarm, Id}, State) ->
   case dict:find(Id, State#health.heap) of
      error ->
         {ok, undefined, State};
      {ok, Val} ->
         {ok, Val, State}
   end;

handle_call(_, State) -> 
   {ok, badarg, State}.


