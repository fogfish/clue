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
-module(clue_node_native).
-behaviour(gen_server).

-export([
   start_link/0,
   % gen_server
   init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3
]).


%%
%%
start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
   {ok, undefined}.

terminate(_, _) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% gen_server
%%%
%%%----------------------------------------------------------------------------   

%%
%%
handle_call(_, _, S) ->
   {noreply, S}.

%%
%%
handle_cast(_, S) ->
   {noreply, S}.

%%
%% 
handle_info({clue, Key, Val}, S) ->
   clue:put(Key, Val),
   {noreply, S};

handle_info(_, S) ->
   {noreply, S}.

%%
%%
code_change(_Vsn, S, _Extra) ->
   {ok, S}.   



