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

-export([
   start_link/0, init/1
]).

%%
%%
start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).

   
init([]) ->   
   {ok,
      {
         {one_for_one, 4, 1800},
         clue_node() ++ clue_peer()
      }
   }.

%%
%%
clue_node() ->
   clue_node(opts:val(port, undefined, clue)).
clue_node(undefined) ->
   [];
clue_node(native) ->
   [{
      clue_node,
      {clue_node_native, start_link, []},
      permanent, 60000, worker, dynamic
   }];
clue_node(Port)
 when is_number(Port) ->
   [{
      clue_node,
      {clue_node_tcp, start_link, [Port]},
      permanent, 60000, worker, dynamic
   }].


%%
%%
clue_peer() ->
   [{
      clue_peer,
      {clue_peer_sup, start_link, []},
      permanent, 60000, supervisor, dynamic
   }].


