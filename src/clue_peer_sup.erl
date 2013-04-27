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
-module(clue_peer_sup).
-behaviour(supervisor).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-include("clue.hrl").
-export([
   start_link/0, init/1
]).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   lists:foreach(fun native/1, opts:val(peer, [], clue)),
   {ok, Sup}.

init([]) ->   
   {ok,
      {
         {one_for_one, 4, 1800},
         []
      }
   }.

%%
%%
native({Peer, Mask})
 when is_atom(Peer) ->
   {ok, _} = supervisor:start_child(?MODULE, {
      Peer,
      {clue_peer_native, start_link, [Peer, Mask, opts:val(sync, ?CLUE_SYNC, clue)]},
      permanent, 60000, worker, dynamic
   });

native(Peer)
 when is_atom(Peer) ->
   {ok, _} = supervisor:start_child(?MODULE, {
      Peer,
      {clue_peer_native, start_link, [Peer, '_',  opts:val(sync, ?CLUE_SYNC, clue)]},
      permanent, 60000, worker, dynamic
   }).


