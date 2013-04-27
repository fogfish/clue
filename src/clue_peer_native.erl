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
%%   Erlang clue peer (native distribution protocol)
-module(clue_peer_native).
-behaviour(gen_server).

-export([
   start_link/3,
   % gen_server
   init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3
]).

%% internal state
-record(srv, {
   peer,   % peer to synchronize 
   mask,   % key mask to synchronize
   sync    % sync timeout
}).

%%
%%
start_link(Peer, Mask, Sync) ->
   gen_server:start_link(?MODULE, [Peer, Mask, Sync], []).

init([Peer, Mask, Sync]) ->
   {ok, 
      #srv{
         peer = Peer,
         mask = Mask,
         sync = Sync
      },
      Sync
   }.

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
handle_info(timeout, S) ->
   synchronize(S),
   {noreply, S, S#srv.sync};

handle_info(_, S) ->
   {noreply, S}.

%%
%%
code_change(_Vsn, S, _Extra) ->
   {ok, S}.   


synchronize(S) ->
   Node = erlang:node(),
   List = lists:map(
      fun({Key, Raw, _}) -> {clue, list_to_tuple([Node | key_to_list(Key)]), Raw} end,
      clue:lookup(S#srv.mask)
   ),
   [erlang:send({clue_node_native, S#srv.peer}, X) || X <- List].

   
key_to_list(Key)
 when is_tuple(Key) ->
   tuple_to_list(Key);
key_to_list(Key)
 when is_atom(Key) ->
   [Key];
key_to_list(Key)
 when is_list(Key) ->
   Key.



