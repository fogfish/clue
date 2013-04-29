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
%%    clue tcp peer
-module(clue_peer_tcp).
-include("clue.hrl").

-export([
   start_link/2,
   init/1, free/2, ioctl/2, ping/2, handle/2
]).

%% internal state
-record(srv, {
   peer,   % peer to synchronize 
   sync    % sync timeout
}).

%%
%%
start_link(Peer, Sync) ->
   knet:connect([
      {knet_tcp, ?CLUE_TCP},
      {?MODULE,  [Peer, Sync]}
   ]).

init([Peer, Sync]) ->
   {ok, ping, 
      #srv{
         peer = Peer,
         sync = Sync
      },
      Sync
   }.

free(_, _) ->
   ok.

ioctl(_, _) ->
   undefined.

%%%----------------------------------------------------------------------------   
%%%
%%% ping
%%%
%%%----------------------------------------------------------------------------   

%% ping peer and establish connection 
ping(timeout, S) ->
   {reply, {connect, S#srv.peer}, ping, S};

ping({tcp, Peer, established}, S) ->
   ?DEBUG("clue tcp peer: established ~p", [Peer]),
   {next_state, handle, S, S#srv.sync};

ping({tcp, Peer, {error, Reason}}, S) ->
   ?DEBUG("clue tcp peer: terminated ~p ~p", [Peer, Reason]),
   {next_state, ping, S, S#srv.sync}.

%%%----------------------------------------------------------------------------   
%%%
%%% handle
%%%
%%%----------------------------------------------------------------------------   

handle(timeout, S) ->
   ?DEBUG("clue tcp peer: sync ~p", [S#srv.peer]),
   {reply, {send, S#srv.peer, synchronize(S)}, handle, S, S#srv.sync};

handle({tcp, Peer, terminated}, S) ->
   ?DEBUG("clue tcp peer: terminated ~p normal", [Peer]),
   {next_state, ping, S, S#srv.sync};

handle({tcp, Peer, {error, Reason}}, S) ->
   ?DEBUG("clue tcp peer: terminated ~p ~p", [Peer, Reason]),
   {next_state, ping, S, S#srv.sync}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

synchronize(S) ->
   clue:flush(
      fun({Key, Val}, Acc) ->
         [io_lib:format("~s:~p|g\r\n", [clue:lit(Key), Val]) | Acc]
      end,
      []
   ).

