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
   start_link/3,
   init/1, free/2, ioctl/2, ping/2, handle/2
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
   knet:connect([
      {knet_tcp, ?CLUE_TCP},
      {?MODULE,  [peer(Peer), Mask, Sync]}
   ]).

init([Peer, Mask, Sync]) ->
   {ok, ping, 
      #srv{
         peer = Peer,
         mask = Mask,
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
   % TODO: filter out any key originated by peer
   list_to_binary(
      lists:flatten(
         lists:map(
            fun({Key, Raw, _}) ->
               io_lib:format("~s:~p|g\r\n", [key_to_list(Key), Raw]) 
            end,
            clue:lookup(S#srv.mask)
         )
      )
   ).

   
key_to_list(Key)
 when is_tuple(Key) ->
   string:join([element_to_list(X) || X <- tuple_to_list(Key)], "_");
key_to_list(Key)
 when is_atom(Key) ->
   atom_to_list(Key);
key_to_list(Key)
 when is_list(Key) ->
   string:join([element_to_list(X) || X <- tuple_to_list(Key)], "_").

element_to_list(X)
 when is_list(X) ->
   X;
element_to_list(X)
 when is_atom(X) ->
   atom_to_list(X);

element_to_list(X)
 when is_binary(X) ->
   binary_to_list(X).


peer(Peer) ->
   [Host, Port] = string:tokens(Peer, ":"),
   {Host, parser:int(Port)}.

