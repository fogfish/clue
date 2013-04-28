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
%%    clue tcp node, implements line-based statsd protocol
-module(clue_node_tcp).
-include("clue.hrl").

-export([
   start_link/1,
   init/1, free/2, ioctl/2, handle/2
]).


%%
%%
start_link(Port) ->
   knet:start_link([
      {knet_tcp, [{addr, Port} | ?CLUE_TCP]},
      {?MODULE,  []}
   ]).

init(_) ->
   {ok, handle, undefined}.

free(_, _) ->
   ok.

ioctl(_, _) ->
   undefined.

%%
%%
handle({tcp, Peer, Msg}, S)
 when is_binary(Msg) ->
   try
      % this is text base protocol \r\n 
      message(Peer, binary:part(Msg, 0, byte_size(Msg) - 2)),
      {next_state, handle, S}
   catch _:_ ->
      {next_state, handle, S}
   end;

handle({tcp, Peer, established}, S) ->
   ?DEBUG("clue tcp node: established ~p", [Peer]),
   {next_state, handle, S};

handle({tcp, Peer, terminated}, S) ->
   ?DEBUG("clue tcp node: terminated ~p normal", [Peer]),
   {stop, normal, S};

handle({tcp, Peer, {error, Reason}}, S) ->
   ?DEBUG("clue tcp node: terminated ~p ~p", [Peer, Reason]),
   {stop, normal, S};

handle(_, S) ->
   {next_state, handle, S}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%% <metric name>:<value>|<type>
%%
%% <metric name>:<value>|g  - gauges
%% <metric name>:<value>|c[|@<sample rate>] - counter
%% <metric name>:<value>|ms - timers
message({Peer, _}, Msg) ->
   ?DEBUG("clue tcp: ~p ~s", [Peer, Msg]),
   case binary:split(Msg, [<<":">>, <<"|">>], [global, trim]) of
      [Metric, Value, <<$g>>] ->
         clue:put(metric_to_key(Peer, Metric), parser:scalar(Value));
      _ ->
         ok
   end.

metric_to_key(Peer, Metric) ->
   List = binary:split(Metric, [<<"_">>, <<"/">>], [global, trim]),
   Host = inet_parse:ntoa(Peer),
   list_to_tuple([Host | List]).




