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
         [sys()] ++ httpd()
      }
   }.

%%
%%
sys() ->
   {
      sys,
      {clue_sys, start_link, []},
      permanent, 60000, worker, dynamic
   }.

%%
%%
httpd() ->
   case opts:val(httpd, undefined, clue) of
      undefined -> 
         [];
      Port ->
         [{
            httpd,
            {clue_httpd_sup, start_link, [Port]},
            permanent, 60000, supervisor, dynamic
         }]
   end.



% %%
% clue_node() ->
%    [{
%       clue_node,
%       {clue_node, start_link, []},
%       permanent, 60000, worker, dynamic
%    }].

% %% local node, aggregates of statistic
% clue_node_tcp() ->
%    clue_node_tcp(opts:val(node, undefined, clue)).
% clue_node_tcp(undefined) ->
%    [];
% clue_node_tcp(Port) ->
%    [{
%       clue_node_tcp,
%       {clue_node_tcp, start_link, [Port]},
%       permanent, 60000, worker, dynamic
%    }].

% %% remote node, receives statistic
% clue_peer() ->
%    clue_peer(opts:val(peer, undefined, clue)).
% clue_peer(undefined) ->
%    [];
% clue_peer(Peer) ->
%    [{
%       Peer,
%       {clue_peer_tcp, start_link, [Peer, opts:val(sync, ?CLUE_SYNC, clue)]},
%       permanent, 60000, worker, dynamic
%    }].
