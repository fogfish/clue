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
%%   @description
%%      light-weight simple http interface
-module(clue_httpd_sup).
-behaviour(supervisor).

-export([
   start_link/1, init/1
]).
-define(SO_TCP, [binary, {active, false}, {reuseaddr, true}]).



start_link(Port) ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]),
   supervisor:start_child(?MODULE, []),
   {ok, Sup}.


init([Port]) ->
   {ok, LSock} = gen_tcp:listen(Port, ?SO_TCP),
   {ok,
      {
         {simple_one_for_one, 1000, 1},
         [httpd(LSock)]
      }
   }.

httpd(LSock) ->
   {
      httpd,
      {clue_httpd, start_link, [LSock]},
      temporary, brutal_kill, worker, dynamic
   }.
