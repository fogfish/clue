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
%%      clue dumps clue to std io
-module(clue_dump).

-export([
   start_link/0, init/1
]).

%%
%%
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
   proc_lib:init_ack(Parent, {ok, self()}),
   loop().

%%
%%
loop() ->
   timer:sleep(10000),
   io:format("------------~n"),
   [io:format("~p\t~p~n", [Key, Val]) || {Key, Val} <- clue:lookup('_')],
   loop().



