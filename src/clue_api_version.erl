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
%%    version of all running applications
-module(clue_api_version).
-author('Dmitry Kolesnikov <dmkolesnikov@gmail.com>').

-export([
   uri/0, 
   allowed_methods/1, content_provided/1, content_accepted/1,
   'GET'/3
]).

uri() ->
   {vsn,  "/_sys/vsn"}.

allowed_methods(_Uid)  -> 
   ['GET'].

content_provided(_Uid) -> 
   [
      {text, 'text/plain'}
   ].

content_accepted(_Uid) -> 
   [].

'GET'({vsn, _}, _Uri, _Heads) ->
   Text = lists:map(
      fun({Title, Desc, Vsn}) ->
         io_lib:format("~s-~s\t\t~s~n", [Title, Vsn, Desc])
      end,
      application:which_applications()
   ),
   {ok, list_to_binary(Text)}.

