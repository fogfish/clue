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
-module(clue_api).

-export([
   uri/0, 
   allowed_methods/1, content_provided/1, content_accepted/1,
   'GET'/3
]).

uri() ->
   [
      {clue,  "/_sys/clue/_"},
      {all,   "/_sys/clue"}
   ].

allowed_methods(_Uid)  -> 
   ['GET'].

content_provided(_Uid) -> 
   [
      {json, 'application/json'}
   ].

content_accepted(_Uid) -> 
   [].

%%
%%
'GET'({clue, _}, Uri, _Heads) ->
   [<<"_sys">>, <<"clue">> | Id] = uri:get(segments, Uri),
   Key  = [binary_to_existing_atom(X, utf8) || X <- Id],
   Json = to_json(Uri, clue:lookup(all, list_to_tuple(Key))),
   {ok, jsx:to_json(Json)};
 
'GET'({all,  _}, Uri, _Heads) ->
   Json = to_json(Uri, clue:lookup(all, '_')),
   {ok, jsx:to_json(Json)}.

%%
%%
to_json(Uri, List) ->
   [{uri(Uri, K), V} || {K, V} <- List].

%%
%% convert counter value to urn
uri(Uri, Key)
 when is_tuple(Key) ->
   uri(Uri, tuple_to_list(Key));

uri(Uri, Key)
 when is_list(Key) ->
   Path = [<<"_sys">>, <<"clue">> | [list_to_binary(format:scalar(X)) || X <- Key]],
   uri:to_binary(
      uri:set(segments, Path, Uri)
   );

uri(Uri, Key)
 when is_atom(Key) ->
   uri(Uri, [Key]).



