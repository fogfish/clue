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
-module(clue).
-include("clue.hrl").

-export([
   start/0, start/1,

   %% define entities
   counter/1, meter/1, blob/1, 
   
   %% counter api
   put/2, put/3, put_/3, 
   get/1, get/2, val/1, val/2,
   inc/1, inc/2, inc/3,
   dec/1, dec/2, dec/3,

   usec/2,

   %% 
   lookup/1
]).

%%
%% start application
start()    -> applib:boot(?MODULE, []).
start(Cfg) -> applib:boot(?MODULE, Cfg).


%%
%% create counter
-spec(counter/1 :: (any()) -> ok).
-spec(meter/1   :: (any()) -> ok).
-spec(blob/1    :: (any()) -> ok).

counter(Key) ->
   _ = ets:insert(clue, 
      #clue{
         type = counter,
         key  = Key, 
         val  = 0
      }
   ),
   ok.

meter(Key) ->
   _ = ets:insert(clue, 
      #clue{
         type = meter,
         key  = Key, 
         val  = 0, 
         ext  = {erlang:now(), 0}
      }
   ),
   ok.

blob(Key) ->
   _ = ets:insert(clue, 
      #clue{
         type = blob,
         key  = Key, 
         val  = <<>>
      }
   ),
   ok.

%%
%% put value
-spec(put/2  :: (any(), any()) -> ok).
-spec(put/3  :: (node(), any(), any()) -> ok).
-spec(put_/3 :: (node(), any(), any()) -> ok).

put(Key, Val) ->
   case ets:update_element(clue, Key, {#clue.val, Val}) of
      true  -> 
         ok;
      false -> 
         % unable to update, key not found, fall-back to counter
         counter(Key),
         ets:update_element(clue, Key, {#clue.val, Val}),
         ok
   end.

put(Node, Key, Val) ->
   rpc:call(Node, clue, put, [Key, Val]).

put_(Node, Key, Val) ->
   rpc:cast(Node, clue, put, [Key, Val]).

%%
%% get value
-spec(get/1 :: (any()) -> any()).
-spec(get/2 :: (node(), any()) -> any()).

get(#clue{type=counter, val=Val}) ->
   Val;

get(#clue{type=meter, key=Key, val=Val, ext={T, Last}}) ->
   R = (Val - Last) / (timer:now_diff(erlang:now(), T) / 1000000),
   %% ??? do update once 60 sec
   _ = ets:update_element(clue, Key, {#clue.ext, {erlang:now(), Val}}),
   R;

get(#clue{type=blob, val=Val}) ->
   Val;

get(Key) ->
   case ets:lookup(clue, Key) of
      []  -> undefined;
      [E] -> clue:get(E)
   end.

get(Node, Key) ->
   rpc:call(Node, clue, get, [Key]).

%%
%% get raw counter value
-spec(val/1 :: (any()) -> any()).

val(#clue{val=Val}) ->
   Val;

val(Key) ->
   try
      ets:lookup_element(clue, Key, #clue.val)
   catch _:badarg ->
      undefined
   end.

val(Node, Key) ->
   rpc:call(Node, clue, val, [Key]).

%%
%% increment counter
-spec(inc/1 :: (any()) -> integer()).
-spec(inc/2 :: (any(), integer()) -> integer()).
-spec(inc/3 :: (node(), any(), integer()) -> integer()).

inc(Key) ->
   try
      ets:update_counter(clue, Key, 1)
   catch _:badarg ->
      undefined
   end.

inc(Key, Val) ->
   try
      ets:update_counter(clue, Key, Val)
   catch _:badarg ->
      undefined
   end.

inc(Node, Key, Val) ->
   rpc:call(Node, clue, inc, [Key, Val]).

%%
%% decrement counter
-spec(dec/1 :: (any()) -> integer()).
-spec(dec/2 :: (any(), integer()) -> integer()).

dec(Key) ->
   try
      ets:update_counter(clue, Key, -1)
   catch _:badarg ->
      undefined
   end.

dec(Key, Val) ->
   try
      ets:update_counter(clue, Key, -Val)
   catch _:badarg ->
      undefined
   end.

dec(Node, Key, Val) ->
   rpc:call(Node, clue, dec, [Key, Val]).


%%
%% helper function to increment duration in usec
-spec(usec/2 :: (any(), any()) -> ok).

usec(Key, T) ->
   clue:inc(Key, timer:now_diff(erlang:now(), T)).

%%
%%
lookup(Key) ->
   [{element(#clue.key, X), clue:val(X), clue:get(X)} || X <- ets:match_object(clue, {clue, '_', Key, '_', '_'})].
