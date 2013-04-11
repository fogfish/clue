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
   start/0,

   %% define entities
   counter/1, meter/1, blob/1, 
   
   %% counter api
   put/2, get/1, val/1,
   inc/1, inc/2, dec/1, dec/2, usec/2,

   %% 
   lookup/1
]).

%%
%% start application
start() ->
   applib:boot(?MODULE, []).

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
-spec(put/2 :: (any(), any()) -> ok).

put(Key, Val) ->
   case ets:update_element(clue, Key, {#clue.val, Val}) of
      true  -> ok;
      false -> undefined
   end.

%%
%% get value
-spec(get/1 :: (any()) -> any()).

get(#clue{type=counter, val=Val}) ->
   Val;

get(#clue{type=meter, key=Key, val=Val, ext={T, Last}}) ->
   R = (Val - Last) / (timer:now_diff(erlang:now(), T) / 1000000),
   _ = ets:update_element(clue, Key, {#clue.ext, {erlang:now(), Val}}),
   R;

get(#clue{type=blob, val=Val}) ->
   Val;

get(Key) ->
   case ets:lookup(clue, Key) of
      []  -> undefined;
      [E] -> clue:get(E)
   end.

%%
%% get raw counter value
-spec(val/1 :: (any()) -> any()).

val(Key) ->
   try
      ets:lookup_element(clue, Key, #clue.val)
   catch _:badarg ->
      undefined
   end.

%%
%% increment counter
-spec(inc/1 :: (any()) -> integer()).
-spec(inc/2 :: (any(), integer()) -> integer()).

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


%%
%% helper function to increment duration in usec
-spec(usec/2 :: (any(), any()) -> ok).

usec(Key, T) ->
   adb:inc(Key, timer:now_diff(erlang:now(), T)).

%%
%%
lookup(Key) ->
   [{element(#clue.key, X), clue:get(X)} || X <- ets:match_object(clue, {clue, '_', Key, '_', '_'})].

