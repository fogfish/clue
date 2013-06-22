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

   %% define counters
   define/2, define/3,
   gauge/1, gauge/2, 
   counter/1, counter/2, 
   meter/1, meter/2, 
   functor/2,

   %% update counter
   put/2, put/3, 
   get/1, get/2, 
   inc/1, inc/2, inc/3,
   dec/1, dec/2, dec/3,

   %% query counters
   prefix/1, lookup/1, fold/2,

   %% utility
   usec/2, key/1, key/2, lit/1, lit/2
]).

%%
%% start application
start()    -> applib:boot(?MODULE, []).
start(Cfg) -> applib:boot(?MODULE, Cfg).

%%
%% define 
-spec(define/2 :: (atom(), any()) -> ok).
-spec(define/3 :: (atom(), any(), any()) -> ok).

define(Type, Key) ->
   define(Type, Key, 0).
define(Type, Key, Val) ->
   _ = ets:insert(clue, metric(Type, Key, Val)),
   ok.

%%
%% instantaneous measurement of a value 
-spec(gauge/1   :: (any()) -> ok).
-spec(gauge/2   :: (any(), any()) -> ok).

gauge(Key) ->
   gauge(Key, 0).

gauge(Key, Val) ->
   _ = ets:insert(clue, metric(gauge, Key, Val)), 
   ok.

%%
%% metric value is incremented or decremented, reset to 0 at flush
-spec(counter/1 :: (any()) -> ok).
-spec(counter/2 :: (any(), any()) -> ok).

counter(Key) ->
   counter(Key, 0).

counter(Key, Val) ->
   _ = ets:insert(clue, metric(counter, Key, Val)),
   ok.

%%
%% measures the rate of events per second
-spec(meter/1   :: (any()) -> ok).
-spec(meter/2   :: (any(), any()) -> ok).

meter(Key) ->
   meter(Key, 0).

meter(Key, Val) ->
   _ = ets:insert(clue, metric(meter, Key, Val)),
   ok.

%%
%% dynamically calculated metric (e.g. wrapper to other subsystem) 
-spec(functor/2 :: (any(), function()) -> ok).

functor(Key, Fun) ->
   _ = ets:insert(clue, metric(functor, Key, Fun)),
   ok.

%%
%% put value / reset counter to initial state
-spec(put/2  :: (any(), any()) -> ok).
-spec(put/3  :: (node(), any(), any()) -> ok).

put(Key, Val)
 when is_atom(Key) orelse is_tuple(Key) ->
   case ets:update_element(clue, Key, {#clue.val, Val}) of
      true  -> ok;
      false -> ?DEFAULT_METRIC(Key, Val)
   end;

put(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:put(X, Val) end, Key).

put(Node, Key, Val) ->
   rpc:cast(Node, clue, put, [Key, Val]).

%%
%% get counter value
-spec(get/1 :: (any()) -> any()).
-spec(get/2 :: (node(), any()) -> any()).

get(#clue{type=gauge, val=Val}) ->
   Val;

get(#clue{type=counter, val=Val}) ->
   Val;

get(#clue{type=meter, key=Key, val=Val, time=T, ttl=TTL}) ->
   case erlang:now() of
      X when X < TTL ->
         Val / (timer:now_diff(erlang:now(), T) / 1000000);
      X ->
         New = tempus:inc(X, opts:val(ttl, ?CONFIG_TTL, clue)),
         ets:update_element(clue, Key, [{#clue.val, 0}, {#clue.time, X}, {#clue.ttl, New}]),
         Val / (timer:now_diff(erlang:now(), T) / 1000000)
   end;

get(#clue{type=functor, val=Fun}) 
 when is_function(Fun) ->
   Fun();

get(#clue{type=functor, val=Val}) ->
   Val;

get(Key)
 when is_atom(Key) orelse is_tuple(Key) ->
   case ets:lookup(clue, Key) of
      []  -> undefined;
      [E] -> clue:get(E)
   end;

get(Key)
 when is_list(Key) ->
   [clue:get(X) || X <- Key].

get(Node, Key) ->
   rpc:call(Node, clue, get, [Key]).

%%
%% increment counter
-spec(inc/1 :: (any()) -> integer()).
-spec(inc/2 :: (any(), integer()) -> integer()).
-spec(inc/3 :: (node(), any(), integer()) -> integer()).

inc(Key) ->
   inc(Key, 1).

inc(Key, Val)
 when is_atom(Key) orelse is_tuple(Key) ->
   try
      ets:update_counter(clue, Key, Val)
   catch _:badarg ->
      undefined
   end;

inc(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:inc(X, Val) end, Key).

inc(Node, Key, Val) ->
   rpc:cast(Node, clue, inc, [Key, Val]).

%%
%% decrement counter
-spec(dec/1 :: (any()) -> integer()).
-spec(dec/2 :: (any(), integer()) -> integer()).

dec(Key) ->
   dec(Key, 1).

dec(Key, Val)
 when is_atom(Key) orelse is_tuple(Key) ->
   try
      ets:update_counter(clue, Key, -Val)
   catch _:badarg ->
      undefined
   end;

dec(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:dec(X, Val) end, Key).

dec(Node, Key, Val) ->
   rpc:cast(Node, clue, dec, [Key, Val]).

%%
%% lookup key(s) based on prefix
-spec(prefix/1 :: (any()) -> list()).

prefix(Key)
 when is_tuple(Key) ->
   %% ensure that only keys partial match are selected 
   Prefix = lists:map(
      fun(X) ->
         {'=:=', {element, X, {element, #clue.key, '$1'}}, erlang:element(X, Key)}
      end,
      lists:seq(1, size(Key))
   ),
   Query  = [{
      '$1', 
      [{'>=', {element, #clue.key, '$1'}, {const, Key}} | Prefix ], 
      ['$_'] 
   }],
   case ets:select(clue, Query) of
      '$end_of_table' ->
         [];
      List ->
         [{erlang:element(#clue.key, X), clue:get(X)} || X <- List]
   end;

prefix(Key) ->
   prefix({Key}).

%%
%% lookup key(s) based on pattern
-spec(lookup/1 :: (any()) -> list()).

lookup(Key) ->
   Query  = [{
      {clue, '_', Key, '_', '_', '_'},
      [],
      ['$_']
   }],
   case ets:select(clue, Query) of
      '$end_of_table' ->
         [];
      List ->
         [{erlang:element(#clue.key, X), clue:get(X)} || X <- List]
   end.

%%
%% helper function to increment duration in usec
-spec(usec/2 :: (any(), any()) -> ok).

usec(Key, T) ->
   clue:inc(Key, timer:now_diff(erlang:now(), T)).


%%
%% fold function over dataset
-spec(fold/2 :: (function(), any()) -> any()).

fold(Fun, Acc0) ->
   ets:foldl(
      fun(X, Acc) -> Fun({erlang:element(#clue.key, X), clue:get(X)}, Acc) end,
      Acc0,
      clue
   ).

%%
%% 
key(Key)
 when is_binary(Key) ->
   list_to_tuple(parse_key(Key)).

key(Prefix, Key)
 when is_binary(Key) ->
   list_to_tuple([Prefix | parse_key(Key)]).

parse_key(Key) ->
   lists:map(
      fun(<<$*>>) -> '_'; (X) -> X end,
      binary:split(Key, [<<"_">>, <<"/">>, <<".">>], [global, trim])
   ).


lit(Key)
 when is_tuple(Key) ->
   [H | Tail] = tuple_to_list(Key),
   List = [format:scalar(H) | [ [$/, format:scalar(X)] || X <- Tail] ],
   list_to_binary(List);

lit(Key)
 when is_atom(Key) ->
   atom_to_binary(Key, utf8).

lit(Prefix, Key)
 when is_tuple(Key) ->
   List = [format:scalar(Prefix) | [ [$/, format:scalar(X)] || X <- tuple_to_list(Key)] ],
   list_to_binary(List);

lit(Prefix, Key)
 when is_atom(Key) ->
   list_to_binary(
      [format:scalar(Prefix), atom_to_binary(Key, utf8)]
   ).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%% create new metric
metric(Type, Key, Val) ->
   #clue{
      type = Type,
      key  = Key,
      val  = Val,
      time = erlang:now(),
      ttl  = tempus:inc(erlang:now(), opts:val(ttl, ?CONFIG_TTL, clue))
   }.






