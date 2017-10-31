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
%%   system status / statistic repository
%%    * gauge   - the simplest metric type, it just contain a value.
%%    * counter - monotonically increasing 64-bit integer.
%%    * meter   - reflects the rate at which a set of events occur
%%    * measure - measures are like a meter except that passed values are monotonically increasing values
%%                read from external sources e.g. OS/VM counters
-module(clue).
-include("clue.hrl").

-export([start/0]).
-export([
   %% data structure
   new/2,
   new/3,
   update/2,
   value/1,
   %% global stats table
   define/2, 
   define/3,
   put/2, 
   get/1, 
   inc/1, 
   inc/2, 
   dec/1, 
   dec/2, 
   %% query counters
   prefix/1, 
   lookup/1, 
   fold/2,
   %% utility
   log/1,
   usec/2,  % @deprecated
   t/2,
   tinc/2
]).

%%
%%
-type metric() :: gauge | counter | meter | measure | {decay, number()} | {ewma, number()}.
-type key()    :: tuple().
-type ttl()    :: timeout().

%%
%% start application
start() -> 
   applib:boot(?MODULE, []).

%%%----------------------------------------------------------------------------   
%%%
%%% data type
%%%
%%%----------------------------------------------------------------------------   

%%
%% create new counter data structure
-spec new(metric(), key()) -> #clue{}.
-spec new(metric(), key(), ttl()) -> #clue{}.

new(Type, Id) ->
   new(Type, Id, infinity).

new(gauge, Key, TTL) ->
   clue_type_gauge:new(Key, TTL);

new(counter, Key, TTL) ->
   clue_type_counter:new(Key, TTL);

new(meter, Key, TTL) ->
   clue_type_meter:new(Key, TTL);

new(measure, Key, TTL) ->
   clue_type_measure:new(Key, TTL);

new({decay, A}, Key, TTL) ->
   clue_type_decay:new(A, Key, TTL);

new({ewma, A}, Key, TTL) ->
   clue_type_ewma:new(A, Key, TTL).


%%
%% update counter
-spec update(number(), #clue{}) -> #clue{}.

update(X, #clue{val = Val} = State) ->
   State#clue{val = Val + X}.

%%
%% read counter value and apply aggregation 
-spec value(#clue{}) -> {number(), #clue{}}.

value(#clue{type = Type} = State) ->
   Type:value(State).


%%%----------------------------------------------------------------------------   
%%%
%%% global stats table
%%%
%%%----------------------------------------------------------------------------   


%%
%% define new metric / reset existed
%% time-to-live is defined in milliseconds
-spec define(metric(), key()) -> ok.
-spec define(metric(), key(), ttl()) -> ok.

define(Type, Key) ->
   _ = ets:insert_new(clue, new(Type, Key, infinity)),
   Key.
define(Type, Key, TTL) ->
   _ = ets:insert_new(clue, new(Type, Key, erlang:trunc(TTL * 1000))),
   Key.

%%
%% get metric value
-spec get(any()) -> any().

get(#clue{type = Type, key = Key} = State0) ->
   case value(State0) of
      {false, Value, _} ->
         Value;
      {true, Value, State1} ->
         ets:update_element(clue, Key, Type:update(State1)),
         Value
   end;

get(Key)
 when is_list(Key) ->
   [{X, clue:get(X)} || X <- Key];

get(Key) ->
   case ets:lookup(clue, Key) of
      []  -> undefined;
      [E] -> clue:get(E)
   end.

%%
%% put value / reset counter to initial state 
%% return counter value 
-spec put(any(), any()) -> any().

put(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:put(X, Val) end, Key),
   Val;

put(Key, Val) ->
   case ets:update_element(clue, Key, {#clue.val, Val}) of
      true  -> 
         Val;
      false -> 
         ?DEFAULT_METRIC(Key), 
         clue:put(Key, Val)
   end.


%%
%% increment counter
-spec inc(any()) -> integer().
-spec inc(any(), integer()) -> integer().

inc(Key) ->
   inc(Key, 1).

inc(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:inc(X, Val) end, Key);

inc(Key, Val) ->
   try
      ets:update_counter(clue, Key, Val)
   catch _:badarg ->
      undefined
   end.

%%
%% decrement counter
-spec dec(any()) -> integer().
-spec dec(any(), integer()) -> integer().

dec(Key) ->
   dec(Key, 1).

dec(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:dec(X, Val) end, Key);

dec(Key, Val) ->
   try
      ets:update_counter(clue, Key, -Val)
   catch _:badarg ->
      undefined
   end.

%%
%% lookup key(s) based on prefix
-spec prefix(any()) -> list().

prefix(Key)
 when is_tuple(Key) ->
   %% ensure that only key partial match is selected 
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
-spec lookup(any()) -> list().

lookup(Key) ->
   Query  = [{
      {clue, '_', Key, '_', '_', '_', '_'},
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
%% fold function over dataset
-spec fold(function(), any()) -> any().

fold(Fun, Acc0) ->
   ets:foldl(
      fun(X, Acc) -> Fun({erlang:element(#clue.key, X), clue:get(X)}, Acc) end,
      Acc0,
      clue
   ).

%%
%%
log(Key) ->
   gen_server:call(clue_logger, {log, Key}).


%%
%% helper function to increment duration in usec
-spec usec(any(), any()) -> ok.
-spec t(any(), any()) -> ok.

usec(Key, T) ->
   clue:inc(Key, timer:now_diff(os:timestamp(), T)).

t(Key, T) ->
   clue:inc(Key, timer:now_diff(os:timestamp(), T)).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
usec() ->
   os:timestamp().

%%
%% add time
tinc(_, infinity) ->
   infinity;
tinc({Msec, Sec, Usec}, T)
 when is_integer(T) ->
   case Usec + T of
      X when X =< 1000000 ->
         {Msec, Sec, X};
      X when X =< 1000000 * 1000000 ->
         {Msec, Sec + (X div 1000000), X rem 1000000};
      X ->
         {Msec + (X div (1000000 * 1000000)), Sec + (X div 1000000), X rem 1000000}
   end.

%%
%%
diff(T) ->
   diff(os:timestamp(), T).

diff(A, B) ->
   (timer:now_diff(A, B) / 1000000).
