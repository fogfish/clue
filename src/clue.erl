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
   define/2, 
   define/3,
   %% update counter
   put/2, 
   put/3, 
   putt/4,
   get/1, 
   get/2, 
   inc/1, 
   inc/2, 
   inc/3,
   dec/1, 
   dec/2, 
   dec/3,
   %% query counters
   prefix/1, 
   lookup/1, 
   fold/2,
   %% utility
   usec/2%, key/1, key/2, lit/1, lit/2
]).

%%
%%
-type(metric() :: gauge | counter | meter | measure).
-type(key()    :: tuple()).
-type(ttl()    :: timeout()).

%%
%% start application
start() -> 
   applib:boot(?MODULE, []).

%%
%% define new metric / reset existed
-spec(define/2 :: (metric(), key()) -> ok).
-spec(define/3 :: (metric(), key(), ttl()) -> ok).

define(Type, Key) ->
   _ = ets:insert(clue, metric(Type, Key, infinity)),
   ok.
define(Type, Key, TTL) ->
   _ = ets:insert(clue, metric(Type, Key, TTL * 1000)),
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% setters / getters
%%%
%%%----------------------------------------------------------------------------   

%%
%% get metric value
-spec(get/1 :: (any()) -> any()).
-spec(get/2 :: (node(), any()) -> any()).

get(#clue{type=gauge, val=Val}) ->
   Val;


get(#clue{type=counter, val=Val, ttl=infinity}) ->
   Val;

get(#clue{type=counter, key=Key, val=Val, time=T, ttl=TTL, state=Last}) ->
   case os:timestamp() of
      %% TTL is not expired, current value is not flushed
      X when X < TTL ->
         sub(Val, Last);
      %% TTL is expired shift current value
      X ->
         NTTL = tinc(X, timer:now_diff(TTL, T)),
         ets:update_element(clue, Key, [{#clue.time, X}, {#clue.ttl, NTTL}, {#clue.state, Val}]),
         sub(Val, Last)
   end;


get(#clue{type=meter, val=Val, time=T, ttl=infinity}) ->
   val(Val) / (timer:now_diff(os:timestamp(), T) / 1000000);

get(#clue{type=meter, key=Key, val=Val, time=T, ttl=TTL}) ->
   case os:timestamp() of
      X when X < TTL ->
         val(Val) / (timer:now_diff(X, T) / 1000000);
      X ->
         NTTL = tinc(X, timer:now_diff(TTL, T)),
         ets:update_element(clue, Key, [{#clue.val, 0}, {#clue.time, X}, {#clue.ttl, NTTL}]),
         val(Val) / (timer:now_diff(X, T) / 1000000)
   end;


get(#clue{type=measure, val=Val,  time=T, ttl=infinity}) ->
   Val / (timer:now_diff(os:timestamp(), T) / 1000000);

get(#clue{type=measure, key=Key, val=Val, time=T, ttl=TTL, state=Last}) ->
   case os:timestamp() of
      %% TTL is not expired, current value is not flushed
      X when X < TTL ->
         sub(Val, Last) / (timer:now_diff(X, T) / 1000000);
      %% TTL is expired shift current value
      X ->
         NTTL = tinc(X, timer:now_diff(TTL, T)),
         ets:update_element(clue, Key, [{#clue.time, X}, {#clue.ttl, NTTL}, {#clue.state, Val}]),
         sub(Val, Last) / (timer:now_diff(X, T) / 1000000)
   end;


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
%% put value / reset counter to initial state 
%% return counter value 
-spec(put/2  :: (any(), any()) -> any()).
-spec(put/3  :: (node(), any(), any()) -> any()).

put(Key, Val)
 when is_atom(Key) orelse is_tuple(Key) ->
   case ets:update_element(clue, Key, {#clue.val, Val}) of
      true  -> 
         Val;
      false -> 
         ?DEFAULT_METRIC(Key), 
         clue:put(Key, Val)
   end;

put(Key, Val)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:put(X, Val) end, Key),
   Val.

put(Node, Key, Val) ->
   rpc:cast(Node, clue, put, [Key, Val]).

%%
%% define and put value  
-spec(putt/4  :: (metric(), key(), any(), ttl()) -> any()).

putt(Type, Key, Val, TTL)
 when is_atom(Key) orelse is_tuple(Key) ->
   case ets:update_element(clue, Key, {#clue.val, Val}) of
      true  -> 
         Val;
      false -> 
         clue:define(Type, Key, TTL),
         clue:put(Key, Val)
   end;

putt(Type, Key, Val, TTL)
 when is_list(Key) ->
   lists:foreach(fun(X) -> clue:putt(Type, X, Val, TTL) end, Key),
   Val.



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
-spec(lookup/1 :: (any()) -> list()).

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
-spec(fold/2 :: (function(), any()) -> any()).

fold(Fun, Acc0) ->
   ets:foldl(
      fun(X, Acc) -> Fun({erlang:element(#clue.key, X), clue:get(X)}, Acc) end,
      Acc0,
      clue
   ).

%%
%% helper function to increment duration in usec
-spec(usec/2 :: (any(), any()) -> ok).

usec(Key, T) ->
   clue:inc(Key, timer:now_diff(os:timestamp(), T)).



% %%
% %% 
% key(Key)
%  when is_binary(Key) ->
%    list_to_tuple(parse_key(Key)).

% key(Prefix, Key)
%  when is_binary(Key) ->
%    list_to_tuple([Prefix | parse_key(Key)]).

% parse_key(Key) ->
%    lists:map(
%       fun(<<$*>>) -> '_'; (X) -> X end,
%       binary:split(Key, [<<"_">>, <<"/">>, <<".">>], [global, trim])
%    ).


% lit(Key)
%  when is_tuple(Key) ->
%    [H | Tail] = tuple_to_list(Key),
%    List = [format:scalar(H) | [ [$/, format:scalar(X)] || X <- Tail] ],
%    list_to_binary(List);

% lit(Key)
%  when is_atom(Key) ->
%    atom_to_binary(Key, utf8).

% lit(Prefix, Key)
%  when is_tuple(Key) ->
%    List = [format:scalar(Prefix) | [ [$/, format:scalar(X)] || X <- tuple_to_list(Key)] ],
%    list_to_binary(List);

% lit(Prefix, Key)
%  when is_atom(Key) ->
%    list_to_binary(
%       [format:scalar(Prefix), atom_to_binary(Key, utf8)]
%    ).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% create new metric
metric(Type, Key, TTL) ->
   #clue{
      type  = Type,
      key   = Key,
      val   = undefined,
      time  = usec(),
      ttl   = tinc(usec(), TTL),
      state = undefined 
   }.

usec() ->
   os:timestamp().

%%
%% sum time stamps
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
%% sub  
sub(_, undefined) ->
   0;
sub(X, Y) ->
   X - Y.

%%
%% maybe val
val(undefined) ->
   0;
val(X) ->
   X.



