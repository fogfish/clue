%%
-module(clue_type_meter_SUITE).

-export([all/0]).
-export([
   value/1,
   value_ttl/1
]).

all() ->
   [value, value_ttl].

value(_) ->
   clue:start(),
   Key = {t,e,s,t,0},
   clue:define(meter, Key),
   timer:sleep(100),
   10  = clue:put(Key, 10),
   gt(100, clue:get(Key)),
   lt(90, clue:get(Key)),
   timer:sleep(100),
   gt(50, clue:get(Key)),
   lt(40, clue:get(Key)).

value_ttl(_) ->
   clue:start(),
   Key = {t,e,s,t,1},
   clue:define(meter, Key, 500),
   timer:sleep(100),
   10  = clue:put(Key, 10),
   gt(100, clue:get(Key)),
   timer:sleep(400),
   lt(0.0, clue:get(Key)), 
   lt(0.0, clue:get(Key)).


gt(X, Val)
 when Val =< X ->
   ok;
gt(X, Val) ->
   exit({gt, X, Val}).

lt(X, Val)
 when Val >= X ->
   ok;
lt(X, Val) ->
   exit({lt, X, Val}).
