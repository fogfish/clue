%%
-module(clue_type_counter_SUITE).

-export([all/0]).
-export([
   value/1,
   value_ttl/1
]).

all() ->
   [value, value_ttl].

value(_) ->
   clue:start(),
   clue:define(counter, {t,e,s,t}),
   1  = clue:inc({t,e,s,t}),
   2  = clue:inc({t,e,s,t}),
   12 = clue:inc({t,e,s,t}, 10),
   12 = clue:get({t,e,s,t}).

value_ttl(_) ->
   clue:start(),
   clue:define(counter, {t,e,s,t,1}, 10),
   1 = clue:inc({t,e,s,t,1}),
   2 = clue:inc({t,e,s,t,1}),
   2 = clue:get({t,e,s,t,1}),

   timer:sleep(100),
   2 = clue:get({t,e,s,t,1}),

   3 = clue:inc({t,e,s,t,1}),
   4 = clue:inc({t,e,s,t,1}),
   2 = clue:get({t,e,s,t,1}).
