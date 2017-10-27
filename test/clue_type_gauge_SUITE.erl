%%
-module(clue_type_gauge_SUITE).

-export([all/0]).
-export([
   value/1
]).

all() ->
   [value].

value(_) ->
   clue:start(),
   clue:define(gauge, {t,e,s,t}),
   10 = clue:put({t,e,s,t}, 10),
   10 = clue:get({t,e,s,t}).
