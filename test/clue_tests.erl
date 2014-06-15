-module(clue_tests).
-include_lib("eunit/include/eunit.hrl").


init_test()  ->
	ok = clue:start().

gauge_test() ->
	Key = {clue, gauge, test},
	ok  = clue:define(gauge, Key),
	0   = clue:get(Key),
	10  = clue:put(Key, 10),
	11  = clue:inc(Key),
	21  = clue:inc(Key, 10),
	21  = clue:get(Key).

counter_inf_test() ->
	Key = {clue, counter, inf, test},
	ok  = clue:define(counter, Key),
	1   = clue:inc(Key),
	2   = clue:inc(Key),
	2   = clue:get(Key).

counter_ttl_test() ->
	Key = {clue, counter, ttl, test},
	ok  = clue:define(counter, Key, 500),
	10  = clue:inc(Key, 10),
	timer:sleep(400),
	10  = clue:get(Key),
	timer:sleep(100),
	10  = clue:get(Key),
	0   = clue:get(Key),
	11  = clue:inc(Key),
	1   = clue:get(Key).


meter_inf_test() ->
	Key = {clue, meter, inf, test},
	ok  = clue:define(meter, Key),
	timer:sleep(100),
	10  = clue:put(Key, 10),
	?assert(100 > clue:get(Key)),
	?assert( 90 < clue:get(Key)),
	timer:sleep(100),
	?assert( 50 > clue:get(Key)),
	?assert( 40 < clue:get(Key)).

meter_test() ->
	Key = {clue, meter, ttl, test},
	ok  = clue:define(meter, Key, 500),
	timer:sleep(100),
	10  = clue:put(Key, 10),
	?assert(100 > clue:get(Key)),
	timer:sleep(400),
	?assert(  0 < clue:get(Key)),	
	0.0 = clue:get(Key).






