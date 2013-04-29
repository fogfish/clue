
%-define(VERBOSE, true).
-ifdef(VERBOSE).
-define(DEBUG(Str, Args), error_logger:error_msg(Str, Args)).
-else.
-define(DEBUG(Str, Args), ok).
-endif.

%%
%% default fall-back metric
-define(DEFAULT_METRIC(X, Y),  clue:gauge(X, Y)).

-define(CLUE_FLUSH,  10000). 
-define(CLUE_SYNC,   10000). 

-define(CLUE_TCP, [
   binary,
   {packet,  line},
   {recbuf,  128 * 1024},
   {sndbuf,  128 * 1024}
]).


%% clue metric
-record(clue, {
   type, %% metric type
   key,  %% metric id
   val,  %% metric raw value
   time  %% metric time stamp 
}).

