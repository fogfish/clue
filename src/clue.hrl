
%-define(VERBOSE, true).
-ifdef(VERBOSE).
-define(DEBUG(Str, Args), error_logger:error_msg(Str, Args)).
-else.
-define(DEBUG(Str, Args), ok).
-endif.


-define(CLUE_SYNC,  10000). 
-define(CLUE_TCP, [
   binary,
   {packet,  line},
   {recbuf,  128 * 1024},
   {sndbuf,  128 * 1024}
]).


%% clue entity
-record(clue, {
   type, %% entity type
   key,  %% entity id
   val,  %% entity raw value
   ext   %% entity specific extension
}).

