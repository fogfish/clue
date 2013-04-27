
-define(CLUE_SYNC,  10000). 

%% clue entity
-record(clue, {
   type, %% entity type
   key,  %% entity id
   val,  %% entity raw value
   ext   %% entity specific extension
}).

