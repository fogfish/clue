%%
%% build time config
%-define(CONFIG_VERBOSE, true).
-define(CONFIG_TTL,       10).  %% time-to-live in seconds

%%
%% debug i/o 
-ifdef(VERBOSE).
   -define(DEBUG(Str, Args), error_logger:error_msg(Str, Args)).
-else.
   -define(DEBUG(Str, Args), ok).
-endif.

%%
%% default fall-back metric
-define(DEFAULT_METRIC(X),  clue:define(gauge, X)).

%%
%% clue metric
%% change clue:lookup(...) if size of tuple is changed
-record(clue, {
   type     = undefined :: atom() %% metric type
  ,key      = undefined :: any()  %% metric id
  ,val      = undefined :: any()  %% metric raw value
  ,time     = undefined :: any()  %% metric time stamp 
  ,ttl      = undefined :: any()  %% time-to-live
  ,state    = undefined :: any()  %% metric state
}).


