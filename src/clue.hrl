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
-define(DEFAULT_METRIC(X, Y),  clue:gauge(X, Y)).

%%
%% clue metric
-record(clue, {
   type,  %% metric type
   key,   %% metric id
   val,   %% metric raw value
   time,  %% metric time stamp 
   ttl,   %% time-to-live
   state  %% metric state
}).







% -define(CLUE_FLUSH,  10000). 
% -define(CLUE_SYNC,   10000). 

% -define(CLUE_TCP, [
%    binary,
%    {packet,  line},
%    {recbuf,  128 * 1024},
%    {sndbuf,  128 * 1024}
% ]).



