%% @doc
%%
-module(clue_logger).
-behaviour(gen_server).

-export([
   start_link/1
  ,init/1
  ,terminate/2
  ,handle_call/3
  ,handle_cast/2
  ,handle_info/2
]).


start_link(TTL) ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [TTL], []).

init([0]) ->
   {ok, []};

init([TTL]) ->
   erlang:send_after(TTL, self(), {logger, TTL}),
   {ok, []}.

terminate(_, _) ->
   ok.

handle_call({log, Key}, _, State) ->
   {reply, ok, [Key|State]}.

handle_cast(_, State) ->
   {noreply, State}.

handle_info({logger, TTL}, State) ->
   error_logger:info_report(clue:get(State)),
   erlang:send_after(TTL, self(), {logger, TTL}),
   {noreply, State}.


