%% @doc
%%   apply a side-effect of keys
-module(clue_sensor).
-behaviour(gen_server).

-export([
   start_link/3
  ,start_link/4
  ,init/1
  ,terminate/2
  ,handle_call/3
  ,handle_cast/2
  ,handle_info/2
]).

-record(state, {
   fmap = undefined :: _,
   set  = undefined :: _
}).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(T, Fun, Keys) ->
   gen_server:start_link(?MODULE, [T, Fun, Keys], []).

start_link(Name, T, Fun, Keys) ->
   gen_server:start_link({local, Name}, ?MODULE, [T, Fun, Keys], []).

init([T, Fun, Keys]) ->
   timeout(T),
   {ok, 
      #state{
         fmap = Fun,
         set  = ordsets:from_list(Keys)
      }
   }.

terminate(_, _) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% process
%%%
%%%----------------------------------------------------------------------------   

%%
handle_call({add, Key}, _, #state{set = Set} = State) ->
   {reply, ok,
      State#state{
         set = ordsets:add_element(Key, Set)
      }
   };

handle_call({remove, Key}, _, #state{set = Set} = State) ->
   {reply, ok, 
      State#state{
         set = ordsets:del_element(Key, Set)
      }
   };

handle_call(_, _, State) ->
   {reply, {error, unsupported}, State}.

%%
handle_cast(_, State) ->
   {noreply, State}.

%%
handle_info({timeout, T}, #state{fmap = Fun, set = Keys} = State) ->
   Fun(clue:get(Keys)),
   timeout(T),
   {noreply, State}.


%%
%%
timeout(0) ->
   ok;
timeout(T) ->
   erlang:send_after(T, self(), {timeout, T}).


