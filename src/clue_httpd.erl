%%
%%   Copyright (c) 2012, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%%   @description
%%      light-weight http interface
-module(clue_httpd).
-behaviour(gen_server).

-export([
   start_link/1,
   init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2
]).

-record(httpd, {
   sock  = undefined :: any(),
   dec   = undefined :: htstream:http(),
   iobuf = undefined :: binary()
}).

start_link(LSock) ->
   gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
   {ok, 
      #httpd{
         sock  = LSock,
         dec   = htstream:new(),
         iobuf = <<>>
      },
      0
   }.

terminate(_Reason, S) ->
   gen_tcp:close(S#httpd.sock),
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
handle_call(_, _, S) ->
   {noreply, S}.

%%
handle_cast(_, S) ->
   {noreply, S}.


%%
handle_info(timeout, S) ->
   try
      {ok, Sock} = gen_tcp:accept(S#httpd.sock),
      supervisor:start_child(clue_httpd_sup, []),
      inet:setopts(Sock, [{active, once}]),
      {noreply, S#httpd{sock=Sock}}
   catch _:Reason ->
      supervisor:start_child(clue_httpd_sup, []),
      {stop, Reason, S}
   end;

handle_info({tcp, _, Pckt}, S) ->
   inet:setopts(S#httpd.sock, [{active,once}]),
   {Input, Buffer, Http} = htstream:decode(iolist_to_binary([S#httpd.iobuf, Pckt]), S#httpd.dec),
   handle_request(htstream:state(Http), Input, S#httpd{dec=Http, iobuf=Buffer}),
   {noreply, S};

handle_info({tcp_closed, _}, S) ->
   {stop, normal, S}.

%%
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%
handle_request(eof, {'GET', <<"/_sys/vsn">>, _}, S) ->
   Msg = which_applications(),
   Heads = [{'Server', <<"clue">>}, {'Content-Length', size(Msg)}, {'Content-Type', 'text/plain'}],
   {Pckt, _, _} = htstream:encode({200, Heads, Msg}, htstream:new()),
   ok = gen_tcp:send(S#httpd.sock, Pckt),
   {stop, normal, S#httpd{dec=htstream:new()}};      

handle_request(eof, {'GET', <<"/_sys/clue">>, _}, S) ->
   Msg = sys_clue(),
   Heads = [{'Server', <<"clue">>}, {'Content-Length', size(Msg)}, {'Content-Type', 'text/plain'}],
   {Pckt, _, _} = htstream:encode({200, Heads, Msg}, htstream:new()),
   ok = gen_tcp:send(S#httpd.sock, Pckt),
   {stop, normal, S#httpd{dec=htstream:new()}};      

handle_request(eof, {'GET', <<"/_sys/clue/", Pfx/binary>>, _}, S) ->
   Msg = sys_clue(binary_to_existing_atom(Pfx, utf8)),
   Heads = [{'Server', <<"clue">>}, {'Content-Length', size(Msg)}, {'Content-Type', 'text/plain'}],
   {Pckt, _, _} = htstream:encode({200, Heads, Msg}, htstream:new()),
   ok = gen_tcp:send(S#httpd.sock, Pckt),
   {stop, normal, S#httpd{dec=htstream:new()}};      

handle_request(_,   {_, _, _}, S) ->
   Msg   = <<"400 Bad Request">>,
   Heads = [{'Server', <<"clue">>}, {'Content-Length', size(Msg)}, {'Content-Type', 'text/plain'}],
   {Pckt, _, _} = htstream:encode({400, Heads, Msg}, htstream:new()),
   ok = gen_tcp:send(S#httpd.sock, Pckt),
   {stop, normal, S#httpd{dec=htstream:new()}};
   
handle_request(_,   _,  S) ->
   {noreply, S}.
   
%%
which_applications() ->
   iolist_to_binary(
      lists:map(
         fun({Title, Desc, Vsn}) ->
            io_lib:format("~s-~s\t\t~s~n", [Title, Vsn, Desc])
         end,
         application:which_applications()
      )
   ).

%% sys clue
sys_clue() ->
   iolist_to_binary(
      lists:map(
         fun({Key, Val}) ->
            [urn(Key), $\t, $\t, scalar:s(Val), $\n]
         end,
         clue:lookup('_')
      )
   ).

%% sys clue
sys_clue(Pfx) ->
   iolist_to_binary(
      lists:map(
         fun({Key, Val}) ->
            [urn(Key), $\t, $\t, scalar:s(Val), $\n]
         end,
         clue:prefix(Pfx)
      )
   ).

urn(Key)
 when is_tuple(Key) ->
   string:join(
      [ [scalar:s(X)] || X <- tuple_to_list(Key)],
      ":"
   );
urn(Key) ->
   scalar:s(Key).

