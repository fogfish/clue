%% @description
%%    version of all running applications
-module(clue_api_version).

-export([
   uri/0, 
   allowed_methods/1, content_provided/1, content_accepted/1,
   'GET'/3
]).

uri() ->
   {vsn,  "/_sys/vsn"}.

allowed_methods(_Uid)  -> 
   ['GET'].

content_provided(_Uid) -> 
   [
      {text, 'text/plain'}
   ].

content_accepted(_Uid) -> 
   [].

'GET'({vsn, _}, _Uri, _Heads) ->
   Text = lists:map(
      fun({Title, Desc, Vsn}) ->
         io_lib:format("~s\t~s-~s~n", [Desc, Title, Vsn])
      end,
      application:which_applications()
   ),
   {ok, list_to_binary(Text)}.

