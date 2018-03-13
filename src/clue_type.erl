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
%% @doc
%%
-module(clue_type).

-export([
   usec/0,
   tinc/2,
   diff/1,
   diff/2,
   round/1
]).


%%
%%
usec() ->
   os:timestamp().

%%
%% add time
tinc(_, infinity) ->
   infinity;
tinc({Msec, Sec, Usec}, T)
 when is_integer(T) ->
   case Usec + T of
      X when X =< 1000000 ->
         {Msec, Sec, X};
      X when X =< 1000000 * 1000000 ->
         {Msec, Sec + (X div 1000000), X rem 1000000};
      X ->
         {Msec + (X div (1000000 * 1000000)), Sec + (X div 1000000), X rem 1000000}
   end.

%%
%%
diff(T) ->
   diff(os:timestamp(), T).

diff(A, B) ->
   (timer:now_diff(A, B) / 1000000).

%%
%%
round(X)
 when X < 1.0e-3 ->
   0.0;
round(X) ->
   X.
