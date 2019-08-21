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

-define(BASE, 1000000).

%%
%%
usec() ->
   os:timestamp().

%%
%% add time
tinc(_, infinity) ->
   infinity;
tinc({Msec, Sec, Usec} = Z, T)
 when is_integer(T) ->
   A0  = T rem ?BASE,
   Y   = T div ?BASE,
   A1  = Y rem ?BASE,
   A2  = Y div ?BASE,
   {C0, Q0} = add_time(A0, Usec,  0),
   {C1, Q1} = add_time(A1, Sec,  Q0),
   {C2,  _} = add_time(A2, Msec, Q1),
   {C2, C1, C0}.
  
add_time(X, Y, Q) ->
   T = X + Y + Q,
   {T rem ?BASE, T div ?BASE}.

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
