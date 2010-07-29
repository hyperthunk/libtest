%% -----------------------------------------------------------------------------
%%
%% Libtest Collector (Server)
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2010 Tim Watson.
%% @doc libtest collector
%%
%% Server which "collects" expectations from clients and information
%% from mocks (invocations, call types, etc) and provides an API for asserting
%% the expectations against the actual mock interactions.
%% -----------------------------------------------------------------------------

-module(libtest.collector).
-author('Tim Watson <watson.timothy@gmail.com>').

-behavior('libtest.gen_server2').

-import(ets). %% to keep the cover tool happy

-import(proplists).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
        ,start_link/1]).

-record(state, {
  
  options = [] :: [term()]
}).

%% -----------------------------------------------------------------------------
%%      Public API
%% -----------------------------------------------------------------------------

%%
%% @doc Starts the collector without any configuration.
%%
start() ->
  do_start(start, []).

%%
%% @doc Starts the server with the supplied configuration.
%%
start_link(Options) ->
  do_start(start_link, Options).

do_start(StartupMode, Options) ->
  apply(proc_lib, StartupMode, [?MODULE,init_it,[self()|Options]).

init_it([Parent|Options]) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Options).

loop(State) ->
  receive
    X -> ok
  end,
  loop(State).
