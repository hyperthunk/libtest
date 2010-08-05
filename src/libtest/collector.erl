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

-include("libtest.hrl").
-include("libtest_internal.hrl").

-import(ets). %% to keep the cover tool happy
-import(error_logger).
-import(lists).
-import(?GEN_SERVER).

-behaviour(?GEN_SERVER).

%% ------------------------------------------------------------------
%% Server API Function Exports
%% ------------------------------------------------------------------

-export([start/0
        ,start/1
        ,start_link/0
        ,start_link/1
        ,stop/0]).

%% ------------------------------------------------------------------
%% Collection API Function Exports
%% ------------------------------------------------------------------

-export([get_received_messages/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-record(state, {
  options   = []          :: [term()]
}).

%% -----------------------------------------------------------------------------
%%      Server API
%% -----------------------------------------------------------------------------

%%
%% @doc Starts the server with default configuration values.
%%
start() ->
  start([]).

%%
%% @doc Starts the server with the supplied configuration.
%%
start(Options) ->
  ?GEN_SERVER:start({global, ?COLLECTOR}, ?MODULE, Options, gen_server_options(Options)).

%%
%% @doc Starts the server (with default configuration values) as part of a supervision tree.
%%
start_link() ->
  start_link([]).

%%
%% @doc Starts the server with the supplied configuration as part of a supervision tree.
%%
start_link(Options) ->
  ?GEN_SERVER:start_link({global, ?COLLECTOR}, ?MODULE, Options, gen_server_options(Options)).

%%
%% @doc Stops the registered instance of this server.
%%
stop() ->
  ?GEN_SERVER:call({global, ?COLLECTOR}, stop).

%% -----------------------------------------------------------------------------
%%      Collector API
%% -----------------------------------------------------------------------------

%%
%% @doc
%% get_received_messages() -> [term()]
%% Gets a list of all received messages.
%%
-spec(get_received_messages/0 :: () -> [term()]).
get_received_messages() ->
  #state{} = ?GEN_SERVER:call({global, ?COLLECTOR}, {?COLLECTOR, retrieve_state}),
  [].

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Options) ->
  {ok, #state{ options=Options }}.

handle_call({?COLLECTOR, retrieve_state}, _From, State) ->
  {reply, State, State};
handle_call(stop, _From, State) ->
  {stop, normal, State};
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @hidden
gen_server_options(Options) ->
  lists:filter(fun({debug, _}) -> true;
                  ({timeout, _}) -> true;
                  ({spawn_opt, _}) -> true;
                  (_) -> false
               end, Options).
