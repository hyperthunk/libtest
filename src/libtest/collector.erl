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

-behavior(libtest.gen_server2).

-include_lib("hamcrest/include/hamcrest.hrl").

-import(ets). %% to keep the cover tool happy

-import(proplists).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-export([start/0
        ,start/1
        ,start_link/1]).

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
%% Options = [
%%     {scope, local | global}   %% indicates local versus global registration
%%    ]
%%
%%
start(Options) ->
    do_start(start,
        [{proplists:get_value(scope, Options), ?MODULE}, ?MODULE, Options, []]).

%%
%% @doc Starts the server with the supplied configuration.
%%
%% -spec(start_link/1 :: (Options::[server_option()]) -> term()).
start_link(Options) ->
    do_start(start_link,
        [{proplists:get_value(scope, Options), ?MODULE}, ?MODULE, Options, []]).

do_start(StartupMode, Options) ->
    apply(libtest.gen_server2, StartupMode, Options).

%% -----------------------------------------------------------------------------
%% gen_server2 callbacks
%% -----------------------------------------------------------------------------

%% @hidden
%% initializes the server with the current "state of the world"
init(_Args) ->
    ok.

handle_call(_Msg, {_From, _Tag}, State) ->
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    {reply, State, State}.

handle_cast(_Msg, State) ->
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%%      Private API
%% -----------------------------------------------------------------------------
