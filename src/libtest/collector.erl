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
-import(gen_server).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0
        ,start_link/1
        ,stop/0]).

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
%%      Public API
%% -----------------------------------------------------------------------------

%%
%% @doc Starts the server with default configuration values.
%%
start_link() ->
  start_link([]).
  
%%
%% @doc Starts the server with the supplied configuration.
%%
start_link(Options) ->
  gen_server:start({global, ?COLLECTOR}, ?MODULE, Options, gen_server_options(Options)).
  
stop() ->
  %%?PDEBUG("sending kill signal to collector at ~p", [global:safe_whereis_name(?COLLECTOR)]),
  %%global:send(?COLLECTOR, {internal, {kill, self()}}),
  %%?WAIT_FOR_MESSAGE({ok, shutting_down})
  gen_server:call({global, ?COLLECTOR}, stop).

%%init_it(Parent, Options) ->
%%  case catch( global:register_name(?COLLECTOR, self()) ) of
%%    yes ->
%%      proc_lib:init_ack(Parent, {ok, self()}),
%%      loop(#state{ parent=Parent, options=Options });
%%    no ->
%%      proc_lib:init_ack(Parent, {error, registration_failed}),
%%      exit(registration_failed)
%%  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Options) ->
  {ok, #state{ options=Options }}.

handle_call(whassup, _From, State) ->
  {reply, fuck_you, State};
handle_call(stop, _From, State) ->
  throw(fuck),
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
