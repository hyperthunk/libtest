%% -----------------------------------------------------------------------------
%%
%% Libtest Collector (Server) SUITE
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
%% -----------------------------------------------------------------------------

-module(libtest_collector_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include("../include/libtest.hrl").
-include("../include/libtest_internal.hrl").
-compile(export_all).

all() -> ?CT_REGISTER_TESTS(?MODULE).

init_per_suite(Config) ->
  %%?PDEBUG("starting ~p~n", [?GEN_SERVER]),
  {ok, Server} = ?COLLECTOR:start_link(),
  ?PDEBUG("collector startup: ~p~n", [Server]),
  ?PDEBUG("collector registered against ~p", [global:whereis_name(?COLLECTOR)]),
  ?PDEBUG("in init - is server alive? ~p~n", [erlang:is_process_alive(Server)]),
  [{server, Server}|Config].
  %%Pid = spawn(?MODULE, loop, []),
  %%{ok, Slave} = slave:start(net_adm:localhost(), ?MODULE),
  %%global:register_name(?COLLECTOR, Pid),
  %%?PDEBUG("collector registered against ~p", [global:whereis_name(?COLLECTOR)]),
  %%global:sync(),
  %%rpc:call(Slave, global, sync, []),
  %%[{slave, Slave}|Config].

end_per_suite(Config) ->
  %%?PDEBUG("stopping [~p]~n", [catch( ?COLLECTOR:stop() )]),
  %%Slave = ?config(slave, Config),
  %%slave:stop(Slave),
  ok.

collector_is_singleton_process(Config) ->
  Server = ?config(server, Config),
  ?PDEBUG("in test - collector registered against ~p~n", [global:whereis_name(?COLLECTOR)]),
  ?PDEBUG("in test - is server alive? ~p~n", [erlang:is_process_alive(Server)]),
  ?PDEBUG("server ~p status: ~p~n", [Server, gen_server:call(?COLLECTOR, whassup)]),
  %% erlang:is_process_alive(Server)]),
  
  %%Slave = ?config(slave, Config),
  {ok, Slave} = slave:start(net_adm:localhost(), ?MODULE),
  Location = global:whereis_name(?COLLECTOR),
  RemoteLocation = rpc:call(Slave, global, whereis_name, [?COLLECTOR]),
  ?PDEBUG("comparing ~p to ~p...~n", [RemoteLocation, Location]),
  ?assertThat(RemoteLocation, is(equal_to(Location))).
