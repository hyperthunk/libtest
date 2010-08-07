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

-import(libtest.matchers, [observed_message/1]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

init_per_suite(Config) ->
  {ok, Server} = ?COLLECTOR:start(),
  ?PDEBUG("in init - collector ~p has started...~n", [Server]),
  ?PDEBUG("in init - collector globally registered against ~p", [global:whereis_name(?COLLECTOR)]),
  ?PDEBUG("in init - is server ~p alive? ~p~n", [Server, erlang:is_process_alive(Server)]),
  Base = ?config(data_dir, Config),
  TestPath = re:replace(Base, filename:basename(Base) ++ "\/$", "", [{return,list}]),
  BinPath = re:replace(TestPath, "\/test\/$", "\/ebin\/", [{return, list}]),
  Args = " -pa " ++ TestPath ++ " -pa " ++ BinPath,
  {ok, Slave} = slave:start(net_adm:localhost(), ?MODULE, Args),
  rpc:call(Slave, global, sync, []),
  [{slave, Slave}|[{server, Server}|Config]].

end_per_suite(Config) ->
  ?PDEBUG("stopping [~p]~n", [catch( ?COLLECTOR:stop() )]),
  Slave = ?config(slave, Config),
  slave:stop(Slave),
  ok.

collector_is_singleton_process(Config) ->
  Slave = ?config(slave, Config),
  Location = global:whereis_name(?COLLECTOR),
  RemoteLocation = rpc:call(Slave, global, whereis_name, [?COLLECTOR]),
  ?PDEBUG("comparing ~p to ~p...~n", [RemoteLocation, Location]),
  ?assertThat(RemoteLocation, is(equal_to(Location))).

observe_macro_maps_messages_to_collector(Config) ->
  Slave = ?config(slave, Config),
  X = rpc:call(Slave, libtest_collector_support, raise_notice_event, []),
  ct:pal("Slave node returned ~p~n", [X]),
  ?assertThat(?COLLECTOR, observed_message({hello, 12345})).
