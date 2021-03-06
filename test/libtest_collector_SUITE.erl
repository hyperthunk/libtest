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

-import(libtest.matchers, [
  as/1,
  categorises/2,
  observed_message/1,
  observed_message_from/2,
  registered_name/1,
  registered_name/2]).
-import(test_support, [rpc_stop/1, rpc/3]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

init_per_testcase(TestCase, Config) ->
  ?CT_TRACE_ON(TestCase, Config),
  ct:pal("TC: ~p~n", [TestCase]),
  Config.

end_per_testcase(_TestCase, Config) ->
  ?TRACE_OFF(Config),
  ok.

init_per_suite(Config) ->
  ?HECKLE(test_support, heckle, []),
  ?PDEBUG("starting collector...~n", []),
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
  ?NOHECKLE,
  ?PDEBUG("stopping [~p]~n", [catch( ?COLLECTOR:stop() )]),
  Slave = ?config(slave, Config),
  slave:stop(Slave),
  ok.

collector_is_singleton_process(Config) ->
  Slave = ?config(slave, Config),
  Location = global:whereis_name(?COLLECTOR),
  RemoteLocation = rpc:call(Slave, global, whereis_name, [?COLLECTOR]),
  ?assertThat(RemoteLocation, is(equal_to(Location))).

observe_macro_maps_messages_to_collector(Config) ->
  Slave = ?config(slave, Config),
  X = rpc:call(Slave, test_support, raise_notice_event, []),
  ?assertThat(?COLLECTOR, observed_message({hello, 12345})).

observed_messages_can_be_tagged_and_verified(Config) ->
  Record = #'libtest.observation'{
    term={message, "woo hoo"},
    pid=self(),
    node=node(),
    tag=category1
  },
  global:send('libtest.collector', Record),
  ?assertThat(?COLLECTOR,
    categorises(observed_message({message, "woo hoo"}),
      as(category1))).

observed_messages_can_be_tagged_and_verified_by_pid(Config) ->
  Slave = ?config(slave, Config),
  Pid = rpc:call(Slave, test_support, start_observer_process, []),
  rpc:call(Slave, test_support, kick_observer_process, [{message, "hello"}]),
  ?assertThat(Pid, observed_message({message, "hello"}), rpc_stop(Slave)).

observed_messages_can_be_tagged_and_verified_by_name(_) ->
  Mod = test_support,
  Mod:start_observer_process(),
  Msg = {message, "dunbar has fallen"},
  Mod:kick_observer_process(Msg),
  ?assertThat(registered_name(Mod), observed_message(Msg), fun() -> Mod:kick_observer_process(shutdown) end).

observed_messages_can_be_tagged_and_verified_by_remote_name(Config) ->
  Slave = ?config(slave, Config),
  Pid = rpc(Slave, start_observer_process, []),
  Msg = {message, "dunbar has fallen"},
  rpc(Slave, kick_observer_process, Msg),
  ProcessName = test_support,
  ?assertThat(registered_name(Slave, ProcessName), observed_message(Msg), rpc_stop(Slave)).

observed_messages_can_be_tagged_and_verified_by_global_name(Config) ->
  Slave = ?config(slave, Config),
  Pid = rpc(Slave, start_observer_process_globally, ?MODULE),
  Msg = {message, "na fineachan gaidhealach"},
  rpc(Slave, kick_global_process, [?MODULE, Msg]),
  global:sync(),
  ProcessName = test_support,
  ?assertThat(registered_name({global, ?MODULE}), observed_message(Msg), rpc_stop(Slave)).

observed_messages_can_be_tagged_and_verified_by_sender(Config) ->
  Mod = test_support,
  Mod:start_tagged_process(),
  Msg = {message, "lle mae'r cyfarfod?"},
  Mod ! {tagged_message, {sender, self()}, Msg},
  ?assertThat(registered_name(Mod), observed_message_from(self(), Msg),
    fun() -> Mod:kick_observer_process(shutdown) end).

unregistered_name_should_fail_registration(_) ->
  ?assertThat(
    fun() ->
      assert_that(registered_name(not_registered), observed_message(irrelevant))
    end, will_fail(error, {assertion_failed,{invalid_pid,undefined}})).

remote_registered_name_should_fail_when_node_down(_) ->
  ?assertThat(
    fun() ->
      assert_that(registered_name('nosuchnode@localhost', nosuchprocess), observed_message({message, "baz"}))
    end, will_fail(error, {assertion_failed,{invalid_pid,{badrpc,nodedown}}})).

globally_registered_name_should_fail_global_registration_check(_) ->
  ?assertThat(
    fun() ->
      register(my_process_name, self()),
      self() ! {message, "foobar"},
      ?OBSERVE_HERE(10),
      assert_that(registered_name({global, my_process_name}), observed_message({message, "foobar"}))
    end, will_fail(error, {assertion_failed,{invalid_pid,undefined}})).

unobserved_messages_should_fail(_) ->
  ?assertThat(
    fun() ->
      assert_that(self(), observed_message({message, "no I didn't"}))
    end, will_fail()).

invalid_sender_checks_should_fail(_) ->
  ?assertThat(
    fun() ->
      test_support:start_observer_process(),
      test_support:kick_observer_process({message, sent}),
      assert_that(?COLLECTOR, observed_message_from(self(), {message, sent}))
    end, will_fail()).

%%observations_should_fail_consistently_in_assertions(Config) ->
%%  Observations = [
%%    ?LAZY(?assertThat(registered_name(not_registered), observed_message(irrelevant))),
%%    ?LAZY(?assertThat(self(), observed_message({message, "never seen this before"}))),
%%    ?LAZY(?assertThat(?COLLECTOR, observed_message_from(self(), {message, "never sent"}))),
%%    ?LAZY(?assertThat(registered_name({global, not_registered_globally}), observed_message(irrelevant))),
%%    ?LAZY(?assertThat(registered_name(Slave, not_registered), observed_message(irrelevant))),
%%    ?LAZY(
%%    ?assertThat(?COLLECTOR,
%%      categorises(observed_message({message, "woo hoo"}), as(undefined_category)))
%%    )
%%  ],
