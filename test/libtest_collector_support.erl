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

-module(libtest_collector_support).

-include("../include/libtest.hrl").
-include("../include/libtest_internal.hrl").

-compile(export_all).

raise_notice_event() ->
  ?OBSERVE({hello, 12345}).

start_observer_process() ->
  Pid = spawn(?MODULE, start_observer, []),
  true = register(?MODULE, Pid),
  Pid.

start_tagged_process() ->
  Pid = spawn(?MODULE, start_observer2, []),
  true = register(?MODULE, Pid),
  Pid.

start_observer_process_globally(Name) ->
  Pid = spawn(?MODULE, start_observer, []),
  yes = global:register_name(Name, Pid),
  global:sync(),
  global:whereis_name(Name).

kick_global_process(Name, Term) ->
  global:send(Name, Term).

kick_observer_process(Term) ->
  ?MODULE ! Term.

start_observer() ->
  Msg = ?OBSERVE_HERE(10000),
  case Msg of
    shutdown -> exit(normal);
    _        -> ok
  end,
  start_observer().

start_observer2() ->
  receive
    shutdown ->
      exit(normal);
    {tagged_message, {sender, Sender}, Msg} ->
      Pid = global:whereis_name('libtest.collector'),
      Record = #'libtest.observation'{
        term=Msg,
        pid=self(),
        node=node(),
        dest=Pid,
        sender = Sender
      },
      global:send('libtest.collector', Record)
  after 10000
    -> ok
  end,
  start_observer2().

rpc(Slave, Method, Args) when is_list(Args) ->
  rpc:call(Slave, ?MODULE, Method, Args);
rpc(Slave, Method, Args) ->
  rpc:call(Slave, ?MODULE, Method, [Args]).

rpc_stop(Slave) ->
  Self = ?MODULE,
  fun() ->
    rpc:call(Slave, Self, kick_observer_processs, [shutdown])
  end.
