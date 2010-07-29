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
  %%case whereis(?COLLECTOR) of
  %%    undefined ->
  %%        Pid = emock:gen_server(fun call_handler/2, []),
  %%        ct:pal("registering ~p", [Pid]),
  %%        register(?COLLECTOR, Pid),
  %%        unlink(Pid),
  %%        ct:pal("registered ~p @ ~p", [Pid, whereis(?COLLECTOR)]);
  %%    Pid ->
  %%        unregister(?COLLECTOR),
  %%        exit(Pid, normal),
  %%        register(?COLLECTOR, emock:gen_server(fun call_handler/2, []))
  %%end,
  {ok, Pid} = ?COLLECTOR:start(),
  catch( register(?COLLECTOR, Pid) ),
  Config.

end_per_suite(_Config) ->
  Pid = whereis(?COLLECTOR),
  catch( unregister(?COLLECTOR) ),
  catch( exit(Pid, normal) ),
  ok.

shutdown_can_be_controlled(_) ->
  Pid = ?COLLECTOR:start(),
  ?assertThat(Pid, isalive()),
  ?COLLECTOR:stop(Pid),
  ?assertThat(Pid, isdead()).
