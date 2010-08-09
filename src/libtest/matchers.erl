%% -----------------------------------------------------------------------------
%%
%% Libtest Matchers (Library Module)
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
%% @doc libtest.matchers
%%
%% Custom Hamcrest Matchers for use with libtest
%% -----------------------------------------------------------------------------

-module(libtest.matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

-include("libtest.hrl").
-include("libtest_internal.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").

-import(ets). %% to keep the cover tool happy
-import(lists).
-import(io_lib).
-import(rpc).
-import(ct).

-record(registration_query, {
  type    = local       :: term(),
  name    = undefined   :: term(),
  context = undefined   :: term()
}).

-export([registered_name/1
        ,registered_name/2
        ,observed_message/1
        ,was_received/1]).

%%
%% @doc returns an internal structure representing a query against a
%%      name assumed as registered locally as Term.
%%
registered_name(Term) when is_atom(Term) ->
  #registration_query{ type=local, context=node(), name=Term }.


%%
%% @doc returns an internal structure representing a query against a
%%      name assumed as registered remotely as Term on Node
%%
registered_name(Node, Term) when is_atom(Node) andalso is_atom(Term) ->
  #registration_query{ type=remote, context=Node, name=Term }.

%%
%% @doc Creates a matcher for messages received, using the was_received/1
%%      function as a match fun. If you pass assert_that (or the macro equivalent)
%%      a process id, then the matcher will only evaluate to true if the supplied
%%      pid observed the specified message at least once.
%%
-spec(observed_message/1 :: (term()) -> #'hamcrest.matchspec'{}).
observed_message(Message) ->
  #'hamcrest.matchspec'{
    matcher     = was_received(Message),
    desc        = fun(Expected, Actual) when is_list(Actual) ->
                        Desc = "Expected to have received message ~p, but something went wrong: ~s.",
                        lists:flatten(io_lib:format(Desc, [Expected, Actual]));
                     (Expected, Actual) ->
                        Desc = "Expected to have received message ~p, but something went wrong: ~p.",
                        lists:flatten(io_lib:format(Desc, [Expected, Actual]))
                  end,
    expected    = Message
  }.

%%
%% @doc Returns a function that evaluates whether (or not) the specified
%%      Message has been received by the 'libtest.collector' at any time.
%%
-spec(was_received/1 :: (term()) -> fun((term()) -> true | false)).
was_received(Message) ->
  fun(Ref) ->
    case check_observed_messages(Ref, Message) of
      [_H|_] -> true;
      [] -> false
    end
  end.

check_observed_messages('libtest.collector', Message) ->
  P = fun(Msg) ->
    case Msg of
      #'libtest.observation'{ term=Message } -> true;
      _ -> Msg == Message
    end
  end,
  check_observed_messages(P);
check_observed_messages(Pid, Message) when is_pid(Pid) ->
  P = fun(Msg) ->
    case Msg of
      #'libtest.observation'{ pid=Pid, term=Message} -> true;
      _ -> false
    end
  end,
  check_observed_messages(P);
check_observed_messages(#registration_query{ type=remote, context=Node, name=Term }, Message) ->
  Pid = rpc:call(Node, erlang, whereis, [Term]),
  check_observed_messages(Pid, Message);
check_observed_messages(#registration_query{ type=local, name=Term }, Message) ->
  Pid = whereis(Term),
  check_observed_messages(Pid, Message).

check_observed_messages(P) ->
  lists:filter(P, ?COLLECTOR:get_observed_messages()).