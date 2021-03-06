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
-import(global).
-import(ct).
-import(erl_pp).
-import(erl_parse).

-record(registration_query, {
  type      = local       :: local | global | remote,
  name      = undefined   :: term(),
  context   = undefined   :: term(),
  category  = undefined   :: term()
}).

-export([registered_name/1
        ,registered_name/2
        ,observed/1
        ,observed_all_of/1
        ,observed_all_of/2
        ,observed_message/1
        ,observed_message_from/2
        ,was_received/2
        ,was_received/3
        ,categorises/2
        ,pp_term/1
        ,as/1]).

pp_term(T) when is_pid(T) ->
  io_lib:write(T);
pp_term(T) when is_function(T) ->
  io_lib:format("function ~p", [T]);
pp_term(T) ->
  erl_pp:expr(erl_parse:abstract(T)).

%%
%% @doc returns an internal structure representing a query against a
%%      name assumed as registered globally or locally as Term.
%%
registered_name({global, Term}) when is_atom(Term) ->
  #registration_query{ type=global, context=global, name=Term };
registered_name(Term) when is_atom(Term) ->
  #registration_query{ type=local, context=node(), name=Term }.

%%
%% @doc returns an internal structure representing a query against a
%%      name assumed as registered remotely as Term on Node
%%
registered_name(Node, Term) when is_atom(Node) andalso is_atom(Term) ->
  #registration_query{ type=remote, context=Node, name=Term }.

%%
%% @doc creates a matcher that checks the category of received observations.
%%
categorises(#'hamcrest.matchspec'{ expected={Message, {from, Sender}} }=Matcher, Category) ->
  Matcher#'hamcrest.matchspec'{ matcher=was_received(Message, Sender, Category) };
categorises(#'hamcrest.matchspec'{ expected=Message }=Matcher, Category) ->
  Matcher#'hamcrest.matchspec'{ matcher=was_received(Message, Category) }.

%% @doc this is the default identity function - use as syntactic sugar for writing matchers
as(Category) ->
  Category.

%% @doc more syntactic sugar, so you can write <pre>?assertThat(Thing, observed(Blah))</pre>.
observed(MatchSpec) when is_record(MatchSpec, 'hamcrest.matchspec') ->
  MatchSpec.

%% @doc creates an all_of matcher for the list of supplied messages.
observed_all_of(Messages) when is_list(Messages) ->
  all_of([ observed_message(M) || M <- Messages ]).

%% @doc creates an all_of matcher for the list of supplied messages.
observed_all_of(Sender, Messages) when is_list(Messages) ->
  all_of([ observed_message_from(Sender, M) || M <- Messages ]).

%%
%% @doc Creates a matcher for messages received from the provided Sender, using the was_received/2
%%      function as a match fun. If you pass assert_that (or the macro equivalent)
%%      a process id, then the matcher will only evaluate to true if the supplied
%%      pid observed the specified message at least once.
%%
-spec(observed_message_from/2 :: (pid(), term()) -> #'hamcrest.matchspec'{}).
observed_message_from(Sender, Message) when is_pid(Sender) ->
  Desc = fun observation_failed/2,
  #'hamcrest.matchspec'{
    matcher     = was_received(Message, Sender, undefined),
    desc        = Desc,
    expected    = {Message, {from, Sender}}
  }.

%%
%% @doc Creates a matcher for messages received, using the was_received/1
%%      function as a match fun. If you pass assert_that (or the macro equivalent)
%%      a process id, then the matcher will only evaluate to true if the supplied
%%      pid observed the specified message at least once.
%%
-spec(observed_message/1 :: (term()) -> #'hamcrest.matchspec'{}).
observed_message(Message) ->
  Desc = fun observation_failed/2,
  #'hamcrest.matchspec'{
    matcher     = was_received(Message, undefined),
    desc        = Desc,
    expected    = Message
  }.

observation_failed(Expected, Actual) when is_list(Actual) ->
  Desc = "Expected to have received message ~p, but something went wrong: ~s.",
  lists:flatten(io_lib:format(Desc, [Expected, Actual]));
observation_failed(Expected, Actual) ->
  Desc = "Expected to have received message ~p, but something went wrong: ~s.",
  lists:flatten(io_lib:format(Desc, [Expected, pp_term(Actual)])).

%%
%% @doc Returns a function that evaluates whether (or not) the specified
%%      Message has been received by the 'libtest.collector' at any time.
%%
-spec(was_received/2 :: (term(), term()) -> fun((term()) -> true | false)).
was_received(Message, Tag) ->
  fun(Ref) ->
    case check_observed_messages(Ref, Message) of
      [#'libtest.observation'{ tag=Tag }|_] -> true;
      [] -> false
    end
  end.

%%
%% @doc Returns a function that evaluates whether (or not) the specified
%%      Message has been received by the 'libtest.collector' at any time.
%%
-spec(was_received/3 :: (term(), pid(), term()) -> fun((term()) -> true | false)).
was_received(Message, Sender, Tag) ->
  fun(Ref) ->
    case check_observed_messages(Ref, Message, Sender) of
      [#'libtest.observation'{ tag=Tag }|_] -> true;
      [] -> false
    end
  end.

verify_pid(Pid) when is_pid(Pid) ->
  Pid;
verify_pid(Pid) ->
  {internal_error, {invalid_pid, Pid}}.

check_observed_messages(#registration_query{ type=global, name=Term }, Message) ->
  check_observed_messages(verify_pid(global:whereis_name(Term)), Message, undefined);
check_observed_messages(#registration_query{ type=remote, context=Node, name=Term }, Message) ->
  check_observed_messages(verify_pid(rpc:call(Node, erlang, whereis, [Term])), Message, undefined);
check_observed_messages(#registration_query{ type=local, name=Term }, Message) ->
  check_observed_messages(verify_pid(whereis(Term)), Message, undefined);
check_observed_messages('libtest.collector', Message) ->
  check_observed_messages(undefined, Message, undefined);
check_observed_messages(Pid, Message) when is_pid(Pid) ->
  check_observed_messages(Pid, Message, undefined).

%% why does rpc:call(Node, erlang, whereis, [Term]) work when the node name is invalid!?

check_observed_messages(#registration_query{ type=global, name=Term }, Message, Sender) ->
  check_observed_messages(verify_pid(global:whereis_name(Term)), Message, Sender);
check_observed_messages(#registration_query{ type=remote, context=Node, name=Term }, Message, Sender) ->
  check_observed_messages(verify_pid(rpc:call(Node, erlang, whereis, [Term])), Message, Sender);
check_observed_messages(#registration_query{ type=local, name=Term }, Message, Sender) ->
  check_observed_messages(verify_pid(whereis(Term)), Message, Sender);
check_observed_messages(Pid, Message, Sender) when is_pid(Pid) ->
  P = fun(Msg) ->
    case Msg of
      #'libtest.observation'{ pid=Pid, term=Message, sender=Sender } -> true;
      _ -> false
    end
  end,
  check_observed_messages(P);
check_observed_messages({internal_error, Err}, _, _) ->
  erlang:error({assertion_override, Err});
check_observed_messages(_Ref, Message, Sender) ->
  P = fun(Msg) ->
    case Msg of
      #'libtest.observation'{ term=Message, sender=Sender } -> true;
      _ -> Msg == Message
    end
  end,
  check_observed_messages(P).

check_observed_messages(P) ->
  lists:filter(P, ?COLLECTOR:get_observed_messages()).
