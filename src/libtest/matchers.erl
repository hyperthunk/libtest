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
-import(ct).

-export([observed_message/1
        ,was_received/1]).

%%
%% @doc Creates a matcher for messages received, using the was_received/1
%%      function as a match fun.
%%
-spec(observed_message/1 :: (term()) -> #'hamcrest.matchspec'{}).
observed_message(Message) ->
  #'hamcrest.matchspec'{
    matcher     = was_received(Message),
    desc        = fun(Expected, Actual) ->
                    Desc = "Expected to have received message ~p, but something went wrong: ~s.",
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
  fun(_) ->
    ct:pal("got ~p~n", [?COLLECTOR:get_observed_messages()]),
    lists:member(Message, ?COLLECTOR:get_observed_messages())
  end.
