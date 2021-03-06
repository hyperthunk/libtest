%% -----------------------------------------------------------------------------
%%
%% Erlang Libtest.
%%
%% Copyright (c) 2006 Tim Watson (tim.watson@hyperthunk.co.uk)
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
%% @author Tim Watson <tim.watson@hyperthunk.co.uk>
%% @copyright 2006 Tim Watson.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%%    Test Suite/Case Support.....
%% -----------------------------------------------------------------------------

-define(CT_REGISTER_TESTS(Mod),
	All = [ FName || {FName, _} <- lists:filter(
      fun ({module_info,_}) -> false ;
          ({all,_}) -> false ;
          ({init_per_suite,1}) -> false ;
          ({end_per_suite,1}) -> false ;
          ({_,1}) -> true ;
          ({_,_}) -> false
      end,
      Mod:module_info(exports)
    )
  ],
  ct:pal("registering ~p~n", [All]),
  All).

-define(CT_TRACE_ON(TestCase, Config),
  libtest:ct_load_trace_configuration(TestCase, Config)).

-define(TRACE_ON(Config),
  libtest:load_bare_trace_configuration(Config)).

-define(TRACE_OFF(Config),
  libtest:unload_trace_configuration(Config)).

%% -----------------------------------------------------------------------------
%%    Support for Observation/Stubbing.....
%% -----------------------------------------------------------------------------

-record('libtest.observation', {
  node    :: node(),
  pid     :: pid(),
  term    :: term(),
  sender  :: pid(),
  tag     :: term()
}).

-define(OBSERVE(Term),
  Record = #'libtest.observation'{ term=Term, pid=self(), node=node() },
  global:send('libtest.collector', Record),
  Record).

-define(OBSERVE_HERE(Timeout),
  receive
    X -> ?OBSERVE(X), X
  after Timeout
    -> timeout
  end).

-define(WAIT_FOR_MESSAGE(Term),
  begin
    receive Term -> ok end
  end).

-define(PDEBUG(Pattern, Args),
  ct:pal("~p: " ++ Pattern, [self()|Args])).
