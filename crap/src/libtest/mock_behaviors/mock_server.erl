%%
%% Copyright (c) Tim Watson, 2008
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%
%%     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions
%% 	     and the following disclaimer in the documentation and/or other materials provided with the distribution.
%%
%%     * Neither the name of the author nor the names of any contributors may be used to endorse or
%% 	     promote products derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% @author Tim Watson [http://hyperthunk.wordpress.com]
%% @copyright (c) Tim Watson, 2008
%% @since: 29 Feb 2008
%% @version 0.2.0
%% @doc  libtest.mock_behaviors.mock_server unit/integration tests
%% <p>
%% Implements a 'mock' gen_server. Using this module, you can start a
%% gen_server process to "stand in" for a registered server at runtime.
%%
%% The server captures all messages sent to it via
%% </p>
%%
%%
%%


-module(libtest.mock_behaviors.mock_server).
-vsn("0.4").
-author('Tim Watson <watson.timothy@gmail.com>').

-behavior(gen_server).

-import(ct).
-import(proplists).
-import(lists).
-import(gen_server).

%% hack to keep 'cover' tool happy....
-import(ets).

%% gen_server exports

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% public api exports

-export([
    start/0,
    stop/0,
    stub/1,
    forward_call/2,
    forward_cast/2,
    expected_cast/3,
    expected_call/3,
    get_received_calls/1,
    get_received_casts/1,
    get_received_info/1
]).

-define(NAME(State), proplists:get_value(name, State)).
-define(CT_LOGGING(State), proplists:get_value(logging, State)).
-define(NOBOUNCE_MSG,
    "no support for bouncing an existing gen_server process exists yet!").

-define(RECEIVED, 'libtest.mock_behaviors.mock_server.received').
-record(gen_svr_stub, {
    name,
    logging_enabled
}).

stub({Name, CT_LOGGING}) ->
    Stub = #gen_svr_stub{name=Name, logging_enabled=CT_LOGGING},
    case start(Stub) of
        {ok, Pid} ->
            {ok, registered}
                = gen_server:call(?MODULE, {register, Name, Pid}),
            Stub
        ;
        {error, {already_started, _}} ->
            %% TODO: implement a means of replacing an existing gen_server!?
            case gen_server:call(?MODULE, {registered, Name}) of
                {registered, Name, Pid} ->
                    %% it's already a stub so it's ok to just re-register it!
                    %% at this point, any gen_server calls to Name will start failing!
                    unregister(Name),
                    %% this kind of brute force seems a bit excessive!?
                    exit(Pid, kill),
                    stub({Name, CT_LOGGING})
                ;
                {unregistered, Name} ->
                    throw({enotimplemented, {Name, ?NOBOUNCE_MSG}})
            end
    end,
    Stub
;
stub(Name) ->
    stub({Name, false}).

start() ->
    This = ?MODULE,
    start(#gen_svr_stub{name=This, logging_enabled=false}).

start(#gen_svr_stub{name=Name, logging_enabled=CT_LOGGING}) ->
    %% should ?MODULE be 'Name' here instead!?
    Calls = [],
    Casts = [],
    Infos = [],
    gen_server:start({local, Name}, ?MODULE,
        [{name, Name}, {logging, CT_LOGGING},
        {calls, Calls}, {casts, Casts}, {infos, Infos}], []).

stop() ->
    gen_server:call(?MODULE, {stop, normal}).

%% TODO: stop(Name) -> ...

forward_call(#gen_svr_stub{name=Name}, Msg) ->
    gen_server:call(Name, Msg).

forward_cast(#gen_svr_stub{name=Name}, Msg) ->
    gen_server:cast(Name, Msg).

%%forward_info(#gen_svr_stub{name=Name}, Msg) ->
%%    gen_server:info(Name, )

expected_cast(Stub, Key, Index) ->
    {casts, Capture} = get_received_casts(Stub),
    get_expected(Capture, Key, Index).

expected_call(Stub, Key, Index) ->
    {calls, Capture} = get_received_calls(Stub),
    get_expected(Capture, Key, Index).

get_expected(Capture, Key, Index) ->
    case lists:keysearch(Key, Index, Capture) of
        {value, Thing} -> Thing
        ;
        _ -> {error, not_found}
    end.

%% TODO: use ?RECEIVED in these next functions instead...

%% get a list of all messages dealt with by handle_call thus far.
get_received_calls(#gen_svr_stub{name=Name}) ->
    %% NB: this is a parameterized module, so Name is bound....
    gen_server:call(Name, {received, calls}).

%% get a list of all messages dealt with by handle_cast thus far.
get_received_casts(#gen_svr_stub{name=Name}) ->
    %% NB: this is a parameterized module, so Name is bound....
    gen_server:call(Name, {received, casts}).

%% get a list of all messages dealt with by handle_info thus far.
get_received_info(#gen_svr_stub{name=Name}) ->
    %% NB: this is a parameterized module, so Name is bound....
    gen_server:call(Name, {received, infos}).

init(Config) -> {ok, Config}.

handle_call(show, _, State) ->
    %% TODO: use erlang:display instead???
    ct:pal("~p", [State]),
    {reply, {ok, shown}, State}
;
handle_call({stop, normal}, _, State) ->
    {stop, normal, {exit_conditions, "terminated by user"}, State}
;
handle_call({received, MsgType}, _, State) ->
    case proplists:get_value(MsgType, State) of
        undefined ->
                {reply, {ebadarg, unknown_message_type, MsgType}, State}
        ;
        Data ->
            {reply, {MsgType, Data}, State}
    end
;
handle_call({registered, Name}, _, State) ->
    case lists:keysearch(Name, 2, State) of
        {value, {registered, Name, Pid}=Record} ->
            {reply, Record, State}
        ;
        _ -> {reply, {unregistered, Name}, State}
    end
;
handle_call({register, Name, Pid}, _, State) ->
    {reply, {ok, registered}, [{registered, Name, Pid}|State]}
;
handle_call(Instruction, _Sender, State) ->
    Calls = proplists:get_value(calls, State),
    NewState = lists:keyreplace(calls, 1, State, {calls, [Instruction|Calls]}),
    case ?CT_LOGGING(State) of
        true ->
            ct:pal("~p[~p] received handle_call with ~p~n", [?MODULE, ?NAME(State), Instruction])
        ;
        _ -> ignored
    end,
    {reply, {ok, received}, NewState}.

handle_cast(Instruction, State) ->
    Casts = proplists:get_value(casts, State),
    NewState = lists:keyreplace(casts, 1, State, {casts, [Instruction|Casts]}),
    case ?CT_LOGGING(State) of
        true ->
            ct:pal("~p[~p] received handle_cast with ~p~n", [?MODULE, ?NAME(State), Instruction])
        ;
        false ->
            ignored
    end,
    {noreply, NewState}.

handle_info(Info, State) ->
    Infos = proplists:get_value(infos, State),
    NewState = lists:keyreplace(infos, 1, State, {infos, [Info|Infos]}),
    case ?CT_LOGGING(State) of
        true ->
            ct:pal("~p[~p] received handle_info with ~p~n", [?MODULE, ?NAME(State), Info])
        ;
        false ->
            ignored
    end,
    {noreply, NewState}.

terminate(Reason, _State) ->
    ct:pal("Terminating: ~p~n", [Reason]).

code_change(_,_,State) ->
    case ?CT_LOGGING(State) of
        true ->
            ct:pal("~p[~p] received code_change", [?MODULE, ?NAME(State)])
        ;
        false ->
            ignored
    end.
