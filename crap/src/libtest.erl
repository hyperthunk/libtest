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
%% @doc  libtest public api
%% <p>
%% This module exposes a simple API for writing unit and integration tests.
%% No particular testing framework is assumed, and none of the functions
%% in this module deliberately generates exceptions, errors or exit signals.
%% </p>
%%

-module(libtest).
-author('Tim Watson <watson.timothy@gmail.com>').

-compile(export_all).

-include("libtest.hrl").

-import(lists).
-import(io_lib).

-define(ANY, 'libtest._any_failure_mode').


%%%-----------------------------------------------------------------
%%% @spec verify(MatchFun, Description) -> ok | {failed, Reason}
%%%       MatchFun = fun()
%%%       Description = string()
%%%       Reason = term()
%%%
%%% @doc Runs <code>MatchFun</code> and verifies that it returns the atom
%%% <code>true</code>, if so, returning the atom <code>ok</code>. If the
%%% the return value is <code>false</code> then a failure record is
%%% returned instead. Finally, if the return value is another (arbitrary)
%%% term, then this (term) is returned verbatim.
verify(MatchFun, Description) when is_function(MatchFun) ->
    fun(Input) ->
        case MatchFun(Input) of
            true  -> ok ;
            false -> ?EX_FAILED(Description, Input) ;
            Other -> Other
        end
    end.

expect_that(Expr, {Matcher, Desc}) when is_function(Expr, 0)
    andalso is_function(Matcher, 1) ->
        wrap_result((verify(Matcher, Desc))(Expr()))
;
expect_that(Expr, {Matcher,_}=Expectation) when not(is_function(Expr))
    andalso is_function(Matcher, 1) ->
        expect_that(fun() -> Expr end, Expectation).

%%% @hidden
wrap_result({failed,
    Context=#'libtest.expectation_failure'{
        expected=Expected,
        actual=Actual
    }}) ->
    {failed, {?MSG_FORMAT(?EXPECTATION_FAILURE_FMT, [Expected, Actual]), Context}}
;
wrap_result(Other) -> Other.

%%should_fail() ->
%%    fun(Fun) -> should(fun() -> check_fail(Fun, [], []) end, "description") end.

%%is_true() -> fun is_true/1.
%%
%%is_true(Fun) when is_function(Fun) -> is_true(Fun())  ;
%%is_true(true) -> ok ;
%%is_true(Other) -> { failed,
%%        #'libtest.expectation_failure'{
%%            expected=true,
%%            actual=Other
%%        }
%%    }.
%%
%%be_in(List) when is_list(List) ->
%%    fun(Item) -> lists:member(Item, List) end.
%%
%%should_fail() ->
%%    fun(Fun) -> check_fail(Fun, [error, exit, throw], ?ANY) end.
%%
%%
%%
%%expect_that(Fun, Rule) when is_function(Fun) ->
%%    Rule(Fun).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc assert that Expression causes an error at runtime, culminating in
%% one of the exception types defined in the list ExpectedTypes, providing
%% exception data matching the expression ExpectedExpression.
%%
%% @spec check_fail(Fun::fun(), ExpectedTypes::list(atom()), ExpectedExpression::term()) -> {ok, failed_as_expected}
%%
%%check_fail(Fun, ExpectedType, ExpectedData)
%%    when is_atom(ExpectedType) ->
%%    check_fail(Fun, [ExpectedType], ExpectedData)
%%;
%%check_fail(Fun, ExpectedTypes, ExpectedData)
%%    when is_list(ExpectedTypes) ->
%%    try (Fun()) of
%%        Response -> expected_exception_failure(
%%            format_types(ExpectedTypes),
%%            ExpectedData,
%%            Response,
%%            erlang:get_stacktrace()
%%        )
%%    catch
%%        ExType:ExPattern ->
%%            case check_ex_type(ExpectedTypes, ExType) of
%%                ok ->
%%                    case check_ex_data(ExpectedData, ExPattern) of
%%                        ok -> ok
%%                        ;
%%                        Failed -> Failed
%%                    end
%%                ;
%%                Failed -> Failed
%%            end
%%    end.
%%
%%check_ex_type(ExpectedTypes, ActualType) ->
%%    is_in()
%%    try assert_member(ActualType, ExpectedTypes) of
%%        _ -> ok
%%    catch
%%        _:_ ->
%%            {failed, #'libtest.expectation_failure'{
%%                expected=ExpectedTypes,
%%                actual=ActualType }}
%%    end.
%%
%%check_ex_data(ExpectedData, ActualData) ->
%%    case (ExpectedData) of
%%        ?ANY -> ok
%%        ;
%%        _ ->
%%            try assert_match(fun(E) -> E = ExpectedData end, ActualData) of
%%                _ -> ok
%%            catch
%%                _:_ ->
%%                    {failed, #'libtest.expectation_failure'{
%%                        expected=ExpectedData,
%%                        actual=ActualData }}
%%            end
%%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_equals(Thing1, Thing2) ->
    assert_equals(Thing1, Thing2, []).

assert_equals(Thing1, Thing2, Format, Args) ->
    assert(Thing1 == Thing2, Format, Args).

assert_equals(Thing1, Thing2, Args) ->
    Format = case length(Args) of
        0 -> "Expected value to equal ~p, but was ~p.~n" ;
        _ -> "Expected value to equal ~p, but was ~p: ~p.~n"
    end,
    assert_equals(Thing1, Thing2, Format, [Thing1, Thing2 | Args]).

assert_same(Thing1, Thing2, Format, Args) ->
    assert(Thing1 =:= Thing2, Format, Args).

assert_same(Thing1, Thing2) ->
    assert_same(Thing1, Thing2, "Expected ~p to match ~p but does not.", [Thing1, Thing2]).

assert_member(Expected, List) ->
    assert(lists:member(Expected, List),
        "Expected ~p to be a member of ~p, but was not.", [Expected, List]).

%% @doc Asserts that Condition is 'true', otherwise fails with Category, Format, Args
assert(Condition, Format, Args) ->
    case Condition of
        true -> true ;
        false ->
            %% ct:log(Category, Format, Args),
            ct:fail(io:format(Format, Args))
    end.

assert_true(true) -> ok
;
assert_true(Other) -> ct:fail("Expected assert_true with 'true' but received ~p.", [Other]).

assert_false(false) -> ok
;
assert_false(Other) -> ct:fail("Expected assert_false with 'false' but received ~p.", [Other]).

assert_match(ShouldMatch, This)
    when is_function(ShouldMatch, 1) ->
    try ShouldMatch(This) of
        _ -> ok
    catch
        error:{badmatch, _} ->
            ct:fail(io:format("Expected ~p to match ~p, but match failed.", [ShouldMatch, This]))
    end.

assert_timely_exit(_Fun, _ExpectedExitStatus, _ExpectedDurationMin,
    _ExpectedDurationMax, _Category, _Format, _Args) -> not_implemented().

assert_pexit(Fun, ExpectedExitStatus) ->
    assert_pexit(Fun, ExpectedExitStatus, {timeout, infinite}).

assert_pexit(_Fun, _ExpectedExitStatus, _ExpectedDuration) ->
    not_implemented().

%%assert_fail(Fun) ->
%%    check_fail(Fun, [error, exit, throw], ?ANY).
%%
%%assert_fail(Fun, ExpectedType) when is_atom(ExpectedType) ->
%%    assert_fail(Fun, [ExpectedType])
%%;
%%assert_fail(Fun, ExpectedTypes) when is_list(ExpectedTypes) ->
%%    assert_fail(Fun, ExpectedTypes, ?ANY).

not_implemented() -> throw({error, not_implemented}).

trace_calling_function() ->
    {_, ProgInfo} = process_info(self(), backtrace),
    io:format("~p", [ProgInfo]),
    MatchTuple = re:run(ProgInfo, "(Return addr\\s[0-9a-z]+\\s.*\\))"),
    io:format("~p", [MatchTuple]),
    case (MatchTuple) of
        {match, [{_,_},{_,_},{Start, Len}|_]} ->
            <<_:Start/binary,Real:Len/binary,_/binary>> = ProgInfo,
            Text = binary_to_list(Real),
            [ModName, FA] = string:tokens(Text, ":"),
            Mod = list_to_atom(ModName),
            [FuncName, Arity] = string:tokens(FA, "/"),
            {Mod, list_to_atom(FuncName), list_to_integer(Arity)}
        ;
        _Other   -> {unknown, unknown}
    end.
