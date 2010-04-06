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
%% @doc  libtest unit/integration tests
%% @private

-module(libtest_SUITE).
-author('Tim Watson <timwatson@munotify.com>').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/libtest.hrl").

-import(libtest, [
    verify/2,
    expect_that/2,
    should_fail/0,
    format_expectation/1
]).

-import(libtest.matchers, [equal_to/1]).

-import(lists, [map/2, seq/2, flatten/1, zip/2]).
-import(io_lib, [format/2]).

-define(FAIL, fun(_) -> false end).
-define(PASS, fun(_) -> true end).

all() -> ?CT_REGISTER_TESTS(?MODULE).

init_per_suite(Conf) ->
    %%dbg:tracer(),
    %%TraceMods = [libtest],
    %%TraceFun = fun(Mod) -> dbg:tpl(Mod, '_', '_', [{'_',[],[{exception_trace}]}]) end,
    %%lists:map(TraceFun, TraceMods),
    %%dbg:p(all,[c]),
    Conf.

end_per_suite(_) -> ok.

verify_wrapper_converts_match_to_ok(_) ->
    ok = (verify(?PASS, "ignored"))(ignored).

verify_wrapper_converts_false_to_failure_tuple(_) ->
    {failed, _} =
        (verify(?FAIL, "ignored"))(ignored),
    ok.

verify_wrapper_appends_expectation_context_to_failure(_) ->
    Desc = "a value matching some custom condition or other",
    Input = input,
    {failed, #'libtest.expectation_failure'{
        expected=Desc,
        actual=Input
    }} = (verify(?FAIL, Desc))(Input),
    ok.

verify_wrapper_should_transparently_handle_fail_tuples(_) ->
    %% TODO: remove duplication and create library functions to support this...
    C1 = 10000,
    C2 = 27349,
    Range = 100,
    {A,B} = list_to_tuple(map(
        fun(Ceiling) ->
            map(fun(_) -> random:uniform(Ceiling) end, seq(1, Range))
        end,
        [C1,C2]
    )),
    map(
        fun({X,Y}) ->
            ?EX_FAILED(X,Y) = (verify(fun(_) -> ?EX_FAILED(X, Y) end, ""))(Y)
        end,
        zip(A,B)
    ), ok.



%% TODO: realise this (below) and we're good!

%%assume_wrapper_should_terminate_on_failure(_) ->
%%    %% forall(generator(), assumptions(), expectations(), stats()) -> libtest_result()
%%    forall(
%%        list(customers()),
%%        %% special 'expect' which takes matcher::fun() and action::fun()
%%        expect(
%%            fun(X, XS) -> not(lists:member(X, XS)) end,
%%            will_hold_for(
%%                fun(#customer{ name=Name }|T] ->
%%                    customers:remove_customer(Name, Rest)
%%                end
%%                %% will_hold_for(fun customers:remove_customer/2)
%%            )
%%        )
%%    ),
%%    forall(
%%        {set(),set()},
%%        assuming(fun({X,Y}) -> check_preconditions(X,Y) end),
%%        expect(fun({X,Y}) -> equal(sets:union(X,Y), sets:union(Y,X)) end)
%%    ),
%%    ?FORALL({X,Y}, ?IN({set(), set()}),
%%        ?ASSUMING(fun check_preconditions/1),
%%        ?EXPECT(equal(sets:union(X,Y), sets:union(Y,X))),
%%        ?PROVIDED(fun check_postconditions/1)
%%    )
%%    ok.

fail_message_should_adhere_to_expected_format(_) ->
    %% TODO: remove duplication and create library functions to support this...
    C1 = 10000,
    C2 = 27349,
    Range = 100,
    [A,B|_] = map(
        fun(Ceiling) ->
            map(fun(_) -> random:uniform(Ceiling) end,seq(1, Range))
        end,
        [C1,C2]
    ),
    Desc = "a value matching or equal to some constant",
    map(
        fun({X,Y}) ->
			Res = (verify(fun(_) -> ?EX_FAILED(Desc, Y) end, Desc))(Y),
			Actual = libtest:wrap_result(Res),
            Expected = ?MSG_FORMAT(?EXPECTATION_FAILURE_FMT, [Desc, Y]),
            Actual == Expected
        end,
        zip(A,B)
    ).

expect_handles_expressions_consistently_with_funs(_) ->
    ok = expect_that(1, equal_to(1)).

expect_examines_zero_arity_funs_immediately(_) ->
    ShouldMatch = {fun(Val) -> expected =:= Val end, true},
    ok = expect_that(fun() -> expected end, ShouldMatch).

expect_failure_returns_description_with_context(_) ->
    Desc = "a value in the range [1,2,3]",
    Input = hello_world,
    ExpectedMessage = ?MSG_FORMAT(?EXPECTATION_FAILURE_FMT, [Desc, Input]),
    {failed,{ExpectedMessage, _}}
        = expect_that(fun() -> Input end,
            {fun(Val) -> lists:member(Val, lists:seq(1,3)) end, Desc}),
    ok.

%%%%%%%%%%%%%%%%%%%%%% TODO: REVISIT

%%expect_failure(_Config) ->
%%    ok = expect_that(fun() -> 1 = 2 end, should_fail()).
%%
%%unexpected_success(_) ->
%%    Reason = expect_that(fun() -> "doesn't fail!" end, should_fail()),
%%    ct:pal("unexpected_success caught test failure with ~p", [Reason]).
%%
%%assert_fail_should_throw_if_invalid_exception_type_raised() ->
%%    ?CT_TESTDOC("If assert_fail observes a failing fun but with an incorrect error type, it should throw.").
%%
%%assert_fail_should_throw_if_invalid_exception_type_raised(_Config) ->
%%    ExpectedErrorType = error,
%%    MisbehavingFun = fun() -> throw({something, ignored}) end,
%%    try lib_test:assert_fail(MisbehavingFun, ExpectedErrorType) of
%%        _ -> ct:fail("Expected lib_test:assert_fail to raise an exception!")
%%    catch
%%        _:_ -> {ok, failed_as_expected}
%%    end.
%%
%%assert_fail_should_throw_if_invalid_error_data_returned() ->
%%    ?CT_TESTDOC("If assert_fail observes a failing fun but with incorrect error data, it should throw.").
%%
%%assert_fail_should_throw_if_invalid_error_data_returned(_Config) ->
%%    try lib_test:assert_fail(fun() -> throw({notexpected}) end, [throw], {expected, data}) of
%%        _ -> ct:fail("Expected lib_test:assert_fail to raise an exception!")
%%    catch
%%        _:_ -> {ok, failed_as_expected}
%%    end.
%%
%%assert_fail_should_match_error_type() ->
%%    ?CT_TESTDOC("assert_fail should succeed when the error type is correctly specified.").
%%
%%assert_fail_should_match_error_type(_Config) ->
%%    ErrorTypes = [error, throw, exit],
%%    Assertion = fun(Fun, Err) -> lib_test:assert_fail(Fun, Err) end,
%%    ThreeSuccessfulRuns = lists:duplicate(3, {ok, failed_as_expected}),
%%    ThreeSuccessfulRuns =
%%        lists:map(
%%            fun(error)
%%                -> Assertion(fun() -> erlang:error({ignored, data}) end, error)
%%                ;
%%               (exit)
%%                -> Assertion(fun() -> exit({ignored, data}) end, exit)
%%                ;
%%               (throw)
%%                -> Assertion(fun() -> throw({ignored, data}) end, throw)
%%            end,
%%            ErrorTypes
%%        ).
%%
%%assert_fail_should_match_error_data() ->
%%    ?CT_TESTDOC("assert_fail should succeed when the error data is correctly specified.").
%%
%%assert_fail_should_match_error_data(_Config) ->
%%    ErrorData = {you, crazy, person},
%%    {ok, failed_as_expected} = lib_test:assert_fail(
%%        fun() -> throw(ErrorData) end,
%%        throw,
%%        ErrorData
%%    ).
%%
%%assert_true_should_not_explode_in_positive_case(_Config) ->
%%    lib_test:assert_true(abc == abc).
%%
%%assert_true_should_explode_in_negative_case(_Config) ->
%%    lib_test:assert_fail(fun() -> lib_test:assert_true(abc == def) end).
%%
%%assert_false_should_not_explode_in_positive_case(_Config) ->
%%    lib_test:assert_false(abc == acd).
%%
%%assert_false_should_explode_in_negative_case(_Config) ->
%%    lib_test:assert_fail(fun() -> lib_test:assert_false(abc == abc) end).
%%
%%test_trace_calling_function() -> ?CT_TESTDOC("Tracing a calling function without raising an exception.").
%%%% @todo make this actually test something!?
%%test_trace_calling_function(_Config) -> io:format("~p", [lib_test:trace_calling_function()]).
