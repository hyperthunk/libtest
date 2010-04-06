%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(libtest_matchers_SUITE).
-author('Tim Watson <watson.timothy@gmail.com>').

%% compilation directives
-compile(export_all).

%% stdlib/kernel imports
-import(ct).
-import(io).

%% stdlib/kernel imports
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/libtest.hrl").

%% public api exports

-import(libtest_matchers, [equal_to/1, is/1]).

%% -import(lists, [map/2, seq/2, flatten/1, zip/2]).
%% -import(io_lib, [format/2]).

-define(FAIL, fun(_) -> false end).
-define(PASS, fun(_) -> true end).

all() -> [commutativity_of_equal_to_and_is_for_values]. %% ?CT_REGISTER_TESTS(?MODULE).

commutativity_of_equal_to_and_is_for_values(_) ->
    Value = 1,
    {X,Y} = is(equal_to(Value)),
    {X2,Y2} = equal_to(Value),
    A = X(Value), B = X2(Value),
    {A, Y} = {B, Y2}.


