%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 Tim Watson.

%% module annotations
-module(libtest.matchers).
-author('Tim Watson <watson.timothy@gmail.com>').

%% stdlib/kernel imports/includes

-import(ets). %% hack to keep cover tool happy
-import(lists).
-import(io_lib, [format/2, write_string/1]).

-include("libtest.hrl").

%% public api exports

-compile(export_all).

%% Calculates the logical conjunction of multiple matchers.
%% Performs shortcut evaulation, subsequent matchers are not called if an earlier predicate fails.
%% all(Matchers) -> ok.

%% Calculates the logical disjunction of multiple matchers.
%% Does not shortcut evaluation; subsequent matchers are always called.
%any(Matchers) when is_list(Matchers) ->
%    fun(Input) ->
%        case lists:dropwhile(
%            fun({match, _}) -> false ; %% keep it
%               ({nomatch, _}) -> true %% drop it
%            end,
%            lists:map(fun(Matcher) -> Matcher(Input) end, Matchers)
%        ) of
%            [H|_] -> H
%            ;
%            [] -> {nomatch, ?MSPEC(Input, 'custom_op')}
%        end
%    end.

%%TODO: write an is_a(Type) matcher to wrap common guard expressions

anything() -> fun(_) -> true end.

is(X) when is_function(X) -> X
;
is({X,_}=Spec) when is_function(X) -> Spec
;
is(X) -> equal_to(X).

is_not(X) when is_function(X) ->
    fun(Y) -> not X(Y) end
;
is_not({X, Desc}) when is_function(X) ->
    {is_not(X), Desc}
;
is_not(X) -> is_not(is(X)).

equal_to(Y) -> {?EQUAL_TO(Y), ?MSG_FORMAT("a value equal to ~p", [Y])}.
