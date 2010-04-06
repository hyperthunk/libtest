%% @author Tim Watson <timwatson@munotify.com>
%% @copyright 2007 author.
%% common tests macros and functions

-define(TESTDOC(Doc), [{userdata,[{doc,Doc}]}]).
-define(NOT_IMPLEMENTED, {skip, "Not implemented."}).
%% -compile({parse_transform,  stub_function_transform}).