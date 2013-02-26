%% Copyright (c) 2013- Thomas Allen <thomas@oinksoft.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

%% @doc Tests for string utilities.
-module(dtl_string_tests).

-include_lib("eunit/include/eunit.hrl").

escape_test_() ->
    [?_assertEqual("a\\._\\(\\(\\?\\:foo\\)\\)", dtl_string:escape_re("a._((?:foo))")),
     ?_assertEqual("", dtl_string:escape_re("")),
     ?_assertEqual("123", dtl_string:escape_re("123")),
     ?_assertEqual("\\.__\\.\\-\\-", dtl_string:escape_re(".__.--"))].

safe_split_test_() ->
    [?_assertEqual([<<"Hello">>, <<"\"Dr. Evil\",">>, <<"we">>, <<"meet">>, <<"again.">>],
                   dtl_string:smart_split(
                       <<"Hello \"Dr. Evil\", we meet again.">>
                   ))].

safe_list_to_atom_test_() ->
    [?_assertEqual(error, dtl_string:safe_list_to_atom("alkjdlakjdlaksjd")),
     ?_assertEqual(foo, dtl_string:safe_list_to_atom("foo"))].
