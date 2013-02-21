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

%% @doc String utilities for implementing the Django Template Language,
%%      ported from Django project source code.
-module(dtl_string).

-export([escape_re/1]).

%% @doc Escape an input string for use within a regular expression (so
%%      that all characters are interpreted literally). Every character
%%      will be escaped except for alphanumeric characters and '_'.
-spec escape_re(list()) -> list().
escape_re(Patt) ->
    lists:flatten(escape_re(Patt, [])).
escape_re([Ch|Patt], Escaped) ->
    escape_re(Patt, [escape_re_char(Ch)|Escaped]);
escape_re([], Escaped) -> lists:reverse(Escaped).
escape_re_char(C) when C < $0;
                       C > $9, C < $A;
                       C > $Z, C < $a, C /= $_;
                       C > $z  -> [$\\, C];
escape_re_char(C) -> C.
