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

%% @doc Common things needed for tokenizing and parsing Django
%%      templates.

-define(BLOCK_TAG_START, "{%").
-define(BLOCK_TAG_END, "%}").
-define(VARIABLE_TAG_START, "{{").
-define(VARIABLE_TAG_END, "}}").
-define(COMMENT_TAG_START, "{#").
-define(COMMENT_TAG_END, "#}").

-define(SINGLE_BRACE_START, "{").
-define(SINGLE_BRACE_END, "}").

-define(TOKEN_TEXT, text).
-define(TOKEN_VAR, var).
-define(TOKEN_BLOCK, block).
-define(TOKEN_COMMENT, comment).

-define(FIELD_SEPARATOR, "|").
-define(FIELD_ARGUMENT_SEPARATOR, ":").
-define(VARIABLE_ATTRIBUTE_SEPARATOR, ".").
-define(TRANSLATOR_COMMENT_MARK, "Translators").

-define(FILTER_SEP, "|").
-define(FILTER_ARG_SEP, ":").

-record(dtl_parser, {
    tokens :: [dtl_lexer:token()],
    tags :: dict(),
    filters :: dict()
}).

-record(dtl_filter_expr, {
    var :: term(),
    token :: binary(),
    filters = [] :: [dtl_filter:filter()]
}).
