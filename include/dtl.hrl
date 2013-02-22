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

%% @doc Records and types needed throughout the program.

%% @doc Contexts. These maintain a stack of states pushed by different
%%      parts of the program, so that updates to the context data
%%      consist of pushing to the stack rather than destroying existing
%%      data.
-record(dtl_ctx, {
    stack = [] :: [dict()],
    autoescape = true :: boolean(),
    render_context :: dtl_context()
}).

%% @doc Templates, this program's core data type. These are the compiled
%%      representation of string templates and all template rendering
%%      occurs via an internal node list.
-record(dtl_tpl, {
    nodelist = [] :: dtl_nodelist()
}).

%% @doc Nodes, the building blocks of templates. Nodes themselves may
%%      contain lists of other nodes, so template rendering is
%%      recursive.
-record(dtl_node, {
    nodelists = [] :: [dtl_nodelist()]
}).

-type dtl_context() :: #dtl_ctx{}.
-type dtl_template() :: #dtl_tpl{}.
-type dtl_node() :: #dtl_node{}.
-type dtl_nodelist() :: [#dtl_node{}].
