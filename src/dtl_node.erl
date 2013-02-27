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

%% @doc Functions for working with template nodes. Nodes are the core
%%      building block of templates, and templates are rendered by
%%      recursively rendering nodelists, which in turn render nodes.
-module(dtl_node).

-export([new_var/1,
         render/2,
         render_list/2,
         render_var/2]).

-include("dtl.hrl").

-type tnode() :: #dtl_node{} | list() | binary().
-type tnodelist() :: [tnode()].
-export_type([tnode/0, tnodelist/0]).

%% @doc Renders a list of nodes.
-spec render_list(tnodelist(), dtl_context:context()) ->
    {ok, [binary()]}.
render_list(NodeList, Ctx) ->
    {ok, render_list(NodeList, Ctx, [])}.

-spec render_list(tnodelist(), dtl_context:context(), [binary()]) ->
    [binary()].
render_list([Node|NodeList], Ctx, Bits) ->
    render_list(NodeList, Ctx, [render(Node, Ctx)|Bits]);
render_list([], _Ctx, Bits) -> lists:reverse(Bits).

%% @doc Renders a single node.
-spec render(tnode(), dtl_context:context()) -> binary().
render(Node = #dtl_node{renderer = {M, F}}, Ctx) ->
    M:F(Node, Ctx);
render(Node = #dtl_node{renderer = Fun}, Ctx) ->
    Fun(Node, Ctx);
render(Node, _Ctx) when is_list(Node) -> list_to_binary(Node);
render(Node, _Ctx) when is_binary(Node) -> Node.

%%
%% Variable nodes: Consist of {{ Var[|filter...] }}.
%
%% See `dtl_filter:parse/2' for more details on this format.
%%

%% @doc Variable node initializer.
-spec new_var(dtl_filter:expr()) -> tnode().
new_var(FilterExpr) ->
    #dtl_node{renderer = {?MODULE, render_var},
              state = FilterExpr}.

%% @doc Variable node renderer.
-spec render_var(tnode(), dtl_context:context()) -> binary().
render_var(#dtl_node{state = FilterExpr}, Ctx) ->
    var_to_binary(dtl_filter:resolve_expr(FilterExpr, Ctx)).

-spec var_to_binary(term()) -> binary().
var_to_binary(T) when is_binary(T) -> T;
%% This behavior may be configurable.
var_to_binary(undefined) -> <<>>;
var_to_binary(T) -> io_lib:format("~w", [T]).
