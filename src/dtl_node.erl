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
%%
%%      Definitions of core node types (text, variable, etc.).
-module(dtl_node).

%% @doc Nodes, the building blocks of templates. Nodes themselves may
%%      contain lists of other nodes, so template rendering is
%%      recursive.
%%
%%      Nodes with no renderer will not attempt to be rendererd.
-record(unode, {
    %% name is for debugging
    name :: list(),
    %% Convenient state field. Block tags that consume tokens need this.
    nodelist = [] :: [tnode()],
    %% Renderer callback. If not provided, this won't render.
    renderer :: {atom(), atom()} | fun() | undefined,
    %% State bucket.
    state
}).

-opaque unode() :: #unode{}.
-type tnode() :: unode() | binary() | list().

-export([name/1,
         new/1,
         new/2,
         new_var/1,
         nodelist/1,
         render/2,
         render_list/2,
         render_var/2,
         set_nodelist/2,
         set_renderer/2,
         set_state/2,
         state/1]).
-export_type([tnode/0,
              unode/0]).

new(Name) -> new(Name, undefined).
new(Name, Renderer) -> #unode{name = Name, renderer = Renderer}.

name(Node) -> Node#unode.name.

set_renderer(Node, Renderer) -> Node#unode{renderer = Renderer}.

set_state(Node, State) -> Node#unode{state = State}.

state(Node) -> Node#unode.state.

set_nodelist(Node, Nodes) -> Node#unode{nodelist = Nodes}.

nodelist(Node) -> Node#unode.nodelist.

%% @doc Renders a list of nodes.
-spec render_list([tnode()], dtl_context:context()) ->
    {ok, [binary()]}.
render_list(NodeList, Ctx) ->
    {ok, render_list(NodeList, Ctx, [])}.

-spec render_list([tnode()], dtl_context:context(), [binary()]) ->
    [binary()].
%% Skip no-renderer.
render_list([#unode{renderer = undefined}|NodeList], Ctx, Bits) ->
    render_list(NodeList, Ctx, Bits);
render_list([Node|NodeList], Ctx, Bits) ->
    render_list(NodeList, Ctx, [render(Node, Ctx)|Bits]);
render_list([], _Ctx, Bits) -> lists:reverse(Bits).

%% @doc Renders a single node.
-spec render(tnode(), dtl_context:context()) -> binary().
render(Node = #unode{renderer = {M, F}}, Ctx) ->
    M:F(Node, Ctx);
render(Node = #unode{renderer = Fun}, Ctx) ->
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
    #unode{renderer = {?MODULE, render_var},
           state = FilterExpr}.

%% @doc Variable node renderer.
-spec render_var(tnode(), dtl_context:context()) -> binary().
render_var(#unode{state = FilterExpr}, Ctx) ->
    var_to_binary(dtl_filter:resolve_expr(FilterExpr, Ctx)).

-spec var_to_binary(term()) -> binary().
%% This behavior may be configurable.
var_to_binary(undefined) -> dtl:setting(empty_term);
var_to_binary(T) when is_binary(T) -> T;
var_to_binary(T) -> list_to_binary(io_lib:format("~w", [T])).
