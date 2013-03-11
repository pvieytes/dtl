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

%% @doc Default tags and templates.
-module(dtl_default_library).
-behaviour(dtl_library).

-type block_context() :: dict().
-type block_spec() :: {list(), dtl_node:unode()}.

-export([registered_tags/0,
         registered_filters/0]).

%% Filters
-export([addslashes/1,
         capfirst/1,
         lower/1,
         upper/1]).

%% Tags
-export([block/2, render_block/2,
         extends/2, render_extends/2,
         load/2]).

registered_tags() -> [block,
                      extends,
                      load].
registered_filters() -> [addslashes,
                         capfirst,
                         lower,
                         upper].

-define(BLOCK_CONTEXT_KEY, block_context).

%%
%% Filters
%%

%% @doc Adds backslash prefix to single quotes, double quotes, and
%%      backslashes. {{ "'\"\\" }} -> "\\'\\\"\\\\".
-spec addslashes(binary()) -> binary().
addslashes(Bin) ->
    binary:replace(Bin, [<<"\\">>, <<"'">>, <<"\"">>], <<"\\">>,
                   [global, {insert_replaced, 1}]).

%% @doc Capitalizes the first character of the input.
%%      {{ "pig"|capfirst }} -> "Pig".
-spec capfirst(binary()) -> binary().
capfirst(Bin) ->
    [C|S] = binary_to_list(Bin),
    list_to_binary([ux_char:to_upper(C)|S]).

%% @doc Converts upper to lowercase. {{ "FOO"|lower }} -> "foo".
-spec lower(binary()) -> binary().
lower(Bin) ->
    list_to_binary(ux_string:to_lower(binary_to_list(Bin))).

%% @doc Converts lowercase to uppercase. {{ "foo"|upper }} -> "FOO".
-spec upper(binary()) -> binary().
upper(Bin) ->
    list_to_binary(ux_string:to_upper(binary_to_list(Bin))).

%%
%% Tags
%%

block(Parser, _Token) ->
    {ok, Nodes, Parser2} = dtl_parser:parse(Parser, [endblock]),
    Node = dtl_node:new("block", {?MODULE, render_block}),
    Node2 = dtl_node:set_nodelist(Node, Nodes),
    {ok, Node2, dtl_parser:delete_first_token(Parser2)}.

render_block(_Node, _Ctx) ->
    <<>>.

%% @doc Inherit one template from another. Child template must define
%%      all content in {% block Name %} tags, which will override any block
%%      with the same name in parent templates.
-spec extends(dtl_parser:parser(), dtl_lexer:token()) ->
    {ok, dtl_node:unode(), dtl_parser:parser()}
        | {error, {template_not_found, binary()}
                | {badarg, extends_tag}}.
extends(Parser, Token) ->
    %% TODO: Validate that only one {% extends %} occurs in document.
    %% TODO: Validate that {% extends %} is the first tag in the
    %%       template.
    case dtl_parser:split_token(Token) of
        [_Cmd, RawName] ->
            %% The name of the template this inherits from.
            ParentExpr = dtl_filter:parse(RawName, Parser),
            Node = dtl_node:new("extends", {?MODULE, render_extends}),
            {ok, Nodes, Parser2} = dtl_parser:parse(Parser),
            Node2 = dtl_node:set_nodelist(Node, Nodes),
            Node3 = dtl_node:set_state(Node2, ParentExpr),
            {ok, Node3, Parser2};
        %% This tag takes exactly one argument.
        _ -> {error, {badarg, extends_tag}}
    end.

-spec render_extends(dtl_node:unode(), dtl_context:context()) ->
    {binary(), dtl_context:context()}.
render_extends(Node, Ctx) ->
    Nodes = dtl_node:nodelist(Node),
    ParentExpr = dtl_node:state(Node),
    Parent = get_parent(ParentExpr, Ctx),
    ParentNodes = dtl_template:nodelist(Parent),
    Blocks = block_specs(Nodes) ++ block_specs(ParentNodes),
    RenderCtx = dtl_context:render_context(Ctx),
    Ctx2 = dtl_context:set_render_context(Ctx, add_blocks(RenderCtx, Blocks)),
    {ok, Bin, Ctx3} = dtl_template:render(Parent, Ctx2),
    {Bin, Ctx3}.

block_specs(Nodes) ->
    [{dtl_node:name(Block), Block}
        || Node <- Nodes,
           Block <- dtl_node:get_nodes_by_type(Node, block)].

%% Fetches the block context, which is used to keep track of which
%% blocks templates have defined so far.
-spec block_context(dtl_context:context()) ->
    {block_context(), dtl_context:context()}.
block_context(RenderCtx) ->
    case dtl_context:render_fetch(RenderCtx, ?BLOCK_CONTEXT_KEY) of
        undefined ->
            BlockCtx = dict:new(),
            RenderCtx2 = dtl_context:set(RenderCtx, ?BLOCK_CONTEXT_KEY,
                                         BlockCtx),
            {BlockCtx, RenderCtx2};
        BlockCtx -> {BlockCtx, RenderCtx}
    end.

-spec add_blocks(dtl_context:context(), [block_spec()]) ->
    dtl_context:context().
add_blocks(RenderCtx, []) -> RenderCtx;
add_blocks(RenderCtx, [{Name, Node}|Blocks]) ->
    {Ctx, RenderCtx2} = block_context(RenderCtx),
    Existing = case dict:find(Name, Ctx) of
        error -> [];
        Named -> Named
    end,
    RenderCtx3 = dtl_context:set(RenderCtx2, ?BLOCK_CONTEXT_KEY,
                                 dict:store(Name, [Node|Existing], Ctx)),
    add_blocks(RenderCtx3, Blocks).

%% TODO: Reject empty template name.
-spec get_parent(dtl_filter:expr(), dtl_context:context()) ->
    dtl_template:template().
get_parent(Expr, Ctx) ->
    Tpl = dtl_filter:resolve_expr(Expr, Ctx),
    case dtl_template:is_template(Tpl) of
        true -> Tpl;
        false ->
            {ok, Tpl2} = dtl_loader:get_template(binary_to_list(Tpl)),
            Tpl2
    end.

%% @doc Loads tag: `{% load library_name %}' where `library_name' is a
%%      module implementing the `dtl_library' interface.
-spec load(dtl_parser:parser(), dtl_lexer:token()) ->
    {ok, dtl_node:unode(), dtl_parser:parser()}
        | {error, missing_library | load_tag_syntax_error}.
load(Parser, Token) ->
    case dtl_parser:split_token(Token) of
        [_Cmd, LibBin] ->
            case dtl_string:safe_list_to_atom(binary_to_list(LibBin)) of
                error -> {error, missing_library};
                Lib ->
                    Parser2 = dtl_parser:add_library(Parser, Lib),
                    {ok, dtl_node:new("load"), Parser2}
            end;
        _ -> {error, load_tag_syntax_error}
    end.
