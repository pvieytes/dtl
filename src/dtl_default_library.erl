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
         comment/2,
         extends/2, render_extends/2,
         load/2]).

registered_tags() -> [block,
                      comment,
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

%% @doc Create or override a named section of a template.
-spec block(dtl_parser:parser(), dtl_lexer:token()) ->
    {ok, dtl_node:unode(), dtl_parser:parser()}
        | {error, {badarg, block_tag}
                | {block_redefined, binary()}}.
block(Parser, Token) ->
    case dtl_parser:split_token(Token) of
        [_Cmd, Name] ->
            Blocks = dtl_parser:blocks(Parser),
            case lists:member(Name, Blocks) of
                true ->
                    {error, {block_redefined, Name}};
                false ->
                    Parser2 = dtl_parser:set_blocks(Parser, [Name|Blocks]),
                    {ok, Nodes, Parser3} = dtl_parser:parse(Parser2, [endblock]),
                    Node = dtl_node:new("block", {?MODULE, render_block}),
                    Node2 = dtl_node:set_nodelist(Node, lists:reverse(Nodes)),
                    Node3 = dtl_node:set_state(Node2, Name),
                    {ok, Node3, dtl_parser:delete_first_token(Parser3)}
            end;
        _ ->
            {error, {badarg, block_tag}}
    end.

-spec render_block(dtl_node:unode(), dtl_context:context()) ->
    {binary(), dtl_context:context()}.
render_block(Node, Ctx) ->
    Ctx2 = dtl_context:push(Ctx),
    RenderCtx = dtl_context:render_context(Ctx2),
    BlockCtx = dtl_context:render_fetch(RenderCtx, ?BLOCK_CONTEXT_KEY),
    {Rendered, FinalCtx} = case BlockCtx of
        undefined ->
            Ctx3 = dtl_context:set(Ctx2, block, Node),
            {ok, Bin, Ctx4} = dtl_node:render_list(dtl_node:nodelist(Node), Ctx3),
            {Bin, Ctx4};
        _ ->
            Name = dtl_node:state(Node),
            case dict:find(Name, BlockCtx) of
                error ->
                    Ctx3 = dtl_context:set(Ctx2, block, Node),
                    {ok, Bin, Ctx4} = dtl_node:render_list(dtl_node:nodelist(Node), Ctx3),
                    {Bin, Ctx4};
                {ok, Found} ->
                    [Node2|_] = lists:reverse(Found),
                    Ctx3 = dtl_context:set(Ctx2, block, Node2),
                    {ok, Bin, Ctx4} = dtl_node:render_list(dtl_node:nodelist(Node2), Ctx3),
                    RenderCtx2 = dtl_context:render_context(Ctx4),
                    BlockCtx2 = dtl_context:render_fetch(RenderCtx2, ?BLOCK_CONTEXT_KEY),
                    LatestBlocks = case dict:find(Name, BlockCtx2) of
                        error -> [];
                        {ok, Latest} -> Latest
                    end,
                    BlockCtx3 = dict:store(Name, lists:reverse([Node|LatestBlocks]), BlockCtx2),
                    RenderCtx3 = dtl_context:set(RenderCtx2, ?BLOCK_CONTEXT_KEY, BlockCtx3),
                    Ctx5 = dtl_context:set_render_context(Ctx4, RenderCtx3),
                    {Bin, Ctx5}
            end
    end,
    {Rendered, dtl_context:pop(FinalCtx)}.

%% @doc Inherit one template from another. Child template must define
%%      all content in {% block Name %} tags, which will override any block
%%      with the same name in parent templates.
-spec extends(dtl_parser:parser(), dtl_lexer:token()) ->
    {ok, dtl_node:unode(), dtl_parser:parser()}
        | {error, {template_not_found, binary()}
                | {badarg, extends_tag}
                | multiple_extends_tags}.
extends(Parser, Token) ->
    %% TODO: Validate that only one {% extends %} occurs in document.
    %% TODO: Validate that {% extends %} is the first tag in the
    %%       template.
    case dtl_parser:split_token(Token) of
        [_Cmd, RawName] ->
            {ok, Nodes, Parser2} = dtl_parser:parse(Parser),
            case dtl_node:get_nodes_by_type_from_list(Nodes, extends) of
                [] ->
                    %% The name of the template this inherits from.
                    ParentName = dtl_filter:parse(RawName, Parser),
                    Blocks = dtl_node:get_nodes_by_type_from_list(Nodes, block),
                    State = {ParentName, Blocks},
                    Node = dtl_node:new("extends", {?MODULE, render_extends}),
                    Node2 = dtl_node:set_nodelist(Node, Nodes),
                    Node3 = dtl_node:set_state(Node2, State),
                    {ok, Node3, Parser2};
                _ -> {error, multiple_extends_tags}
            end;
        %% This tag takes exactly one argument.
        _ -> {error, {badarg, extends_tag}}
    end.

-spec render_extends(dtl_node:unode(), dtl_context:context()) ->
    {binary(), dtl_context:context()}.
render_extends(Node, Ctx) ->
    {ParentName, Blocks} = dtl_node:state(Node),
    Parent = get_parent(ParentName, Ctx),
    ParentNodes = dtl_template:nodelist(Parent),
    BlockSpecs = blocks_to_plist(Blocks),
    BlockSpecs2 = blocks_to_plist(get_parent_blocks(ParentNodes)),
    {BlockCtx, RenderCtx} = block_context(dtl_context:render_context(Ctx)),
    BlockCtx2 = add_blocks(BlockCtx, BlockSpecs),
    BlockCtx3 = add_blocks(BlockCtx2, BlockSpecs2),
    RenderCtx2 = dtl_context:set(RenderCtx, ?BLOCK_CONTEXT_KEY, BlockCtx3),
    Ctx2 = dtl_context:set_render_context(Ctx, RenderCtx2),
    {ok, Bin, Ctx3} = dtl_template:render(Parent, Ctx2),
    {Bin, Ctx3}.

%% TODO: Reject empty template name/take appropriate action when
%%       template is not found.
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

%% Equivalent of dict([tuple()]) in Python, preserving the last of
%% duplicate key values.
blocks_to_plist(Blocks) ->
    lists:foldl(fun (Block, PList) ->
        Name = dtl_node:state(Block),
        [{Name, Block}|proplists:delete(Name, PList)]
    end, [], Blocks).

get_parent_blocks(Nodes) ->
    get_parent_blocks(Nodes, Nodes).
get_parent_blocks([Node|Rest], Nodes) ->
    case dtl_node:name(Node) of
        "text" -> get_parent_blocks(Rest, Nodes);
        "extends" -> [];
        _ ->
            dtl_node:get_nodes_by_type_from_list(Nodes, block)
    end;
get_parent_blocks([], _) -> [].

%% Fetches the block context, which is used to keep track of which
%% blocks templates have been defined so far.
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

-spec add_blocks(dict(), [block_spec()]) -> dict().
add_blocks(BlockCtx, [{Name, Node}|Specs]) ->
    Existing = case dict:find(Name, BlockCtx) of
        error -> [];
        {ok, Found} -> Found
    end,
    add_blocks(dict:store(Name, [Node|Existing], BlockCtx), Specs);
add_blocks(BlockCtx, []) -> BlockCtx.

%% @doc Loads tag: `{% load library_name %}' where `library_name' is a
%%      module implementing the `dtl_library' interface.
-spec load(dtl_parser:parser(), dtl_lexer:token()) ->
    {ok, dtl_node:unode(), dtl_parser:parser()}
        | {error, {missing_library, binary()} | load_tag_syntax_error}.
load(Parser, Token) ->
    case dtl_parser:split_token(Token) of
        [_Cmd, LibBin] ->
            case dtl_string:safe_list_to_atom(binary_to_list(LibBin)) of
                error -> {error, {missing_library, LibBin}};
                Lib ->
                    Parser2 = dtl_parser:add_library(Parser, Lib),
                    {ok, dtl_node:new("load"), Parser2}
            end;

        %% TODO: Validate requested item, only adding that one item to
        %%       the parser.
        [_Cmd, _Item, <<"from">>, LibBin] ->
            case dtl_string:safe_list_to_atom(binary_to_list(LibBin)) of
                error -> {error, {missing_library, LibBin}};
                Lib ->
                    Parser2 = dtl_parser:add_library(Parser, Lib),
                    {ok, dtl_node:new("load"), Parser2}
            end;

        _ -> {error, load_tag_syntax_error}
    end.

comment(Parser, _Token) ->
    Parser2 = dtl_parser:skip_past(Parser, endcomment),
    Node = dtl_node:new("comment", fun (_, _) -> <<>> end),
    {ok, Node, Parser2}.
