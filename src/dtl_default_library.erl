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

-record(if_parser, {
    current :: if_cond_token(),
    tokens :: [if_cond_token()],
    pos = 1 :: integer()
}).

-record(if_cond_token, {
    type :: if_cond_type(),
    value :: term(),
    first :: if_cond_token(),
    second :: if_cond_token(),
    lbp = 0 :: integer(),
    text :: list(),
    id :: binary()
}).

-type block_context() :: dict().
-type block_spec() :: {Name :: list(),
                       Node :: dtl_node:unode()}.
-type if_cond_token() :: #if_cond_token{}.
-type if_cond_type() :: infix | prefix | literal.

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
         for/2, render_for/2,
         'if'/2, render_if/2,
         ifchanged/2, render_ifchanged/2,
         ifequal/2, ifnotequal/2, render_ifequal/2,
         load/2]).

%% {% if %} operators. All are binary except for <<"not">>, and all
%% accept a context.
-export([if_and/3,
         if_eq/3,
         if_exact_eq/3,
         if_exact_neq/3,
         if_gt/3,
         if_gte/3,
         if_in/3,
         if_lt/3,
         if_lte/3,
         if_neq/3,
         if_not/2,
         if_not_in/3,
         if_or/3]).

registered_tags() -> [block,
                      comment,
                      extends,
                      for,
                      'if',
                      ifchanged,
                      ifequal,
                      ifnotequal,
                      load].
registered_filters() -> [addslashes,
                         capfirst,
                         lower,
                         upper].

-define(BLOCK_CONTEXT_KEY, block_context).

-define(IF_OPERATORS, [
    {<<"or">>,     {infix, 6, if_or}},
    {<<"and">>,    {infix, 7, if_and}},
    {<<"not">>,    {prefix, 8, if_not}},
    {<<"in">>,     {infix, 9, if_in}},
    {<<"not in">>, {infix, 9, if_not_in}},
    {<<"=">>,      {infix, 10, if_eq}},
    {<<"==">>,     {infix, 10, if_eq}},
    {<<"=:=">>,    {infix, 10, if_exact_eq}},
    {<<"!=">>,     {infix, 10, if_neq}},
    {<<"/=">>,     {infix, 10, if_neq}},
    {<<"=/=">>,    {infix, 10, if_exact_neq}},
    {<<">">>,      {infix, 10, if_gt}},
    {<<">=">>,     {infix, 10, if_gte}},
    {<<"<">>,      {infix, 10, if_lt}},
    {<<"<=">>,     {infix, 10, if_lte}},
    {<<"=<">>,     {infix, 10, if_lte}}
]).

-define(IF_BINARY(Fun, Op),
        Fun(X, Y, Ctx) -> if_eval_cond(X, Ctx) Op if_eval_cond(Y, Ctx)).

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
                    Node2 = dtl_node:set_nodelist(Node, Nodes),
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

%% @doc Comment tag. Everything between {% comment %} and
%%      {% endcomment %} is dropped from the document.
comment(Parser, _Token) ->
    Parser2 = dtl_parser:skip_past(Parser, endcomment),
    Node = dtl_node:new("comment", fun (_, _) -> <<>> end),
    {ok, Node, Parser2}.

%% @doc If/elif/else/endif tag.
'if'(Parser, Token) ->
    [_Cmd|Bits] = dtl_parser:split_token(Token),
    Cond = parse_if(Parser, Bits),
    {ok, Nodes, Parser2} = dtl_parser:parse(Parser, [elif, else, endif]),
    CondBodies = [{Cond, Nodes}],
    {NextToken, Parser3} = dtl_parser:next_token(Parser2),
    {CondBodies2, Parser4} = if_elif_else(NextToken, Parser3, CondBodies),
    Node = dtl_node:new("if", {?MODULE, render_if}),
    Node2 = dtl_node:set_state(Node, CondBodies2),
    Node3 = dtl_node:set_nodelist(Node2, [N || {_Cond, Ns} <- CondBodies2, N <- Ns]),
    {ok, Node3, Parser4}.

if_elif_else(Token = {_, <<"elif", _/binary>>}, Parser, CondBodies) ->
    [_Cmd|Bits] = dtl_parser:split_token(Token),
    Cond = parse_if(Parser, Bits),
    {ok, Nodes, Parser2} = dtl_parser:parse(Parser, [elif, else, endif]),
    {Next, Parser3} = dtl_parser:next_token(Parser2),
    if_elif_else(Next, Parser3, [{Cond, Nodes}|CondBodies]);
if_elif_else({_, <<"else">>}, Parser, CondBodies) ->
    {ok, Nodes, Parser2} = dtl_parser:parse(Parser, [endif]),
    {Next, Parser3} = dtl_parser:next_token(Parser2),
    if_elif_else(Next, Parser3, [{undefined, Nodes}|CondBodies]);
if_elif_else({_, <<"endif">>}, Parser, CondBodies) ->
    {lists:reverse(CondBodies), Parser};
if_elif_else(_, _, _) ->
    {error, {if_tag, missing_endif}}.

parse_if(Parser, Bits) ->
    Mapped = mapped_tokens(Bits, [], Parser),
    IfParser = #if_parser{tokens = Mapped},
    IfParser2 = move_next_token(IfParser),
    {Token, IfParser3} = parse_if_expr(IfParser2),
    case IfParser3#if_parser.current#if_cond_token.type of
        ending -> Token;
        _ -> {error, {if_tag, unused_token, Token}}
    end.

%% Group "not in" as a single operator.
mapped_tokens([<<"not">>|[<<"in">>|Ts]], Mapped, Parser) ->
    mapped_tokens(Ts, [if_token(<<"not in">>, Parser)|Mapped], Parser);
mapped_tokens([T|Ts], Mapped, Parser) ->
    mapped_tokens(Ts, [if_token(T, Parser)|Mapped], Parser);
mapped_tokens([], Mapped, _Parser) ->
    lists:reverse(Mapped).

-spec if_token(dtl_lexer:token(), dtl_parser:parser()) ->
    if_cond_token().
if_token(V, Parser) ->
    case proplists:get_value(V, ?IF_OPERATORS) of
        undefined ->
            #if_cond_token{type = literal,
                           value = dtl_filter:parse(V, Parser),
                           text = V,
                           id = <<"literal">>};
        {Type, Bp, Cbk} ->
            #if_cond_token{type = Type,
                           lbp = Bp,
                           value = Cbk,
                           id = V}
    end.

move_next_token(Parser = #if_parser{tokens = Tokens, pos = Pos})
        when length(Tokens) < Pos ->
    Token = #if_cond_token{type = ending},
    Parser#if_parser{current = Token};
move_next_token(Parser = #if_parser{tokens = Tokens, pos = Pos}) ->
    Token = lists:nth(Pos, Tokens),
    Parser#if_parser{current = Token, pos = Pos + 1}.

parse_if_expr(Parser) ->
    parse_if_expr(Parser, 0).

parse_if_expr(Parser = #if_parser{current = Token}, Rbp) ->
    Parser2 = move_next_token(Parser),
    {Left, Parser3} = if_parser_nud(Token, Parser2),
    do_parse_if_expr(Parser3, Left, Rbp).

do_parse_if_expr(Parser = #if_parser{current = #if_cond_token{lbp = Lbp}},
        Left, Rbp) when Rbp >= Lbp ->
    {Left, Parser};
do_parse_if_expr(Parser = #if_parser{current = Token}, Left, Rbp) ->
    Parser2 = move_next_token(Parser),
    {Left2, Parser3} = if_parser_led(Token, Left, Parser2),
    do_parse_if_expr(Parser3, Left2, Rbp).

if_parser_nud(Token = #if_cond_token{type = literal}, Parser) ->
    {Token, Parser};
if_parser_nud(Token = #if_cond_token{type = prefix, lbp = Bp}, Parser) ->
    {First, Parser2} = parse_if_expr(Parser, Bp),
    {Token#if_cond_token{first = First}, Parser2};
if_parser_nud(#if_cond_token{type = ending}, _Parser) ->
    {error, {if_tag, unexpected_end_of_expression}};
if_parser_nud(#if_cond_token{id = Id}, _Parser) ->
    {error, {if_tag, unexpected_prefix, Id}}.

if_parser_led(Token = #if_cond_token{type = infix, lbp = Bp}, Left, Parser) ->
    {Second, Parser2} = parse_if_expr(Parser, Bp),
    {Token#if_cond_token{first = Left, second = Second}, Parser2};
if_parser_led(#if_cond_token{id = Id}, _Left, _Parser) ->
    {error, {if_tag, unexpected_infix, Id}}.

render_if(Node, Ctx) ->
    do_render_if(dtl_node:state(Node), Ctx, []).

%% Evaluate each condition and render its node list if it evaluates to
%% `true'. Render items with no condition ({% else %} blocks)
%% automatically.
do_render_if([{undefined, Nodes}|Conds], Ctx, Rendered) ->
    {ok, Bin, Ctx2} = dtl_node:render_list(Nodes, Ctx),
    do_render_if(Conds, Ctx2, [Bin|Rendered]);
do_render_if([{Cond, Nodes}|Conds], Ctx, Rendered) ->
    {Rendered2, Ctx2} = case if_true(if_eval_cond(Cond, Ctx)) of
        true ->
            {ok, Bin, Ctx3} = dtl_node:render_list(Nodes, Ctx),
            {[Bin|Rendered], Ctx3};
        false ->
            {Rendered, Ctx}
    end,
    do_render_if(Conds, Ctx2, Rendered2);
do_render_if([], _Ctx, Rendered) ->
    lists:reverse(Rendered).

if_eval_cond(#if_cond_token{type = literal, value = FilterExpr}, Ctx) ->
    dtl_filter:resolve_expr(FilterExpr, Ctx);
if_eval_cond(#if_cond_token{type = prefix, value = Fun, first = X}, Ctx) ->
    ?MODULE:Fun(X, Ctx);
if_eval_cond(#if_cond_token{type = infix, value = Fun, first = X, second = Y}, Ctx) ->
    ?MODULE:Fun(X, Y, Ctx).

%%
%% {% if %} operators.
%%
if_or(X, Y, Ctx) ->
    if_true(if_eval_cond(X, Ctx)) or if_true(if_eval_cond(Y, Ctx)).

if_and(X, Y, Ctx) ->
    if_true(if_eval_cond(X, Ctx)) and if_true(if_eval_cond(Y, Ctx)).

if_not(X, Ctx) ->
    not if_true(if_eval_cond(X, Ctx)).

if_in(X, Y, Ctx) ->
    lists:member(if_eval_cond(X, Ctx),
                 if_eval_cond(Y, Ctx)).

if_not_in(X, Y, Ctx) -> not if_in(X, Y, Ctx).

?IF_BINARY(if_eq, ==).
?IF_BINARY(if_exact_eq, =:=).
?IF_BINARY(if_neq, /=).
?IF_BINARY(if_exact_neq, =/=).
?IF_BINARY(if_lt, <).
?IF_BINARY(if_lte, =<).
?IF_BINARY(if_gt, >).
?IF_BINARY(if_gte, >=).

if_true([]) -> false;
if_true(0) -> false;
if_true(0.0) -> false;
if_true(<<>>) -> false;
if_true(false) -> false;
if_true(undefined) -> false;
if_true({}) -> false;
if_true(_) -> true.

%% {% ifequal %} and {% ifnotequal %} tags.
ifequal(Parser, Token) ->
    do_ifequal(Parser, Token, true, endifequal).

ifnotequal(Parser, Token) ->
    do_ifequal(Parser, Token, false, endifnotequal).

do_ifequal(Parser, Token, Equal, End) ->
    case dtl_parser:split_token(Token) of
        [_Cmd, X, Y] ->
            {ok, TrueNodes, Parser2} = dtl_parser:parse(Parser,
                                                        [else, End]),
            {{_, Bin}, Parser3} = dtl_parser:next_token(Parser2),
            {FalseNodes, Parser4} = case Bin of
                <<"else">> ->
                    {ok, Ns, Parser5} = dtl_parser:parse(Parser3,
                                                         [End]),
                    {Ns, dtl_parser:delete_first_token(Parser5)};
                _ ->
                    {[], Parser3}
            end,
            Node = dtl_node:new("ifequal", {?MODULE, render_ifequal}),
            Nodes = [N || NL <- [TrueNodes, FalseNodes], N <- NL],
            Node2 = dtl_node:set_nodelist(Node, Nodes),
            X1 = dtl_filter:parse(X, Parser4),
            Y1 = dtl_filter:parse(Y, Parser4),
            Node3 = dtl_node:set_state(Node2, {Equal, X1, Y1,
                                               TrueNodes, FalseNodes}),
            {ok, Node3, Parser4};
        _ -> {error, {badarg, ifequal_tag}}
    end.

render_ifequal(Node, Ctx) ->
    {Equal, X, Y, TrueNodes, FalseNodes} = dtl_node:state(Node),
    X1 = dtl_filter:resolve_expr(X, Ctx),
    Y1 = dtl_filter:resolve_expr(Y, Ctx),
    Passes = case Equal of
        true -> X1 == Y1;
        false -> X1 /= Y1
    end,
    Nodes = case Passes of
        true -> TrueNodes;
        false -> FalseNodes
    end,
    {ok, Bin, Ctx2} = dtl_node:render_list(Nodes, Ctx),
    {Bin, Ctx2}.

%% {% for %}.
for(Parser, Token) ->
    case dtl_parser:split_token(Token) of
        [_Cmd|BaseVars = [_, _, _|_]] ->
            case lists:reverse(BaseVars) of
                [<<"reversed">>, From, <<"in">>|Vars] ->
                    do_for(From, Vars, true, Parser);
                [From, <<"in">>|Vars] ->
                    do_for(From, Vars, false, Parser);
                _ ->
                    {error, {badarg, for_tag}}
            end;
        _ ->
            {error, {badarg, for_tag}}
    end.

do_for(From, RawVars, Reversed, Parser) ->
    VarNames = [binary_to_list(V) || V <- lists:reverse(RawVars)],
    Vars = re:split(string:join(VarNames, " "), " *, *",
                    [{return, binary}]),
    %% Django validates that arguments do not contain spaces or are
    %% empty strings here.
    FromExpr = dtl_filter:parse(From, Parser),
    {ok, LoopNodes, Parser2} = dtl_parser:parse(Parser,
                                                [empty, endfor]),
    {EmptyNodes, Parser3} = case dtl_parser:next_token(Parser2) of
        {{_, <<"empty">>}, Parser4} ->
            {ok, Empty, Parser5} = dtl_parser:parse(Parser4, [endfor]),
            {Empty, dtl_parser:delete_first_token(Parser5)};
        {_, Parser4} ->
            {[], Parser4}
    end,
    Nodes = [N || NL <- [LoopNodes, EmptyNodes], N <- NL],
    Node = dtl_node:new("for", {?MODULE, render_for}),
    Node2 = dtl_node:set_nodelist(Node, Nodes),
    Node3 = dtl_node:set_state(Node2, {Vars, FromExpr, Reversed,
                                       LoopNodes, EmptyNodes}),
    {ok, Node3, Parser3}.

render_for(Node, Ctx) ->
    {Vars, FromExpr, Reversed, LoopNodes,
     EmptyNodes} = dtl_node:state(Node),
    ParentLoop = case dtl_context:fetch(Ctx, forloop) of
        undefined -> [];
        P -> P
    end,
    Ctx2 = dtl_context:push(Ctx),
    {Bin, Ctx3} = case dtl_filter:resolve_expr(FromExpr, Ctx2) of
        undefined ->
            render_for_empty(EmptyNodes, Ctx2);
        [] ->
            render_for_empty(EmptyNodes, Ctx2);
        From ->
            ForCtx = dtl_context:fetch(Ctx, forloop, dtl_context:new()),
            ForCtx2 = dtl_context:set(ForCtx, parentloop, ParentLoop),
            Ctx4 = dtl_context:set(Ctx2, forloop, ForCtx2),
            render_for_each(LoopNodes, From, Vars, length(From),
                            Reversed, Ctx4)
    end,
    {Bin, dtl_context:pop(Ctx3)}.

render_for_empty(Nodes, Ctx) ->
    {ok, Bin, Ctx2} = dtl_node:render_list(Nodes, Ctx),
    {Bin, Ctx2}.

render_for_each(Nodes, From, Vars, NVars, true, Ctx) ->
    render_for_each(lists:reverse(Nodes), From, Vars, NVars, Ctx, 0, []);
render_for_each(Nodes, From, Vars, NVars, false, Ctx) ->
    render_for_each(Nodes, From, Vars, NVars, Ctx, 0, []).

render_for_each(_Nodes, [], _Vars, _NVars, Ctx, _I, Bins) ->
    {lists:reverse(Bins), Ctx};
render_for_each(Nodes, [Iter|From], Vars, NVars, Ctx, I, Bins) ->
    LoopVars = case Vars of
        [V] ->
            [{raw_var_to_term(V), Iter}];
        _ ->
            lists:zip([raw_var_to_term(V) || V <- Vars], Iter)
    end,
    ForCtx = dtl_context:fetch(Ctx, forloop, dtl_context:new()),
    Ctx2 = dtl_context:set(Ctx, forloop, dtl_context:set(ForCtx, [
        {counter0, I},
        {counter, I + 1},
        {revcounter, NVars - I},
        {revcounter0, NVars - I - 1},
        {first, I =:= 0},
        {last, I =:= NVars - 1}
    ])),
    Ctx3 = dtl_context:update(Ctx2, LoopVars),
    {ok, Bin, Ctx4} = dtl_node:render_list(Nodes, Ctx3),
    render_for_each(Nodes, From, Vars, NVars, dtl_context:pop(Ctx4),
                    I + 1, [Bin|Bins]).

raw_var_to_term(V) ->
    list_to_atom(binary_to_list(V)).

%% {% ifchanged %}.
ifchanged(Parser, Token) ->
    [_|Bits] = dtl_parser:split_token(Token),
    {ok, TrueNodes, Parser2} = dtl_parser:parse(Parser,
                                                [else, endifchanged]),
    {FalseNodes, Parser3} = case dtl_parser:next_token(Parser2) of
        {{_, <<"else">>}, Parser4} ->
            {ok, Empty, Parser5} = dtl_parser:parse(Parser4,
                                                    [endifchanged]),
            {Empty, dtl_parser:delete_first_token(Parser5)};
        {_, Parser4} ->
            {[], Parser4}
    end,
    Nodes = [N || NL <- [TrueNodes, FalseNodes], N <- NL],
    Values = [dtl_filter:parse(Bit) || Bit <- Bits],
    Node = dtl_node:new("ifchanged", {?MODULE, render_ifchanged}),
    Node2 = dtl_node:set_nodelist(Node, Nodes),
    Node3 = dtl_node:set_state(Node2, {make_ref(), Values, TrueNodes,
                                       FalseNodes}),
    {ok, Node3, Parser3}.

render_ifchanged(Node, Ctx) ->
    {Ref, Values, TrueNodes, FalseNodes} = dtl_node:state(Node),
    {Cur, Ctx2} = case Values of
        [] ->
            {ok, CurBin, Ctx3} = dtl_node:render_list(TrueNodes, Ctx),
            {CurBin, Ctx3};
        _ ->
            {[dtl_filter:resolve_expr(V, Ctx) || V <- Values], Ctx}
    end,
    case ifchanged_changed(Ref, Cur, Ctx2) of
        {true, Ctx4} ->
            {ok, Bin, Ctx5} = dtl_node:render_list(TrueNodes, Ctx4),
            {Bin, Ctx5};
        {false, Ctx4} ->
            case FalseNodes of
                [] ->
                    {<<>>, Ctx4};
                _ ->
                    {ok, Bin, Ctx5} = dtl_node:render_list(FalseNodes,
                                                           Ctx4),
                    {Bin, Ctx5}
            end
    end.

ifchanged_changed(Ref, Cur, Ctx) ->
    ForCtx = dtl_context:fetch(Ctx, forloop),
    Old = dtl_context:fetch(ForCtx, Ref),
    ForCtx2 = dtl_context:set(ForCtx, Ref, Cur),
    Ctx2 = dtl_context:set_ref(Ctx, forloop, ForCtx2),
    Changed = case Old of
        undefined -> true;
        _ -> Old =/= Cur
    end,
    {Changed, Ctx2}.
