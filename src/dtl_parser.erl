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

%% @doc Template parsing functions.
-module(dtl_parser).

-include("dtl.hrl").
-include("dtl_compiler.hrl").

-type parser() :: #dtl_parser{}.

-export([add_library/2,
         find_filter/2,
         new/1,
         parse/1,
         parse/2,
         split_token/1]).
-export_type([parser/0]).


%% @doc Creates a new, empty parser.
-spec new([dtl_lexer:token()]) -> parser().
new(Tokens) ->
    Parser = #dtl_parser{tokens = Tokens,
                         filters = dict:new(),
                         tags = dict:new()},
    add_library(Parser, dtl_default_library).

-spec add_library(parser(), atom()) -> parser().
add_library(Parser = #dtl_parser{tags = Tags, filters = Filters}, Mod) ->
    Parser#dtl_parser{filters = dtl_library:add_filters(Mod, Filters),
                      tags = dtl_library:add_tags(Mod, Tags)}.

%% @doc Parses all tokens within the provided parser, returning the
%%      resulting nodelist. See parse/2 for errors.
-spec parse(parser()) ->
    {ok, dtl_node:tnodelist(), parser()} | {error, atom()}.
parse(Parse) -> parse(Parse, []).

%% @doc Parses all tokens within the provided parser until a block tag
%%      token named in the second parameter is encountered.
%%
%%      Errors:
%%
%%      unclosed_block_tag: If the second parameter is non-empty and
%%          none of the candidate block tags are ever encountered.
%%
%%      empty_block_tag: If the parser encounters a block tag with no
%%          contents.
%%
%%      unknown_tag: If an unregistered block tag is encountered.
-spec parse(parser(), [atom()]) ->
    {ok, dtl_node:tnodelist(), parser()} | {error, atom()}.
parse(Parser = #dtl_parser{tokens = Tokens}, Until) ->
    parse_until(Parser, Tokens, Until, []).

-spec parse_until(parser(), [dtl_lexer:token()], [atom()],
        dtl_node:tnodelist()) ->
    {ok, dtl_node:tnodelist(), parser()} | {error, atom()}.
parse_until(Parser, [{?TOKEN_TEXT, Src}|Tokens], Until, Nodes) ->
    parse_until(Parser, Tokens, Until, [Src|Nodes]);
parse_until(Parser, [{?TOKEN_VAR, Src}|Tokens], Until, Nodes) ->
    FilterExpr = dtl_filter:parse(Src, Parser),
    Node = dtl_node:new_var(FilterExpr),
    parse_until(Parser, Tokens, Until, [Node|Nodes]);
%% TODO: Clean up this ugly function ...
parse_until(Parser, AllTokens = [Token = {?TOKEN_BLOCK, Src}|_Tokens],
        Until, Nodes) ->
    case split_token(Src) of
        [] -> {error, empty_block_tag};
        [CmdBin|_Rest] ->
            CmdString = binary_to_list(CmdBin),
            case dtl_string:safe_list_to_atom(CmdString) of
                error -> {error, unknown_tag};
                Cmd ->
                    case lists:member(Cmd, Until) of
                        %% Is in the "parse until" list,
                        true ->
                            {ok, Nodes,
                             Parser#dtl_parser{tokens = AllTokens}};
                        false ->
                            {Node, Parser2} = run_command(Parser, Cmd, Token),
                            %% Start over with tokens because the tag
                            %% function may have modified them.
                            parse_until(Parser2,
                                        Parser2#dtl_parser.tokens,
                                        Until, [Node|Nodes])
                    end
            end
    end;
parse_until(Parser, [], [], Nodes) ->
    {ok, lists:reverse(Nodes), Parser};
parse_until(_Parser, [], _Until, _Nodes) ->
    {error, unclosed_block_tag}.

%% @doc Splits a block tag template token into its constituent parts,
%%      splitting on all whitespace not contained in a "" pair.
-spec split_token(binary()) -> binary().
split_token(Src) ->
    dtl_string:smart_split(Src).

run_command(_Parser, _Cmd, _Token) ->
    %% Stub, pending library functions.
    ok.

-spec find_filter(parser(), binary()) -> {atom(), atom()} | error.
find_filter(#dtl_parser{filters = Filters}, Name) ->
    case dtl_string:safe_list_to_atom(binary_to_list(Name)) of
        error -> error;
        A -> case dict:find(A, Filters) of
            error -> error;
            {ok, Spec} -> Spec
        end
    end.
