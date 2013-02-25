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

-export([new/1,
         parse/1,
         parse/2,
         split_token/1]).

-spec new([dtl_token()]) -> dtl_parser().
new(Tokens) ->
    #dtl_parser{tokens = Tokens,
                tags = [],
                filters = []}.

-spec parse(dtl_parser()) ->
    {ok, dtl_nodelist(), dtl_parser()} | {error, atom()}.
parse(Parse) -> parse(Parse, []).

-spec parse(dtl_parser(), [atom()]) ->
    {ok, dtl_nodelist(), dtl_parser()} | {error, atom()}.
parse(Parser = #dtl_parser{tokens = Tokens}, Until) ->
    io:format("~p ~p~n", [Tokens, Until]),
    parse_until(Parser, Tokens, Until, []).

-spec parse_until(dtl_parser(), [dtl_token()], [atom()], dtl_nodelist()) ->
    {ok, dtl_nodelist(), dtl_parser()} | {error, atom()}.
parse_until(Parser, [{?TOKEN_TEXT, Src}|Tokens], Until, Nodes) ->
    parse_until(Parser, Tokens, Until, [Src|Nodes]);
parse_until(Parser, [{?TOKEN_VAR, Src}|Tokens], Until, Nodes) ->
    FilterExpr = dtl_filter:parse(Src),
    Node = dtl_node:new_var(FilterExpr),
    parse_until(Parser, Tokens, Until, [Node|Nodes]);
%% TODO: Clean up this ugly function ...
parse_until(Parser, AllTokens = [Token = {?TOKEN_BLOCK, Src}|Tokens],
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

split_token(Src) ->
    %% TODO: Add support for _("Text") like in Django.
    dtl_string:smart_split(Src).

run_command(_Parser, _Cmd, _Token) ->
    %% Stub, pending library functions.
    ok.
