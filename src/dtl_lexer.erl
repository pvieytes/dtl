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

%% @doc Template tokenizing functions.
-module(dtl_lexer).

%% Not sure if I should provide a behavior so that this and the debug
%% lexer are guaranteed to share an interface.

-export([tokenize/1]).

-include("dtl_compiler.hrl").

-define(SPLITTER_PARTS, [?BLOCK_TAG_START,
                         ?BLOCK_TAG_END,
                         ?VARIABLE_TAG_START,
                         ?VARIABLE_TAG_END,
                         ?COMMENT_TAG_START,
                         ?COMMENT_TAG_END]).

%% @doc Traverses the provided template source code, generating a list
%%      of tokens describing the basic structure.
-spec tokenize(binary()) -> token().
tokenize(Str) ->
    Bits = re:split(Str, make_splitter()),
    tokenize_bits(Bits, [], false, false).

%% Convenience function to create a regular expression for splitting up
%% template source code.
make_splitter() ->
    Expr = io_lib:format("(~s.*~s|~s.*~s|~s.*~s)", ?SPLITTER_PARTS),
    {ok, Re} = re:compile(Expr),
    Re.

%% Accumulator for tokenized bits.
tokenize_bits([<<>>|Bits], Tokens, InTag, Verbatim) ->
    tokenize_bits(Bits, Tokens, not InTag, Verbatim);
tokenize_bits([Bit|Bits], Tokens, InTag, Verbatim) ->
    {ok, Token, Verbatim2} = make_token(Bit, InTag, Verbatim),
    tokenize_bits(Bits, [Token|Tokens], not InTag, Verbatim2);
tokenize_bits([], Tokens, _InTag, _Verbatim) ->
    lists:reverse(Tokens).

%% Token factory function (stub).
make_token(_Str, _InTag, _Verbatim) ->
    ok.
