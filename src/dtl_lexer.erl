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

%% Main constituents of the template string splitting regex. Groups of
%% these start and end combinations are captured.
-define(SPLITTER_PARTS, [?BLOCK_TAG_START,
                         ?BLOCK_TAG_END,
                         ?VARIABLE_TAG_START,
                         ?VARIABLE_TAG_END,
                         ?COMMENT_TAG_START,
                         ?COMMENT_TAG_END]).

%% @doc Traverses the provided template source code, generating a list
%%      of tokens describing the basic structure.
-spec tokenize(binary()) -> [dtl_token()].
tokenize(Src) ->
    Bits = re:split(Src, make_splitter()),
    %% Django strips out empty tokens here, we do that when creating the
    %% tokens by discarding the bit.
    tokenize_bits(Bits, [], false, false).

%% Convenience function to create a regular expression for splitting up
%% template source code.
-spec make_splitter() -> {re_pattern, term(), term(), term()}.
make_splitter() ->
    Expr = io_lib:format("(~s.*?~s|~s.*?~s|~s.*?~s)", ?SPLITTER_PARTS),
    {ok, Re} = re:compile(Expr),
    Re.

%% Accumulator for tokenized bits. `InTag' is flipped on each recursion.
%% A "tag" in this case is not a Django template tag, but rather a lexer
%% tag, which is any bit that the splitter captured. The splitter, as
%% defined, results in a list of the form [Tag, Text, Tag, Text, ...].
%%
%% Empty bit, discard it.
-spec tokenize_bits([binary()], [dtl_token()], boolean(), boolean()) -> [dtl_token()].
tokenize_bits([<<>>|Bits], Tokens, InTag, Verbatim) ->
    tokenize_bits(Bits, Tokens, not InTag, Verbatim);
%% Non-empty bit, process it.
tokenize_bits([Bit|Bits], Tokens, InTag, Verbatim) ->
    {Token, Verbatim2} = make_token(Bit, InTag, Verbatim),
    tokenize_bits(Bits, [Token|Tokens], not InTag, Verbatim2);
%% Finished, return the token list.
tokenize_bits([], Tokens, _InTag, _Verbatim) -> lists:reverse(Tokens).

%% Token factory.
-spec make_token(binary(), boolean(), boolean()) -> dtl_token().
%% Check for {% endverbatim %} if in {% verbatim %}.
make_token(Src = <<?BLOCK_TAG_START, Rest/binary>>, true, true) ->
    Stripped = strip_token(Rest),
    EndsVerbatim = Stripped =:= <<"endverbatim">>,
    Token = case EndsVerbatim of
        true -> {?TOKEN_BLOCK, Stripped};
        false -> {?TOKEN_TEXT, Src}
    end,
    {Token, not EndsVerbatim};
%% Throw everything out in {% verbatim %}.
make_token(Src, _InTag, true) -> {{?TOKEN_TEXT, Src}, true};
%% If not in a tag, it's always a text token.
make_token(Bit, false, Verbatim) ->
    {{?TOKEN_TEXT, Bit}, Verbatim};
%% Chop the {{, }}, and extra whitespace off of variable tags.
make_token(<<?VARIABLE_TAG_START, Rest/binary>>, true, Verbatim) ->
    {{?TOKEN_VAR, strip_token(Rest)}, Verbatim};
%% Chop the {%, %}, and extra whitespace off of block tags.
make_token(<<?BLOCK_TAG_START, Rest/binary>>, true, _Verbatim) ->
    Stripped = strip_token(Rest),
    {{?TOKEN_BLOCK, Stripped}, Stripped =:= <<"verbatim">>};
%% Chop the {#, #}, and extra whitespace off of comment tags, saving
%% their contents, if "Translators" occurs in the comment. Otherwise,
%% throw it all out.
%%
%% Django records line number on each step.
make_token(<<?COMMENT_TAG_START, Rest/binary>>, true, _Verbatim) ->
    {?TOKEN_COMMENT, case binary:match(Rest, <<?TRANSLATOR_COMMENT_MARK>>) of
        nomatch -> <<>>;
        {_Pos, _Len} -> strip_token(Rest)
     end}.

%% Strip whitespace and tag marks off the provided token.
%%
%% Assume this is coming from make_token() and the first two characters
%% have already been dropped.
-spec strip_token(binary()) -> binary().
strip_token(Bin) ->
    re:replace(Bin, "^\\s*|\\s*..$", "", [global, {return, binary}]).
