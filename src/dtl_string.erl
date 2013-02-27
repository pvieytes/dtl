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

%% @doc String utilities.
-module(dtl_string).

-export([escape_re/1,
         safe_list_to_atom/1,
         smart_split/1]).

%% Regex for splitting block tag tokens.
-define(TOKEN_PART_MATCHER,
    %% 1. Capture all token parts.
    "((?:"
        %% 1.1. Any number of non-space, ', " characters (spaces and quote
        %%    marks are our delimiters).
        "[^\\s\"]*"

        %% 1.2. Any number of quoted groups, optionally surrounded by
        %%      unquoted, non-quote, non-space text.
        %%
        %% 1.3. Or, unquoted, non-quote, non-space text.
        %%      "" and '' must match.
        "(?:"
            %% 1.3.1 Double quotes and any number of non-quotes in-between.
            "\"(?:[^\"\\\\]|\\\\.)*\""
            %% 1.3.2. Any number of non-delimiters (see #1.1)
            "[^\\s\"]*"
        ")+"
    %% 2. Or any number of non-space characters.
    ")|\\S+)").

%% @doc Escape an input string for use within a regular expression (so
%%      that all characters are interpreted literally). Every character
%%      will be escaped except for alphanumeric characters and '_'.
-spec escape_re(list()) -> list().
escape_re(Patt) ->
    lists:flatten(escape_re(Patt, [])).
escape_re([Ch|Patt], Escaped) ->
    escape_re(Patt, [escape_re_char(Ch)|Escaped]);
escape_re([], Escaped) -> lists:reverse(Escaped).
escape_re_char(C) when C < $0;
                       C > $9, C < $A;
                       C > $Z, C < $a, C /= $_;
                       C > $z  -> [$\\, C];
escape_re_char(C) -> C.

%% @doc Splits on spaces, except within quoted characters "" and ''.
-spec smart_split(binary()) -> [binary()].
smart_split(Subj) ->
    {ok, Re} = re:compile(?TOKEN_PART_MATCHER),
    {match, Parts} = re:run(Subj, Re, [{capture, first, binary}, global]),
    [Part || [Part] <- Parts].

-spec safe_list_to_atom(list()) -> atom() | error.
safe_list_to_atom(L) ->
    try list_to_existing_atom(L) of
        A -> A
    catch
        _:_ -> error
    end.
