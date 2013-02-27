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

%% @doc Functions for running and creating template filters. See Django
%%      docs for what a filter is ...
-module(dtl_filter).

-include("dtl_compiler.hrl").

-type filter_fun() ::{atom(), atom()}.
-type filter_arg() :: {boolean(), term()}.
-type filter() :: {filter_fun(), [filter_arg()]}.
-type expr() :: {}.

-export([parse/2,
         resolve_expr/2]).
-export_type([expr/0,
              filter/0]).

%% Double-quoted string.
-define(STR_DQUOTE_RE, "'[^'\\\\]*(?:\\.[^'\\\\]*)*'").
%% Constant string.
%%
%% This is the part of the regex to change if you want to capture strings
%% marked for translation.
-define(CONSTANT_RE, "(?:" ?STR_DQUOTE_RE ")").
%% Numbers.
-define(NUM_RE, "[-+\.]?\\d[\\d\\.e]*").
%% Variable names.
-define(VAR_CHARS_RE, "\\w\\.").
%% Filter separator.
-define(SEP_RE, ++ dtl_string:escape_re(?FILTER_SEP) ++).
%% Filter argument separator.
-define(ARG_SEP_RE, ++ dtl_string:escape_re(?FILTER_ARG_SEP) ++).
%% Parses a filter expression: [str|var]\|filter_name[:(str|var)...].
-define(FILTER_RE, "(?P<constant>" ?CONSTANT_RE ")|"
                   "(?P<var>[" ?VAR_CHARS_RE "]+|" ?NUM_RE ")|"
                   "(?:\\s*" ?SEP_RE "\\s*"
                   "(?P<filter_name>\\w+)"
                       "(?:" ?ARG_SEP_RE
                           "(?:"
                               "(?P<constant_arg>" ?CONSTANT_RE ")|"
                               "(?P<var_arg>[" ?VAR_CHARS_RE "]+|" ?NUM_RE ")"
                           ")"
                       ")?"
                   ")").
%% Groups we want to capture from the previous regex:
-define(CAPTURE_GROUPS, [constant, var, filter_name, constant_arg, var_arg]).
%% Variable part separator.
-define(VARIABLE_SEP, ".").


%% @doc Parses the contents of a variable node into a filter expression
%%      record: [str|var]\|filter_name[:(str|var)...]
-spec parse(binary(), dtl_parser:parser()) -> expr().
parse(Token, Parser) ->
    {ok, Re} = re:compile(?FILTER_RE),
    Match = re:run(Token, Re, [global, {capture, ?CAPTURE_GROUPS, binary}]),
    {Var, Filters} = case Match of
        nomatch -> {undefined, []};
        {match, Matches} -> process_matches(Matches, undefined, [], Parser)
    end,
    #dtl_filter_expr{token = Token,
                     var = Var,
                     filters = Filters}.

%% First match must contain a variable or constant.
process_matches([[<<>>, <<>>, _, _, _]|_], undefined, _, _) ->
    {error, no_variable};
%% Use the first variable.
process_matches([[<<>>, Var, _, _, _]|Matches], undefined, Filters, Parser) ->
    process_matches(Matches, process_var(Var), Filters, Parser);
%% Use the first constant.
process_matches([[Const, <<>>, _, _, _]|Matches], undefined, Filters, Parser) ->
    process_matches(Matches, process_const(Const), Filters, Parser);
%% Save the filter name and an argument, if present.
process_matches([[_, _, Name, ConstArg, VarArg]|Matches], Var, Filters, Parser) ->
    case dtl_parser:find_filter(Parser, Name) of
        error -> process_matches(Matches, Var, Filters, Parser);
        FilterFun ->
            Args = case {ConstArg, VarArg} of
                {<<>>, <<>>} -> [];
                {Const, <<>>} -> [{false, process_const(Const)}];
                {<<>>, Var} -> [{true, process_var(Var)}]
            end,
            Filter = {FilterFun, Args},
            process_matches(Matches, Var, Filters, Parser)
    end;
%% Finished.
process_matches([], Var, Filters, _Parser) ->
    {Var, Filters}.

%% Process a raw constant value.
-spec process_const(binary()) -> term().
process_const(Const) ->
    case erl_scan:string(binary_to_list(Const)) of
        %% There should only be one term.
        {ok, [{_Type, _Pos, Term}]} -> Term;
        {error, _} -> {error, invalid_constant}
    end.

%% Parse a variable specification.
-spec process_var(binary()) -> [list()].
process_var(Var) ->
    string:tokens(binary_to_list(Var), ?VARIABLE_SEP).

-spec resolve_expr(expr(), dtl_context:context()) -> binary().
resolve_expr(#dtl_filter_expr{var = Var, filters = Filters}, Ctx) ->
    filter_var(resolve_var(Var, Ctx), Filters, Ctx).

-spec resolve_var([list()], dtl_context:context()) -> binary();
                 (term(), dtl_context:context()) -> binary().
%% Ensure Lookup is a list of lists.
resolve_var(Lookup = [[_|_]|_], Ctx) -> resolve_lookup(Lookup, Ctx);
resolve_var(T, _Ctx) -> T.

-spec resolve_lookup([list()], term()) -> binary().
%% Start with the context ...
resolve_lookup([Head|Lookups], Ctx) ->
    case dtl_string:safe_list_to_atom(Head) of
        error -> undefined;
        A -> resolve_lookup(Lookups, dtl_context:fetch(Ctx, A))
    end;
%% Empty if undefined.
resolve_lookup(_, undefined) -> undefined;
resolve_lookup([Head|Lookups], PList) when is_list(PList) ->
    case dtl_string:safe_list_to_atom(Head) of
        error -> undefined;
        A -> resolve_lookup(Lookups, proplists:get_value(A, PList))
    end;
resolve_lookup([], Val) -> Val.

-spec filter_var(term(), [filter()], dtl_context:context()) -> binary().
filter_var(Var, [{{Mod, Fun}, Args}|Filters], Ctx) ->
    RealArgs = case Args of
        [] -> [Var];
        [{true, Arg}] -> [Var, resolve_lookup(Arg, Ctx)];
        [{false, Arg}] -> [Var, Arg]
    end,
    filter_var(apply(Mod, Fun, RealArgs), Filters, Ctx);
filter_var(Var, [], _Ctx) -> Var.
