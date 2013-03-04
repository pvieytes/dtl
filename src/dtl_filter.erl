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

-include("compiler.hrl").

-record(expr, {
    var :: term(),
    token :: binary(),
    filters = [] :: [dtl_filter:filter()]
}).

-opaque expr() :: #expr{}.
-type filter() :: {filter_fun(), [filter_arg()]}.
-type filter_fun() ::{module(), atom()}.
-type filter_arg() :: {boolean(), term()}.

-export([parse/2,
         resolve_expr/2]).
-export_type([expr/0,
              filter/0,
              filter_fun/0]).

%% Double-quoted string.
-define(CONSTANT_RE, "(?:\"[^\"\\\\]*(?:\\.[^\"\\\\]*)*\")").
%% Numbers.
-define(NUM_RE, "[-+]?\\d[\\d\\.e]*").
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
    #expr{token = Token,
          var = Var,
          filters = Filters}.

-spec process_matches([list(binary())], term(), [filter()],
        dtl_parser:parser()) ->
    {term(), [filter()]} | {error, {unknown_filter, atom()}}.
%% First match must contain a variable or constant.
process_matches([[<<>>, <<>>, _, _, _]|_], undefined, _, _) ->
    {error, no_variable};
%% Use the first variable.
process_matches([[<<>>, Var, _, _, _]|Matches], undefined, Filters,
        Parser) ->
    process_matches(Matches, process_var(Var), Filters, Parser);
%% Use the first constant.
process_matches([[Const, <<>>, _, _, _]|Matches], undefined, Filters,
        Parser) ->
    process_matches(Matches, process_string(Const), Filters, Parser);
%% Save the filter name and an argument, if present.
process_matches([[_, _, RawName, ConstArg, VarArg]|Matches], Var,
        Filters, Parser) ->
    case dtl_string:safe_list_to_atom(binary_to_list(RawName)) of
        error -> {error, {unknown_filter, RawName}};
        Name ->
            case dtl_parser:find_filter(Parser, Name) of
                nomatch -> {error, {unknown_filter, RawName}};
                {ok, FilterFun} ->
                    Args = case {ConstArg, VarArg} of
                        {<<>>, <<>>} -> [];
                        {Const, <<>>} -> [{false, process_string(Const)}];
                        {<<>>, Var2} ->
                            Var3 = process_var(Var2),
                            Lookup = is_list(Var3),
                            {Lookup, Var3}
                    end,
                    Filter = {FilterFun, Args},
                    process_matches(Matches, Var, [Filter|Filters], Parser)
            end
    end;
%% Finished.
process_matches([], Var, Filters, _Parser) ->
    {Var, lists:reverse(Filters)}.

%% Process a double-quoted string.
-spec process_string(binary()) -> binary().
process_string(Bin) ->
    case process_term(Bin) of
        {error, Reason} -> {error, Reason};
        Term -> list_to_binary(Term)
    end.

%% Process a raw Erlang term.
-spec process_term(binary()) -> term() | {error, invalid_constant}.
process_term(Const) ->
    case erl_scan:string(binary_to_list(Const)) of
        {ok, [{_Type, _Pos, Term}], _Loc} -> Term;
        %% For '-' and '+' operators:
        {ok, [{Op, _Loc}, {_Type, _Pos, Term}], _Loc2} ->
            erlang:Op(0, Term);
        {error, _, _Loc} -> {error, invalid_term}
    end.

%% Parse a variable specification. This function is designed to handle
%% numbers as well. Django does it this way, but this approach probably
%% isn't ideal.
-spec process_var(binary()) -> [list()].
process_var(Num = <<C, _/binary>>) when C >= $0, C =< $9;
                                        C =:= $+;
                                        C =:= $- ->
    process_term(Num);
process_var(Var) ->
    string:tokens(binary_to_list(Var), ?VARIABLE_SEP).

-spec resolve_expr(expr(), dtl_context:context()) -> term().
resolve_expr(#expr{var = Var, filters = Filters}, Ctx) ->
    resolve_expr_filters(Filters, resolve_var(Var, Ctx), Ctx).

resolve_expr_filters([{{Mod, Fun}, Args}|Filters], Var, Ctx) ->
    RealArgs = case Args of
        [] -> [Var];
        [{true, Arg}] -> [Var, resolve_lookup(Arg, Ctx)];
        [{false, Arg}] -> [Var, Arg]
    end,
    case apply(Mod, Fun, RealArgs) of
        {error, Reason} -> {error, Reason};
        Var2 -> resolve_expr_filters(Filters, Var2, Ctx)
    end;
resolve_expr_filters([], Var, _Ctx) -> Var.

-spec resolve_var(term(), dtl_context:context()) -> binary().
resolve_var(Lookup = [[_|_]|_], Ctx) -> resolve_lookup(Lookup, Ctx);
resolve_var(T, _Ctx) -> T.

-spec resolve_lookup([list()], term()) -> binary().
%% This comes later but the function clause must be listed first.
resolve_lookup([Head|Lookups], PList) when is_list(PList) ->
    case dtl_string:safe_list_to_atom(Head) of
        error -> undefined;
        A -> resolve_lookup(Lookups, proplists:get_value(A, PList))
    end;
%% Start with the context ...
resolve_lookup([Head|Lookups], Ctx) ->
    case dtl_string:safe_list_to_atom(Head) of
        error -> undefined;
        A -> resolve_lookup(Lookups, dtl_context:fetch(Ctx, A))
    end;
%% Empty if undefined.
resolve_lookup(_, undefined) -> undefined;
resolve_lookup([], Val) -> Val.
