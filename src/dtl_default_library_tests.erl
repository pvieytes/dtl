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

%% @doc Default template tag and filter tests.
-module(dtl_default_library_tests).

-behaviour(dtl_library).

-export([registered_filters/0,
         registered_tags/0]).

-export([make_cat/1,
         wc/2,
         wc_render/2]).

-include("dtl.hrl").
-include_lib("eunit/include/eunit.hrl").

load_tag_test_() ->
    dtl_tests:compare_templates([
        {Out, <<"{% load dtl_default_library_tests %}", In/binary>>}
            || {Out, In} <- [{<<"Cat">>, <<"{{ dog|make_cat }}">>},
                             {<<"2">>, <<"{% wc %} Two words {% endwc %}">>}]
        ], dtl_context:new([{dog, <<"Dog">>}])).

%% dtl_library.
registered_filters() -> [make_cat].
registered_tags() -> [wc].

%%
%% Filters
%%

make_cat(_) -> <<"Cat">>.

%%
%% Tags
%%
wc(Parser, _Token) ->
    {ok, Nodes, Parser2} = dtl_parser:parse(Parser, [endwc]),
    Node = #dtl_node{name = "wc",
                     nodelists = [Nodes],
                     renderer = {?MODULE, wc_render}},
    {ok, Node, dtl_parser:delete_first_token(Parser2)}.

wc_render(Node, Ctx) ->
    [Nodes] = Node#dtl_node.nodelists,
    Out = dtl_node:render(Nodes, Ctx),
    In = binary_to_list(iolist_to_binary(Out)),
    Wc = case re:run(In, "(?:^|\\b)\\w+(?:\\b|$)", [global]) of
        nomatch -> 0;
        {match, Ms} -> length(Ms)
    end,
    list_to_binary(integer_to_list(Wc)).
