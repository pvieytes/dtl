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

-include_lib("eunit/include/eunit.hrl").

load_tag_test_() ->
    dtl_tests:compare_templates([
        {Out, <<"{% load dtl_default_library_tests %}", In/binary>>} ||
            {Out, In} <- [{<<"Cat">>, <<"{{ dog|make_cat }}">>},
                             {<<"2">>,
                              <<"{% wc %} Two words {% endwc %}">>}]
        ], dtl_context:new([{dog, <<"Dog">>}])).

block_tag_test() ->
    {ok, Expected} = dtl_loader:find_template("block-out.html"),
    {ok, Out} = dtl:render("block.html"),
    {ok, Expected2} = dtl_loader:find_template("block-out2.html"),
    {ok, Out2} = dtl:render("block2.html"),
    {ok, Expected3} = dtl_loader:find_template("block-out3.html"),
    {ok, Out3} = dtl:render("block3.html"),
    ?assertEqual(Expected, Out),
    ?assertEqual(Expected2, Out2),
    ?assertEqual(Expected3, Out3).

filter_test_() ->
    Ctx = dtl_context:new([{ quotes, <<"'\"\\">>}]),
    dtl_tests:compare_templates([
        {<<"abc">>, <<"{{ \"ABC\"|lower }}">>},
        {<<"ABC">>, <<"{{ \"abc\"|upper }}">>},
        {<<"\\'\\\"\\\\">>, <<"{{ quotes|addslashes }}">>},
        {<<"Trout">>, <<"{{ \"trout\"|capfirst }}">>}
    ], Ctx).

comment_tag_test_() ->
    Tests = [{<<>>, <<"{% comment %} Stuff {% endcomment %}">>}],
    Ctx = dtl_context:new(),
    dtl_tests:compare_templates(Tests, Ctx).

if_tag_test_() ->
    Tests = [{<<"true">>, <<"{% if 1 > 0 %}true{% endif %}">>},
             {<<"Else">>, <<"{% if 1 < 0 %}If{% else %}Else{% endif %}">>},
             {<<"eq">>, <<"{% if 2 =:= 2 %}eq{% endif %}">>},
             {<<"eq">>, <<"{% if a == b %}eq{% endif %}">>},
             {<<"1">>, <<"{% if 1 %}1{% endif %}">>},
             {<<>>, <<"{% if a == c %}eq{% endif %}">>},
             {<<"The weather is sunny.">>,
              <<"{% if weather %}The weather is {{ weather }}.{% endif %}">>},
             {<<"neq">>, <<"{% if 2 =/= 3 %}neq{% endif %}">>}],
    Ctx = dtl_context:new([{a, a},
                           {b, a},
                           {c, b},
                           {weather, <<"sunny">>}]),
    dtl_tests:compare_templates(Tests, Ctx).

ifequal_tag_test_() ->
    Tests = [{<<"true">>, <<"{% ifequal 1 1 %}true{% endifequal %}">>},
             {<<"false">>,
              <<"{% ifnotequal 1 1 %}true{% else %}false{% endifnotequal %}">>},
             {<<"false">>,
              <<"{% ifequal a c %}true{% else %}false{% endifequal %}">>},
             {<<"true">>, <<"{% ifnotequal a c %}true{% endifnotequal %}">>},
             {<<"true">>, <<"{% ifequal a a %}true{% endifequal %}">>}],
    Ctx = dtl_context:new([{a, a},
                           {b, a},
                           {c, b}]),
    dtl_tests:compare_templates(Tests, Ctx).

forloop_test_() ->
    Tests = [{<<"1234">>, <<"{% for n in l %}{{ n }}{% endfor %}">>},
             {<<"0123">>, <<"{% for n in l %}{{ forloop.counter0 }}{% endfor %}">>},
             {<<"Empty">>, <<"{% for n in l2 %}{{ n }}{% empty %}Empty{% endfor %}">>},
             {<<"1">>, <<"{% for n in l3 %}{{ n }}{% empty %}Empty{% endfor %}">>},
             {<<"Empty2">>, <<"{% for n in l99 %}{{ n }}{% empty %}Empty2{% endfor %}">>},
             {<<"1,2:3,4">>, <<"{% for x, y in l4 %}{{ x }},{{ y }}{% if not forloop.last %}:{% endif %}{% endfor %}">>}],
    Ctx = dtl_context:new([{l, [1, 2, 3, 4]},
                           {l2, []},
                           {l3, [1]},
                           {l4, [[1, 2],
                                 [3, 4]]}]),
    dtl_tests:compare_templates(Tests, Ctx).


%% dtl_library (for {% load %} tests).
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
    Node = dtl_node:new("wc", {?MODULE, wc_render}),
    Node2 = dtl_node:set_nodelist(Node, Nodes),
    {ok, Node2, dtl_parser:delete_first_token(Parser2)}.

wc_render(Node, Ctx) ->
    Out = dtl_node:render(dtl_node:nodelist(Node), Ctx),
    In = binary_to_list(iolist_to_binary(Out)),
    Wc = case re:run(In, "(?:^|\\b)\\w+(?:\\b|$)", [global]) of
        nomatch -> 0;
        {match, Ms} -> length(Ms)
    end,
    list_to_binary(integer_to_list(Wc)).
