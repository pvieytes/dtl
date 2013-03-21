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

%% @doc Tests of basic node/nodelist mechanics and basic node types.
-module(dtl_node_tests).

-include_lib("eunit/include/eunit.hrl").


%% Test that basic text nodes are output correctly.
text_node_test_() ->
    Ctx = dtl_context:new(),
    [?_assertEqual(<<"A">>, dtl_node:render(<<"A">>, Ctx)),
     ?_assertEqual(<<"B">>, dtl_node:render("B", Ctx))].

%% Test that a nodelist renders correctly.
list_render_test_() ->
    Ctx = dtl_context:new(),
    [?_assertEqual({ok, [<<"a">>, <<"b">>, <<"c">>], Ctx},
                         dtl_node:render_list(["a", "b", "c"], Ctx)),
     ?_assertEqual({ok, [], Ctx}, dtl_node:render_list([], Ctx))].

%% Test that all sorts of variables and constants are read and
%% substituted properly in renderered templates.
variable_node_test_() ->
    Tests = [{<<"Orange">>, <<"{{ color }}">>},
             {<<"Piglets: 4">>, <<"Piglets: {{ piglets }}">>},
             {<<"{1,2,3}">>, <<"{{ an_atom }}">>},
             {<<"[1,2,3] = L">>, <<"{{ a_list }} = L">>},
             {<<"1">>, <<"{{ nested.a }}">>},
             {<<"Oink">>, <<"{{ \"Oink\" }}">>},
             {<<"2">>, <<"{{ 2 }}">>},
             {<<"8.8">>, <<"{{ 8.8 }}">>},
             {<<"-1.9">>, <<"{{ -1.9 }}">>},
             {<<"-4">>, <<"{{ -4 }}">>},
             {<<"1.89e5">>, <<"{{ 1.89e5 }}">>},
             {<<"2">>, <<"{{ 2 }}">>}],
    Ctx = dtl_context:new([{color, <<"Orange">>},
                           {piglets, 4},
                           {nested, [{ a, 1 }]},
                           {an_atom, {1, 2, 3}},
                           {a_list, [1, 2, 3]}]),
    dtl_tests:compare_templates(Tests, Ctx).

empty_render_test_() ->
    Tests = [{<<"  ">>, <<" {{ MISSING }} ">>}],
    Ctx = dtl_context:new(),
    dtl_tests:compare_templates(Tests, Ctx).

comment_render_test_() ->
    Tests = [{<<>>, <<"{#\nComments\n#}">>}],
    Ctx = dtl_context:new(),
    dtl_tests:compare_templates(Tests, Ctx).
