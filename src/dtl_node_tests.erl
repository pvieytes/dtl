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

setup() ->
    dtl_ets_settings:set(template_loaders, [dtl_fs_loader]).

teardown(_) ->
    dtl_ets_settings:clear().

text_node_test_() ->
    Ctx = dtl_context:new(),
    {setup, fun setup/0, fun teardown/1,
     [?_assertEqual(<<"A">>, dtl_node:render(<<"A">>, Ctx)),
      ?_assertEqual(<<"B">>, dtl_node:render("B", Ctx))]}.

list_render_test_() ->
    Ctx = dtl_context:new(),
    {setup, fun setup/0, fun teardown/1,
     [?_assertEqual({ok, [<<"a">>, <<"b">>, <<"c">>]},
                          dtl_node:render_list(["a", "b", "c"], Ctx)),
      ?_assertEqual({ok, []}, dtl_node:render_list([], Ctx))]}.

variable_node_test_() ->
    Ctx = dtl_context:new([{color, <<"Orange">>},
                           {piglets, 4},
                           {nested, [{ a, 1 }]},
                           {an_atom, {1, 2, 3}},
                           {a_list, [1, 2, 3]}]),
    Tpl = dtl_template:new(<<"{{ color }}">>),
    Tpl2 = dtl_template:new(<<"Piglets: {{ piglets }}">>),
    Tpl3 = dtl_template:new(<<"{{ an_atom }}">>),
    Tpl4 = dtl_template:new(<<"{{ a_list }} = L">>),
    Tpl5 = dtl_template:new(<<"{{ nested.a }}">>),
    {setup, fun setup/0, fun teardown/1,
     [?_assertEqual({ok, <<"Orange">>, Ctx}, dtl_template:render(Tpl, Ctx)),
      ?_assertEqual({ok, <<"Piglets: 4">>, Ctx}, dtl_template:render(Tpl2, Ctx)),
      ?_assertEqual({ok, <<"{1,2,3}">>, Ctx}, dtl_template:render(Tpl3, Ctx)),
      ?_assertEqual({ok, <<"[1,2,3] = L">>, Ctx}, dtl_template:render(Tpl4, Ctx)),
      ?_assertEqual({ok, <<"1">>, Ctx}, dtl_template:render(Tpl5, Ctx))]}.
