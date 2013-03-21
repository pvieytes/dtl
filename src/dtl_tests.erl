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

%% @doc System tests of high-level DTL functions. Test utilities are all
%%      defined here as well.
-module(dtl_tests).

-export([compare_templates/2]).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    dtl_ets_settings:set(apps, [test_app]).

teardown(_) ->
    dtl_ets_settings:clear().

render_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [?_assertEqual({ok, <<"Test\n">>}, dtl:render("test.html"))]}.

%%
%% Test utilities.
%%

%% Convenience function to test template source compilation and
%% rendering. Provide expected template source, output, and context.
compare_templates(Tests, Ctx) ->
    lists:foldr(fun ({Expect, In}, Tests2) ->
        {ok, Out, _Ctx} = dtl_template:render(dtl_template:new(In), Ctx),
        [?_assertEqual(Expect, Out)|Tests2]
    end, [], Tests).
