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

%% @doc Tests for contexts.
-module(dtl_context_tests).

-export([example_processor/0,
         example_processor2/0]).

-include_lib("eunit/include/eunit.hrl").

example_processor() -> [{b, 7}, {z, 8}, {x, 20}].
example_processor2() -> [{b, 8}].

base_context_test_() ->
    Ctx = dtl_context:new([{a, 1}, {b, 2}]),
    Ctx2 = dtl_context:pop(Ctx),
    Ctx3 = dtl_context:set(dtl_context:push(Ctx), a, 4),
    Ctx4 = dtl_context:update(Ctx, [{a, 5}]),
    [?_assertEqual(1, dtl_context:fetch(Ctx, a)),
     ?_assertEqual(2, dtl_context:fetch(Ctx, b)),
     ?_assertEqual(undefined, dtl_context:fetch(Ctx, c)),
     ?_assertEqual(3, dtl_context:fetch(Ctx, c, 3)),
     ?_assertEqual(undefined, dtl_context:fetch(Ctx2, a)),
     ?_assertEqual(4, dtl_context:fetch(Ctx3, a, 3)),
     ?_assertEqual(5, dtl_context:fetch(Ctx4, a)),
     ?_assertEqual(2, dtl_context:fetch(Ctx4, b))].

context_processor_test() ->
    dtl_ets_settings:set(context_processors, [
        {dtl_context_tests, example_processor},
        {dtl_context_tests, example_processor2}
    ]),
    Ctx = dtl_context:new([{z, 12}]),
    ?assertEqual(20, dtl_context:fetch(Ctx, x)),
    ?assertEqual(8, dtl_context:fetch(Ctx, b)),
    ?assertEqual(20, dtl_context:fetch(Ctx, x)),
    ?assertEqual(12, dtl_context:fetch(Ctx, z)),
    ?assertEqual(undefined, dtl_context:fetch(Ctx, y)),
    dtl_ets_settings:clear().
