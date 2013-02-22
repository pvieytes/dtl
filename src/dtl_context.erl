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

%% @doc Functions for dealing with template contexts. Template contexts
%%      are the primary way of transmitting data from application code
%%      to templates.
%%
%%      TODO: Support operations via dicts? This would allow for direct
%%            updates rather than repeated set() calls when a user is
%%            setting several values at once.
-module(dtl_context).

-export([fetch/2,
         fetch/3,
         new/0,
         new/1,
         pop/1,
         push/1,
         render_fetch/2,
         render_fetch/3,
         set/3]).

-include("dtl.hrl").

%% @doc Creates a new template context with no data.
-spec new_base() -> dtl_context().
new() -> new([]).

%% @doc Creates a new template context with the provided data.
-spec new(list()) -> dtl_context().
new(PList) ->
    Ctx = new_base(),
    Ctx2 = Ctx#dtl_ctx{render_context = new_base()},
    update(process_all(Ctx2), PList).

new_base() -> #dtl_ctx{}.

%% TODO: Support some initial state (Django provides an HTTP request
%%       object to each processor).
process_all(BaseCtx) ->
    lists:foldl(fun ({M, F}, Ctx) ->
        update(Ctx, M:F())
    end, BaseCtx, dtl_settings:context_processors()).

%% @doc Pushes a new dict on the stack.
-spec push(dtl_context()) -> dtl_context().
push(Ctx = #dtl_ctx{stack = Stack}) ->
    Ctx#dtl_ctx{stack = [dict:new()|Stack]}.

%% @doc Pops a dict off of the stack.
-spec pop(dtl_context()) -> dtl_context().
pop(Ctx = #dtl_ctx{stack = [_|Stack]}) ->
    Ctx#dtl_ctx{stack = Stack};
%% Django raises an exception when popping an empty stack. Not sure if
%% it matters.
pop(Ctx = #dtl_ctx{stack = []}) -> Ctx.

%% @doc Sets a value on the stack.
-spec set(dtl_context(), term(), term()) -> dtl_context().
set(Ctx = #dtl_ctx{stack = [Head|Stack]}, K, V) ->
    Ctx#dtl_ctx{stack = [dict:store(K, V, Head)|Stack]};
set(Ctx = #dtl_ctx{stack = []}, K, V) ->
    set(push(Ctx), K, V).

%% @doc Set many values on a context at once.
-spec update(dtl_context(), [{term(), term()}]) -> dtl_context().
update(Ctx, PList) ->
    Ctx2 = push(Ctx),
    lists:foldl(fun ({K, V}, Ctx3) ->
        set(Ctx3, K, V)
    end, Ctx2, PList).

%% @doc Looks up a value on all stacks, in top-to-bottom order.
-spec fetch(dtl_context(), term()) -> {ok, term()} | undefined.
fetch(#dtl_ctx{stack = []}, _K) ->
    undefined;
fetch(#dtl_ctx{stack = Stack}, K) ->
    fetch(Stack, K);
fetch([Head|Stack], K) ->
    case dict:find(K, Head) of
        error -> fetch(Stack, K);
        {ok, V} -> V
    end;
fetch([], _K) -> undefined.

%% @doc Looks up a value on all stacks, in top-to-bottom order,
%%      returning the default value if none is found.
-spec fetch(dtl_context(), term(), term()) -> term().
fetch(Ctx, K, Def) ->
    case fetch(Ctx, K) of
        undefined -> Def;
        V -> V
    end.

%% @doc Render contexts have different scope rules: Only look at the
%%      head of the stack. End users shouldn't have much reason to use
%%      this function.
-spec render_fetch(dtl_context(), term()) -> term() | undefined.
render_fetch(#dtl_ctx{stack = [Head|_Stack]}, K) ->
    fetch([Head], K).

-spec render_fetch(dtl_context(), term(), term()) -> term().
render_fetch(Ctx, K, Def) ->
    case render_fetch(Ctx, K) of
        undefined -> Def;
        {ok, V} -> V
    end.
