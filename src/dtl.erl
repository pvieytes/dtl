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

%% @doc Core API functions, some convenient shortcuts.
-module(dtl).

-export([render/1,
         render/2,
         setting/1,
         setting/2]).

-include("dtl.hrl").

%% @doc Creates and renders the template with the provided name.
-spec render(list()) -> {ok, binary()} | {error, atom()}.
render(Name) -> render(Name, dtl_context:new()).

%% @doc Creates and renders the template with the provided name.
-spec render(list(), [{term(), term()}]) ->
                {ok, binary()} | {error, atom()};
            (list(), dtl_context()) ->
                {ok, binary()} | {error, atom()}.
render(Name, PList) when is_list(PList) ->
    render(Name, dtl_context:new(PList));
render(Name, Ctx) ->
    %% TODO: Switch on error condition.
    {ok, Tpl} = dtl_loader:get_template(Name),
    {ok, Out, _Ctx2} = dtl_template:render(Tpl, Ctx),
    {ok, Out}.

%% @doc Find some setting, defaults to `undefined'.
-spec setting(atom()) -> term().
setting(Name) ->
    setting(Name, undefined).

%% @doc Find some setting, or return the provided default.
-spec setting(atom(), term()) -> term().
setting(Name, Default) ->
    {ok, Mod} = application:get_env(dtl, settings_module),
    Mod:setting(Name, Default).
