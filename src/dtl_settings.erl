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
-module(dtl_settings).

-export([apps/0,
         context_processors/0,
         debug/0,
         template_dirs/0,
         template_loaders/0]).

%% @doc Returns a list of apps that should be searched for templates at
%%      runtime.
-spec apps() -> [atom()].
apps() -> env(apps).

%% @doc A list of {Mod, Fun} tuples, representing functions that receive
%%      a single parameter, that being the existing context. In Django,
%%      parameter is the request object, but there is no analog to this
%%      a plain template engine.
-spec context_processors() -> [{atom(), atom()}].
context_processors() -> env(context_processors).

%% @doc Returns `true' if the application is configured in "debug" mode,
%%      `false' otherwise.
-spec debug() -> boolean().
debug() -> env(debug).

%% @doc Returns the application's configured list of template
%%      directories.
-spec template_dirs() -> [list()].
template_dirs() -> env(template_dirs).

%% @doc Returns the configured list of template loader modules. All
%%      should implement the `dtl_loader' behaviour.
-spec template_loaders() -> [atom()].
template_loaders() -> env(template_loaders).

env(K) ->
    %% Defaults should be defined for every env var.
    {ok, V} = application:get_env(dtl, K),
    V.
