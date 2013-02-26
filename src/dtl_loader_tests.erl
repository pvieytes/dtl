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

%% @doc Tests of basic `dtl_loader' mechanics by creating a dummy
%%      template loader.
-module(dtl_loader_tests).
-behaviour(dtl_loader).

-export([is_usable/0,
         load_template_source/1,
         load_template_source/2]).

-include_lib("eunit/include/eunit.hrl").

%% Loader behaviour
is_usable() -> true.

load_template_source(_Name) ->
    {ok, <<"Test">>, "<virtual>"}.

load_template_source(Name, _Dirs) ->
    load_template_source(Name).

%% Tests
setup() ->
    dtl_ets_settings:set(template_loaders, [?MODULE]).

behaviour_test_() ->
    {setup, fun setup/0,
     [?_assertEqual({ok, <<"Test">>}, dtl_loader:find_template(""))]}.
