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

%% @doc Tests for the filesystem loader.
-module(dtl_settings_tests).

-include_lib("eunit/include/eunit.hrl").

settings_test_() ->
    Apps = [dtl],
    Processors = [{foo, bar}],
    Debug = false,
    TemplateDirs = [file:get_cwd()],
    Loaders = [dtl_fs_loader],
    application:set_env(dtl, apps, Apps),
    application:set_env(dtl, context_processors, Processors),
    application:set_env(dtl, debug, Debug),
    application:set_env(dtl, template_dirs, TemplateDirs),
    application:set_env(dtl, template_loaders, Loaders),
    [?_assertEqual(dtl_settings:apps(), Apps),
     ?_assertEqual(dtl_settings:context_processors(), Processors),
     ?_assertEqual(dtl_settings:debug(), Debug),
     ?_assertEqual(dtl_settings:template_dirs(), TemplateDirs),
     ?_assertEqual(dtl_settings:template_loaders(), Loaders)].
