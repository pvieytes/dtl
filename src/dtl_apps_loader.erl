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

%% @doc App directories loader. Searches the "priv" directory in each
%%      configured application.
-module(dtl_apps_loader).
-behaviour(dtl_loader).

-export([is_usable/0,
         load_template_source/1,
         load_template_source/2]).

%% @doc Callback to test if this module is usable.
-spec is_usable() -> true.
is_usable() -> true.

%% @doc Searches configured applications' "priv" directories for the
%%      named template. Returns the files contents if found, or a
%%      not_found error otherwise.
-spec load_template_source(list()) -> 
    {ok, binary(), list()} | {error, not_found}.
load_template_source(Name) -> load_template_source(Name, []).

%% @doc Searches configured applications' "priv" directories for the
%%      named template. Returns the files contents if found, or a
%%      not_found error otherwise.
-spec load_template_source(list(), [list()]) ->
    {ok, binary(), list()} | {error, not_found}.
load_template_source(Name, _Dirs) ->
    load_template_source(Name, dtl_settings:apps(), []).
load_template_source(Name, [App|Apps], Tried) ->
    case code:priv_dir(App) of
        %% Django reports this error, not sure if we should too ...
        {error, bad_name} ->
            load_template_source(Name, Apps, [App|Tried]);
        Priv ->
            Dir = filename:join(Priv, "templates"),
            case dtl_fs_loader:load_from_directory(Dir, Name) of
                {ok, Contents, Path} -> {ok, Contents, Path};
                {error, not_found} ->
                    load_template_source(Name, Apps, [App|Tried])
            end
    end;
load_template_source(_Name, [], _Tried) ->
    {error, not_found}.
