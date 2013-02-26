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

%% @doc File system template loader. Searches directories for templates.
-module(dtl_fs_loader).
-behaviour(dtl_loader).

-export([is_usable/0,
         load_from_directory/2,
         load_template_source/1,
         load_template_source/2]).

%% @doc Callback to test if this module is usable.
-spec is_usable() -> true.
is_usable() -> true.

%% @doc Searches configured template directories for the named template.
%%      Returns the file's contents if found, and its path, or an error
%%      if there was one.
-spec load_template_source(list()) -> 
    {ok, binary(), list()}
      | {error, no_template_dirs | not_found}.
load_template_source(Name) ->
    load_template_source(Name, dtl:setting(template_dirs)).

%% @doc Searches the specified directories for the named template
%%      (rather than the application's configured directory list). If
%%      the provided list is empty, we use the configured list of these.
%%      
%%      If the template is not found, this returns a `not_found' error.
-spec load_template_source(list(), [list()]) ->
    {ok, binary(), list()} 
      | {error, no_template_dirs | not_found}.
load_template_source(Name, []) ->
    load_template_source(Name, dtl:setting(template_dirs));
load_template_source(Name, Dirs) ->
    load_template_source(Name, Dirs, []).
load_template_source(Name, [Dir|Dirs], Tried) ->
    case load_from_directory(Dir, Name) of
        {ok, Contents, Path} -> {ok, Contents, Path};
        {error, not_found} ->
            load_template_source(Name, Dirs, [Dir|Tried])
    end;
load_template_source(_Name, [], []) ->
    {error, no_template_dirs};
load_template_source(_Name, [], _Tried) ->
    {error, not_found}.

%% @doc Searches the provided directory for the named file, reading its
%%      contents if found. An error of `not_found' will be returned in
%%      all failure cases.
-spec load_from_directory(list(), list()) ->
    {ok, binary(), list()} | {error, not_found}.
load_from_directory(Dir, Name) ->
    RelativePath = filename:join(Dir, Name),
    case dtl_file:safe_path(RelativePath, Dir) of
        undefined -> {error, not_found};
        Path -> case file:read_file(Path) of
            {ok, Contents} ->
                {ok, Contents, Path};
            {error, _Reason} ->
                {error, not_found}
        end
    end.
