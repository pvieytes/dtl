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

%% @doc Functions for dealing with files and filesystems.
-module(dtl_file).

-export([abspath/1,
         safe_path/2]).

%% @doc Returns the absolute path of the provided relative path,
%%      prepending the current working directory if the path is not
%%      already absolute, resolving "." and ".." path components in the
%%      process.
-spec abspath(list()) -> list().
abspath([$/|_] = Path) ->
    Parts = filename:split(Path),
    abspath_resolve(Parts, "");
abspath(Path) ->
    {ok, Dir} = file:get_cwd(),
    abspath(filename:join(Dir, Path)).

abspath_resolve([".."|Parts], [_Last|Acc]) ->
    abspath_resolve(Parts, Acc);
abspath_resolve(["."|Parts], Acc) ->
    abspath_resolve(Parts, Acc);
abspath_resolve([Part|Parts], Acc) ->
    abspath_resolve(Parts, [Part|Acc]);
abspath_resolve([], Acc) ->
    filename:join(lists:reverse(Acc)).

%% @doc Returns the first path if the second (root) path is its ancestor.
%%      Returns `undefined' if the second path is not an ancestor of the
%%      first.
-spec safe_path(list(), list()) -> list() | undefined.
safe_path(Path, Root) ->
    case string:str(abspath(Path), abspath(Root)) of
        0 -> undefined;
        _ -> Path
    end.
