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

%% @doc Template loader interface.
-module(dtl_loader).

-callback is_usable() -> boolean().
-callback load_template_source(list()) ->
    {ok, binary()} | {error, atom()}.
-callback load_template_source(list(), [list()]) ->
    {ok, binary()} | {error, atom()}.

-export([find_template/1,
         find_template/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Try to load the named template with each configured loader
%%      module.
-spec find_template(list()) ->
    {ok, binary()} | {error, not_found | no_loaders | atom()}.
find_template(Name) -> find_template(Name, []).
-spec find_template(list(), [list()]) ->
    {ok, binary()} | {error, not_found | no_loaders | atom()}.
find_template(Name, Dirs) ->
    case dtl_settings:template_loaders() of
        [] -> {error, no_loaders};
        Loaders -> try_loaders(Name, Dirs, Loaders)
    end.

try_loaders(Name, Dirs, [Loader|Loaders]) ->
    case Loader:is_usable() of
        true ->
            case Loader:load_template_source(Name, Dirs) of
                {ok, Source, _DisplayName} -> 
                    {ok, Source};
                {error, not_found} ->
                    try_loaders(Name, Dirs, Loaders);
                {error, Reason} ->
                    {error, Reason}
            end;
        false -> try_loaders(Name, Dirs, Loaders)
    end;
try_loaders(_Name, _Dirs, []) -> {error, not_found}.

%%
%% Tests,
%%
-ifdef(TEST).
-endif.
