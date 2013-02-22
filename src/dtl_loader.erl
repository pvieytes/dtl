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
%%
%%      Template loaders should implement the `dtl_loader' behaviour'
%%      and its interface:
%%
%%      is_usable() -> boolean().
%%          
%%          Return `true' if the loader can or should be used in the
%%          current environment, and `false' otherwise. You may want to
%%          cache this value if it is expensive to compute. DTL does
%%          not cache this result, under the assumption that a usable
%%          loader may become unusable if runtime conditions change.
%%
%%      load_template_source(list()) ->
%%          {ok, binary()} | {error,atom()}.
%%      load_template_source(list(), [list()]) ->
%%          {ok, binary()} | {error,atom()}.
%%
%%          The `load_template_source' callbacks are used to locate
%%          templates. The first argument to this function is the name
%%          of the requested template. The second argument can in most
%%          cases be ignored, it is the list of configured template
%%          directories, used by `dtl_fs_loader'.
%%
%%      This module's functions all operate, in order, on the list of
%%      template loaders configured in this application's environment
%%      variable, `template_loaders'. Functions will try each loader
%%      until one returns a template successfully.
-module(dtl_loader).

-callback is_usable() -> boolean().
-callback load_template_source(list()) ->
    {ok, binary()} | {error, atom()}.
-callback load_template_source(list(), [list()]) ->
    {ok, binary()} | {error, atom()}.

-export([get_template/1,
         find_template/1,
         find_template/2,
         select_template/1]).

%% @doc Finds a template and compiles it, returning the compiled
%%      representation.
-spec get_template(list()) ->
    dtl_template:template() | {error, not_found | atom()}.
get_template(Name) ->
    case find_template(Name) of
        {ok, Source} -> {ok, dtl_template:new(Source)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Returns the first of several requested templates that a loader
%%      module finds, in its compiled representation.
-spec select_template([list()]) ->
    dtl_template:template() | {error, not_found | atom()}.
select_template([Name|Names]) ->
    case get_template(Name) of
        {ok, Tpl} -> {ok, Tpl};
        {error, _Reason} -> select_template(Names)
    end;
select_template([]) -> {error, not_found}.

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
