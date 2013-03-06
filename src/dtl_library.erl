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

%% @doc Libraries are containers of template tags and filters. Custom
%%      DTL tags and filters must be implemented in a module that
%%      implements the `dtl_library' behaviour.
-module(dtl_library).

-type name() :: atom().
-type library() :: dict().
-type filter_spec() :: name().
-type tag_spec() :: name()
                  | {{module(), atom()}, name()}
                  | {{module(), atom(), term()}, name()}.

-callback registered_filters() -> [filter_spec()].
-callback registered_tags() -> [tag_spec()].

-export([add_filters/2,
         add_tags/2,
         search_collection/2]).
-export_type([filter_spec/0,
              library/0,
              name/0,
              tag_spec/0]).

%% @doc Update the filters dict with all filters registered to the
%%      specified `dtl_library' callback module.
-spec add_filters(atom(), dict()) -> dict().
add_filters(Mod, Filters) ->
    lists:foldl(fun (Filter, Dict) ->
        dict:store(Filter, {Mod, Filter}, Dict)
    end, Filters, Mod:registered_filters()).

%% @doc Update the tags dict with all tags registered to the specified
%%      `dtl_library' callback module.
-spec add_tags(atom(), dict()) -> dict().
add_tags(Mod, Tags) ->
    lists:foldl(fun (UserTagSpec, Dict) ->
        {Name, Tag} = convert_user_tag_spec(Mod, UserTagSpec),
        dict:store(Name, Tag, Dict)
    end, Tags, Mod:registered_tags()).

-spec convert_user_tag_spec(name(), tag_spec()) ->
    {name(), dtl_tag:tag()}.
convert_user_tag_spec(Mod, {{WrapMod, WrapFun}, Name}) ->
    {Name, {{WrapMod, WrapFun, []}, {Mod, Name}}};
convert_user_tag_spec(Mod, {{WrapMod, WrapFun, Arg}, Name}) ->
    {Name, {{WrapMod, WrapFun, Arg}, {Mod, Name}}};
convert_user_tag_spec(Mod, Name) ->
    {Name, {Mod, Name}}.

%% @doc Search for a tag or filter in the given library dict.
-spec search_collection(library(), name()) ->
    dtl_tag:tag() | dtl_filter:filter() | nomatch.
search_collection(Coll, Name) ->
    case dict:find(Name, Coll) of
        error -> nomatch;
        {ok, Spec} -> {ok, Spec}
    end.
