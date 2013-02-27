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

-type filter_spec() :: atom().
-type tag_spec() :: atom()
                  | {inclusion_tag, list(), atom()}
                  | {simple_tag, atom()}.

-callback registered_filters() -> [filter_spec()].
-callback registered_tags() -> [tag_spec()].

-export([add_filters/2,
         add_tags/2]).
-export_type([filter_spec/0,
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
    lists:foldl(fun
        ({simple_tag, Fun}, Dict) ->
            dict:store(Fun, {simple_tag, Mod, Fun}, Dict);
        ({inclusion_tag, Path, Fun}, Dict) ->
            dict:store(Fun, {inclusion_tag, Path, Mod, Fun}, Dict);
        (Tag, Dict) ->
            dict:store(Tag, {normal_tag, Mod, Tag}, Dict)
    end, Tags, Mod:registered_tags()).
