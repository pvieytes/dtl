#Django Template Language

A full-featured port of the Django template engine to Erlang.

**NOTE: While this template engine's mechanics are in a working state,
        I've yet to write most of the basic node renderers.

        This notice will be removed once I've made the first tagged
        release and the project is in a truly usable state.**

1. [Introduction](#1-introduction)
2. [Installation](#2-installation)
3. [Configuration](#3-configuration)
4. [Basic Usage](#4-basic-usage)
5. [Syntax](#5-syntax)
6. [Context and Context Processors](#6-context-and-context-processors)
7. [Loader Modules](#7-loader-modules)
8. [Custom Filters](#8-custom-filters)
9. [Custom Tags](#9-custom-tags)
10. [Troubleshooting](#10-troubleshooting)
11. [FAQ](#11-faq)
12. [Support/Getting Help](#12-supportgetting-help)
13. [API Documentation](#13-api-documentation)
14. [Roadmap](#14-roadmap)


##1. Introduction

This project is an effort to fully implement the Django template engine
in Erlang. I hope to create a feature-complete port, using the same data
types and striving for parity with the Python API and the base
filter/tag set included in Django.


##2. Installation

To install the latest version, add this to your dependency list in
rebar.config:

    {dtl, ".*", {git, "git://github.com/oinksoft/dtl.git", "master"}}

and run `rebar get-deps` and `rebar compile`. Refer to the [rebar
documentation](https://github.com/basho/rebar) if this is unclear.

##3. Configuration

The following are the configuration keys for the `dtl` app, their
expected types, and any default values:

|Key                  |Type                 |Default                           |
|---------------------|---------------------|----------------------------------|
|apps                 |`[atom()]`           |`[]`                              |
|debug                |`boolean()`          |`false`                           |
|context\_processors  |`[{atom(), atom()}]` |`[]`                              |
|template\_dirs       |`[list()]`           |`[]`                              |
|template\_loaders    |`[atom()]`           |`[dtl_fs_loader, dtl_apps_loader]`|

**apps**: A list of apps that the `dtl_apps_loader` should use.

**debug**: Set `true` to allow more detailed debugging output in
    rendered templates, `false` otherwise.

**template\_dirs**: A list of arbitrary file system locations where
    `dtl_fs_loader` will look for templates.

**template\_loaders**:
    A list of modules implementing the `dtl_loader` behaviour. During
    template lookup, they will be tried in the order specified.


##4. Basic usage

See "5. Context" for information on setting context variables in your
templates, and "6. Loader Modules" for information on where to store
your template files.

Render a template:

    {ok, Html} = dtl:render("index.html", [
        {title, "The World Wide Web"},
        {visitor_count, 12}
    ]).

Create a template from a string, create a plain context with one item
set, and render it:

    Source = "My name is {{ name }}.",
    {ok, Tpl} = dtl_template:new(Source),
    Ctx = dtl_context:new([
        {name, "Thomas"}
    ]),
    {ok, <<"My name is Thomas">>} = dtl_template:render(Tpl, Ctx).

Find a template and render it:

    Tpl = dtl_loader:get_template("index.html"),
    {ok, Html} = dtl_template:render(Tpl),
    %% ...

Render the first of several found templates:

    Tpl = dtl_loader:select_template(["index.html", "index.htm"]),
    {ok, Html} = dtl_template:render(Tpl),
    %% ...


##5. Syntax

Template syntax is identical to Django template syntax. Please report
any observable differences.

    https://docs.djangoproject.com/en/dev/topics/templates/


##6. Context and Context Processors

Contexts are the primary means of transmitting data from application
code to Django templates. Any value that is accessible on a context
will be accessible in any template into which the context is loaded:

    Ctx = dtl_context:new([
        {foo, "Foo"},
        {bar, "Bar"}
    ]),
    {ok, Bin} = dtl:render(Tpl, Ctx).


###6.1. Context Processors

A user may specify a list of {Mod, Fun} tuples which will be called, in
order, when initializing a new context. Each function should return a
property list. Here is an example context processor:

    process_time() ->
        Time = calendar:local_time(),
        [{Year, Month, Day}, {Hours, Minutes, Seconds}] = Time,
        [{date, io_lib:format("~p-~p-~p", [Year, Month, Day])},
         {time, io_lib:format("~p:~p:~p", [Hours, Minutes, Seconds])}].

Context processors are specified in application config.

    application:set_env(dtl, context_processors, [{my_app, process_time}]).

Now, a template could access `time` and `date` variables.


##7. Loader Modules

DTL comes with two template loader modules, which are described here:

**dtl\_fs\_loader**: This loader tries each of the configured
    `template_dirs`, in order, to see if the named template exists in
    one of them. Only templates contained in one of these directories
    will be found.

**dtl\_apps\_loader**: This loader searches in "templates" in the "priv"
    directory of each app specified with the `apps` configuration
    option.  That is, "index.html" would be searched for at
    foo/priv/templates/index.html if `foo` were included in the `apps`
    configuration option.

You can also implement your own loaders. Here is a loader that tries to
copy a template from a web service (!):

    -module(http_loader).
    -behaviour(dtl_loader).

    -define(BASE_URL, "http://example.com/?img_name=").

    -export([is_usable/0,
             load_template_source/1,
             load_template_source/2]).

    %% A loader must implement is_usable/0. This callback is so that
    %% loaders that are only useful in certain environments (say, a
    %% memcached-backed loader) are not used.
    %%
    %% For instance, this function could test to see if ?BASE_URL's host
    %% is reachable.
    is_usable() -> true.

    %% A loader must implement load_template_source/1 and
    %% load_template_source/2. This is to match the Django API, where a
    %% `dirs' argument must be accepted even for loaders that are not
    %% concerned with this detail.
    %%
    %% This function should return a {ok, Content, DisplayName} triple
    %% where Content is the template string and DisplayName is a name
    %% for the found template, which will be used in debugging outputs.
    %%
    %% It should return {error, not_found} if the template is not found.
    %% Any other error return will immediately halt the lookup process.
    load_template_source(Name) -> load_template_source(Name, []).
    load_template_source(Name, _Dirs) ->
        %% Assume our application has already started `inets'.
        Url = ?BASE_URL ++ Name,
        %% Anything other than a 200 response is "not found".
        case httpc:request(Url) of
            {ok, {_Proto, 200, _Msg}, _Headers, Body} ->
                {ok, Body, Url};
            _ -> {error, not_found};
        end.


##8. Custom Tags and Filters

Custom tags and filters are defined in a "Library," which is a simple
callback module that implements the `dtl_library` behaviour. A library
simply defines `registered_tags/0` and `registered_filters/0`, each of
which return a list of function names, the likes of which are described
in the sections below.

##8.1. Custom Tags

`registered_tags` returns a list of any of the following:

**NodeFunction**: `NodeFunction` is the name of a function that returns
    a `dtl_node()` which should have its `renderer` field set and record
    any important information about the node in its `state` field. The
    renderer function should be either a `fun` object or be specified as
    a module-function in the form `{Mod, Fun}`.

    -behaviour(dtl_library).
    -export([registered_filters/0,
             registered_tags/0,
             color_orange/2,
             render_color_orange/2,
             thumbnail/2]).
    registered_filters() -> [].
    regisered_tags() -> [{simple_tag, fish_name},
                         {simple_tag, thumbnail}].

    %% This function extracts the number of repititions from the tag
    %% token and saves this in the new node's state.
    %%
    %% {% color_orange 1 %} Hello {% endcolor_orange %} ->
    %% <div class="orange"> Hello </div>
    color_orange(Parser, Token) ->
        [<<NBin/binary>>] = dtl_token:split_contents(Token),
        N = list_to_integer(binary_to_list(NBin)),
        {Nodes, Parser2} = dtl_parser:parse("endcolor_orange"),
        Parser3 = dtl_parser:delete_first_token(Parser2),
        {#dtl_node{nodelist = Nodes,
                   renderer = {?MODULE, render_color_orange},
                   state = N}, Parser3}.

    %% Renders the tag contents N times inside an orange block.
    render_color_orange(#dtl_node{nodelist = NodeList, state = N}, Ctx) ->
        ["<div class=\"orange\">",
            [dtl_node:render_list(NodeList) || X <- lists:seq(1, N)],
         "</div>"].

**{simple\_tag, RenderFunction}**: `RenderFunction` is the name of a
    function that accepts two arguments: One for the positional
    arguments to the tag and one for the keyword arguments. This
    function should return a list, binary, or iolist:

    -behaviour(dtl_library).
    -export([registered_filters/0,
             registered_tags/0,
             fish_name/2,
             thumbnail/2]).
    registered_filters() -> [].
    regisered_tags() -> [{simple_tag, fish_name},
                         {simple_tag, thumbnail}].

    %% This takes no arguments: {% fish_name %} -> "Blue fish".
    fish_name([], []) ->
        lists:nth(random:uniform(4), ["One fish", "Two fish",
                                      "Red fish", "Blue fish"]).

    %% This one takes one positional and one keyword argument:
    %%     <img src="/static/{% thumbnail "foo" quality=70 %}">
    thumbnail([Filename], Options) ->
        Quality = proplists:get_value(quality, Options, 85),
        %% This returns the path of the thumbnail file relative to the
        %% static root, generating the new image if needed.
        {ok, ThumbFile} = thumbnail_server:get_image(Filename, Quality),
        ThumbFile.

**{inclusion\_tag, TemplateName, ContextFunction}**: TemplateName is the
    name of the template this tag includes. ContextFunction is a
    function that can inject data into this template by returning a
    `dtl_context()` or a proplist. It receives any arguments passed to
    the inclusion tag as positional and keyword argument lists.

    -behaviour(dtl_library).
    -export([registered_filters/0,
             registered_tags/0,
             pretty_box/2]).
    registered_filters() -> [].
    regisered_tags() -> [{inclusion_tag, "pretty-box.html", pretty_box}].

    %%%% pretty-box.html:
    %% <div class="so" style="color: {{ color }}">
    %%   <div class="many">
    %%     <div class="lovely">
    %%       <div class="elements">
    %%         <h1>{{ title }}</h1>
    %%       </div>
    %%     </div>
    %%   </div>
    %% </div>

    %% Renders a pretty box with the provided title and optional color.
    %%     {% pretty_box "March" color="#f90" %}
    pretty_box([Title], Options) ->
        Color = proplists:get_value(color, Options, "#fc0"),
        [{color, Color},
         {title, Title}].

##8.2. Custom Filters

Custom filters are functions that can accept a list of colon-separated
arguments. They must return a list, binary, or iolist.

    -behaviour(dtl_library).
    -export([registered_filters/0,
             registered_tags/0,
             add/2,
             reverso/2]).
    registered_filters() -> [reverso].
    regisered_tags() -> [].

    %% Reverses its filter:
    %%   {{ "Cat"|reverso }} -> "taC"
    %%
    %% _Args is [], because this should never receive arguments.
    reverse(Text, []) ->
        list_to_binary(lists:reverse(binary_to_list(Text))).

    %% Adds to the first number:
    %%     {{ 1|add:2 }}
    add(X, [Y]) -> integer_to_list(X + Y).


##10. Troubleshooting

Empty.


##11. FAQ

Empty.


##12. Support/Getting Help

Empty.


##13. API Documentation

API functions are all documented in the source code, formatted
documentation is a work in progress.


##14. Roadmap

* Library management.
* Base Django tags and filters.
* Tag interfaces.
* Filter interfaces.
* Debug lexer and parser.
