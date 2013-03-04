#Django Template Language

A full-featured port of the Django template engine to Erlang.

**Warning:** The template engine is in a working but still "alpha"
state, particularly until error handling is cleaned up.

The custom tag and filter API works as do all rendering and lookup
functions. See [Built-in Tags and Filters](#7-built-in-tags-and-filters)
for an up-to-date table documenting tag and filter compatibility with
the Django defaults.

1. [Introduction](#1-introduction)
2. [Installation](#2-installation)
3. [Configuration](#3-configuration)
4. [Basic Usage](#4-basic-usage)
5. [Syntax](#5-syntax)
6. [Context and Context Processors](#6-context-and-context-processors)
7. [Built-in Tags and Filters](#7-built-in-tags-and-filters)
8. [Loader Modules](#8-loader-modules)
9. [Custom Tags and Filters](#9-custom-tags-and-filters)
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

and run `rebar get-deps`, then `rebar compile`. Refer to the [rebar
documentation](https://github.com/basho/rebar) if this is unclear.

##3. Configuration

These are the application-wide environment variables. Set them like you
do any other application options.

**settings\_module**: `module()` (Default: `dtl_app_config_settings`):
Module used to look up settings, must implement `dtl_settings`.

**context\_processors**: `[{module(), atom()}]` (Default: `[]`): List of
`{Mod, Fun}` tuples that are called in left-to-right order to populate
all new `dtl_context:context()` records.

**debug**: `boolean()` (Default: `false`): `true` to enable debugging
aids, `false` otherwise.

**template\_dirs**: `[list()]` (Default: ["priv/templates"]):
Template directories that `dtl_fs_loader` should search, from left.
Paths can be absolute or relative.

**template\_loaders**: `[module()]`
(Default: `[dtl_fs_loader, dtl_apps_loader]`): List of modules
implementing the `dtl_loader` interface. These are used to look up
templates at runtime.

**empty\_term\_replacement**: `[binary()]` (Default: `<<>>`): Binary
that will replace any `undefined` terms in templates. `"None"` and
`"undefined"` are also good ones.

**apps**: `[atom()]` (Default: `[]`): A list of application names that
will be searched in left-to-right order by `dtl_apps_loader`.

At the lowest level, these settings are managed by application env vars.
The defaults in the above table are all defined at this level.

Users can look up settings with `dtl:setting/1`:

    Apps = dtl:setting(apps).

It is not a good idea to change settings at runtime, but use a custom
settings module if you need this functionality and still be aware of
potential race conditions.


###3.1 Settings Modules

_Note: This functionality is unnecessary for the vast majority of users.
It is far more convenient to simply rely on the default settings module
and set the application environment variables described in the previous
section._

Settings modules are modules that implement the `dtl_settings`
behaviour. This module defines one callback, `setting/0`. They are
useful when automating tests that involve toggling DTL configuration.

`dtl_app_config_settings`, the default settings module, simply proxies
settings lookups to application env variable lookups. It's a very good
idea for user-defined settings modules to defer to
`dtl_app_config_settings` for settings that they do not define. Here is
an example of a settings module that overrides the `template_dirs`
setting:

    -module(news_settings).
    -behaviour(dtl_settings).

    -export([setting/2]).

    setting(template_dirs, _Default) ->
        ["/tmp/templates"];
    setting(Key, Default) ->
        dtl_app_config_settings:setting(Key, Default).

You can see another example with `dtl_ets_settings`, which is only used
for DTL tests.


##4. Basic usage

See "6. Context" for information on setting context variables in your
templates, and "8. Loader Modules" for information on where to store
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


##7. Built-in Tags and Filters

Tags: "load"
Filters: "upper", "lower"

Target: https://docs.djangoproject.com/en/dev/ref/templates/builtins/

TODO: Replace this with chart including tags and filters not yet
implemented.

##8. Loader Modules

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


##9. Custom Tags and Filters

Custom tags and filters are defined in a "Library," which is a simple
callback module that implements the `dtl_library` behaviour. A library
simply defines `registered_tags/0` and `registered_filters/0`, each of
which return a list of function names, the likes of which are described
in the sections below.

##9.1. Custom Tags

`registered_tags` returns a list of any of the following:

**NodeFunction**: `NodeFunction` is the name of a function that returns
    a `dtl_node:tnode()`. This may be a `dtl_node:unode()`, a list, or a
    binary.

    -behaviour(dtl_library).
    -export([registered_filters/0,
             registered_tags/0,
             color_orange/2,
             render_color_orange/2]).

    registered_filters() -> [].
    regisered_tags() -> [color_orange].

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
        Node = dtl_node:new("color_orange", {?MODULE, render_color_orange}),
        Node2 = dtl_node:set_nodelist(Node, Nodes),
        Node3 = dtl_node:set_state(Node2, N),
        {Node3, Parser3}.

    %% Renders the tag contents N times inside an orange block.
    render_color_orange(#dtl_node{nodelist = NodeList, state = N}, Ctx) ->
        ["<div class=\"orange\">",
            [dtl_node:render_list(NodeList) || X <- lists:seq(1, N)],
         "</div>"].

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

Simple tag is not needed for most cases, where the custom tag can choose
to simply return a list or binary (see `dtl_node:tnode()` definition).
Use something like the following for a named simple tag:

    my_simple_tag(Parser, _Token) ->
        {ok, dtl_node("my_simple_tag", fun (Node, Ctx) ->
             %% Render ...
         end, Parser)}.


##9.2. Custom Filters

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

Please report issues at https://github.com/oinksoft/dtl/issues


##11. FAQ

Empty.


##12. Support/Getting Help

Please report any bugs as issues at this github project.


##13. API Documentation

API documentation is regularly updated at https://oinksoft.com/doc/dtl/


##14. Roadmap

* Default tags and filters.
* Debug lexer and parser, better error handling.
* I18n support.
* OTP version?
