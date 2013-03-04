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

%% @doc Core template data type and functions. Controls high-level
%%      template operations.
-module(dtl_template).

-export([new/1,
         render/2]).

%% Templates, this program's core data type. These are the compiled
%% representation of string templates and all template rendering occurs
%% via an internal node list.
-record(tpl, {nodelist = [] :: [dtl_node:tnode()]}).
-opaque template() :: #tpl{}.
-export_type([template/0]).

%% @doc Compiles the provided template source, returning the compiled
%%      representation, suitable for use with other functions in this
%%      module.
-spec new(binary()) -> template().
new(Str) ->
    #tpl{nodelist = compile_string(Str)}.

%% @doc Renders the provided template with the context (stub).
-spec render(template(), dtl_context:context()) ->
    {ok, binary(), dtl_context:context()} | {error, atom()}.
render(#tpl{nodelist = NodeList}, Ctx) ->
    %% Push onto the render context: This is so that {% assign %} and
    %% similar tags can modify the context safely.
    RenderCtx = dtl_context:render_context(Ctx),
    Ctx2 = dtl_context:set_render_context(Ctx, dtl_context:push(RenderCtx)),
    {ok, OutList} = dtl_node:render_list(NodeList, Ctx2),
    %% No need to pop the render context, just reuse the original one.
    {ok, iolist_to_binary(OutList), Ctx}.

%% @doc Compile a string to a nodelist.
-spec compile_string(binary()) -> [dtl_node:tnode()].
compile_string(Str) ->
    {LexerMod, ParserMod} = get_compiler(dtl:setting(debug)),
    Tokens = LexerMod:tokenize(Str),
    Parser = ParserMod:new(Tokens),
    {ok, NodeList, _Parser2} = ParserMod:parse(Parser),
    NodeList.

%% @doc `true' for Debug mode, `false' otherwise.
-spec get_compiler(boolean()) -> {atom(), atom()}.
get_compiler(true) -> {dtl_debug_lexer, dtl_debug_parser};
get_compiler(false) -> {dtl_lexer, dtl_parser}.
