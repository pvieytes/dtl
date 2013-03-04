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

%% @doc Tags are functions that return callbacks which in turn return
%%      nodelists (see `dtl_node'). They are used to transform templates
%%      in ways that filters cannot because they can consume tokens,
%%      whereas filters only replace the explicitly filtered content
%%      (see `dtl_filter').
-module(dtl_tag).

-type tag() :: {module(), dtl_library:name()}
             | {{module(), atom()}, {module(), dtl_library:name()}, term()}.

-export([inclusion_tag/5,
         render_inclusion_tag/2,
         run/3]).
-export_type([tag/0]).

-spec run(tag(), dtl_parser:parser(), dtl_lexer:token()) ->
    {ok, dtl_node:tnode(), dtl_parser:parser()} | {error, atom()}.
run({{WrapMod, WrapFun, Arg}, {Mod, Fun}}, Parser, Token) ->
    WrapMod:WrapFun(Arg, Mod, Fun, Parser, Token);
run({Mod, Fun}, Parser, Token) ->
    Mod:Fun(Parser, Token).

inclusion_tag(Name, Mod, Fun, Parser, _Token) ->
    %% TODO: Break up the token into args/options.
    Args = Opts = [],
    Node = dtl_node:new("inclusion_tag", {?MODULE, render_inclusion_tag}),
    {ok, Tpl} = case Name of
        [[_|_]|_] -> dtl_loader:select_template(Name);
        _ -> dtl_loader:get_template(Name)
    end,
    Node2 = dtl_node:set_nodelist(Node, dtl_template:nodelist(Tpl)),
    Node3 = dtl_node:set_state(Node2, {Mod, Fun, Args, Opts}),
    {ok, Node3, Parser}.

render_inclusion_tag(Node, Ctx) ->
    {Mod, Fun, Args, Opts} = dtl_node:state(Node),
    Ctx2 = dtl_context:update(Ctx, Mod:Fun(Args, Opts)),
    {ok, Bin} = dtl_node:render_list(dtl_node:nodelist(Node), Ctx2),
    Bin.
