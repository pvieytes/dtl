# Copyright (c) 2013- Thomas Allen <thomas@oinksoft.com>
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Run this Makefile with a copy of GNU Make:
#     <http://www.gnu.org/software/make/>
#
# Targets:
#     all (dtl): Compile DTL's erlang sources.
#     check: Run DTL's test suite.
#     dialyze: Run Dialyze over the codebase.
#     plt: Build files necessary for running `dialyze'.
#
# Options:
#     ERLC: The path to the Erlang compiler executable.
#     EFLAGS: Options for the Erlang compiler.
#     DIALYZER: The path to the Dialyzer executable.
#     CT_RUN: The path to the Common Test executable.

# Options
ERLC ?= erlc
EFLAGS ?=
DIALYZER ?= dialyzer
CT_RUN ?= ct_run

SHELL := bash

# Files that require compilation.
BEAMS := ebin/dtl_loader.beam \
	 $(shell find src -name '*.erl' \
		| sed -e s/\.erl$$/.beam/ -e s/src\\//ebin\\//)

# Raw distributed module list.
MODS := $(shell find src -name '*.erl' -not -name '*tests.erl' \
	-exec basename -s .erl {} \;)

# Distributed module list, as an erlang term.
#
# Note that this uses an unsafe array expansion. Don't use it files
# could contain spaces (this is never the case for Erlang modules).
MODLIST := $(shell bash -c 'mods=($(MODS)) ; IFS=, ; echo "[$${mods[*]}]"')

dtl: ebin/dtl.app $(BEAMS)

ebin/dtl.app: src/dtl.app.src
	mkdir -p ebin
	sed 's/{modules, \[\]}/{modules, $(MODLIST)}/' $< > $@

ebin/%.beam: src/%.erl
	$(ERLC) $(EFLAGS) -Werror -o ebin -pa ebin -I include $?

clean: ct-clean
	rm -rf ebin

ct-clean:
	rm -rf logs

check: ct

ct: ct-clean dtl
	mkdir -p logs
	$(CT_RUN) $(EFLAGS) -pa ebin -I include -dir test -logdir logs \
		-suite eunit_SUITE

plt:
	$(DIALYZER) --build_plt --output_plt .dtl.plt --apps kernel stdlib

dialyze:
	$(DIALYZER) --src src --plt .dtl.plt --no_native

.PHONY: dtl clean ct-clean check ct
