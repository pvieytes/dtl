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
#     check (test): Run DTL's test suite.
#     dialyze: Run Dialyze over the codebase.
#     plt: Build files necessary for running `dialyze'.
#
# Options:
#     ERLC: The path to the Erlang compiler executable.
#     EFLAGS: Options for the Erlang compiler.
#     DIALYZER: The path to the Dialyzer executable.
#     CT_RUN: The path to the Common Test executable.
#     CT_FLAGS: Extra options for Common Test.

SHELL := bash
PROGRAM = dtl

all: program

# Options
ERL ?= erl
ERLC ?= erlc
EFLAGS ?=

DIALYZER ?= dialyzer
DIALYZER_FLAGS ?= -Wno_opaque

CT_RUN ?= ct_run
CT_FLAGS = -pa $(TEST_APP)/ebin 
CT_SUITES = eunit_SUITE

PREREQ_BEAMS = ebin/dtl_library.beam \
	       ebin/dtl_loader.beam \
	       ebin/dtl_settings.beam

# Files that require compilation.
BEAMS = $(shell find src -name '*.erl' \
	| sed -e s/\.erl$$/.beam/ -e s/src\\//ebin\\//)

# Raw distributed module list.
MAIN_ERLS = $(shell find src -name '*.erl' -not -name '*tests.erl')
MODS = $(shell find src -name '*.erl' -not -name '*tests.erl' \
	-exec basename {} .erl \;)

# Distributed module list, as an erlang term.
#
# Note that this uses an unsafe array expansion. Don't use it files
# could contain spaces (this is never the case for Erlang modules).
MODLIST = $(shell bash -c 'mods=($(MODS)) ; IFS=, ; echo "[$${mods[*]}]"')

TEST_APP = test/eunit_SUITE_data/test_app

# These are separate because eventually we want to use the -j flag (job
# count limit) in the main BEAMS compilation.
program: ebin/$(PROGRAM).app $(PREREQ_BEAMS)
	@$(MAKE) $(BEAMS)

clean: ct-clean doc-clean
	$(MAKE) -C $(TEST_APP) clean
	rm -rf ebin

ct-clean:
	rm -rf logs

doc-clean:
	rm -rf doc

ebin/$(PROGRAM).app: src/$(PROGRAM).app.src
	mkdir -p ebin
	sed 's/{modules, \[\]}/{modules, $(MODLIST)}/' $< > $@

# TODO: Make beam files depend on .hrl files.

ebin/%.beam: src/%.erl
	$(ERLC) $(EFLAGS) -o ebin -pa ebin -I include $?

check: ct

ct: ct-clean program
	$(MAKE) -C $(TEST_APP)
	mkdir -p logs
	$(CT_RUN) $(EFLAGS) $(CT_FLAGS) -pa ebin \
		-I include -dir test -logdir logs -suite $(CT_SUITES)

doc: $(MAIN_ERLS)
	mkdir -p doc
	touch doc
	./edoc.escript

plt:
	$(DIALYZER) --build_plt --output_plt .$(PROGRAM).plt \
		--apps kernel stdlib

dialyze:
	$(DIALYZER) $(DIALYZER_FLAGS) \
		--src $(MAIN_ERLS) --plt .$(PROGRAM).plt --no_native

.PHONY: clean clean-pre ct-clean doc-clean check ct ct-pre
