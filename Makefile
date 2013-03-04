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
#     deps: Install all dependencies (requires `rebar').
#     dialyze: Run Dialyze over the codebase.
#     plt: Build files necessary for running `dialyze'.
#
# Options:
#     DIALYZER: The path to the Dialyzer executable.
#     CT_RUN: The path to the Common Test executable.
#     CT_FLAGS: Extra options for Common Test.

SHELL := bash
PROGRAM = dtl
REBAR ?= rebar
DEPS = deps

all: program

get-deps: rebar.config
	$(REBAR) get-deps

deps: get-deps
	$(REBAR) compile

DIALYZER ?= dialyzer
DIALYZER_FLAGS ?= -Wno_opaque

CT_RUN ?= ct_run
CT_FLAGS = -pa $(TEST_APP)/ebin -pa $(DEPS)/*/ebin
CT_SUITES = eunit_SUITE

TEST_APP = test/eunit_SUITE_data/test_app

program:
	$(REBAR) compile skip_deps=true

clean: ct-clean doc-clean
	$(REBAR) clean
	$(MAKE) -C $(TEST_APP) clean

ct-clean:
	rm -rf logs

doc-clean:
	rm -rf doc

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
