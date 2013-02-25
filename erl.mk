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

# Common Erlang `make` targets.
#
# Targets:
# 	  program: Compile the erlang source code.
# 	  check (ct): Run common test.
# 	  plt: Build Dialyzer PLT file.
# 	  dialyzer: Run Dialyzer.
#
# Hook targets:
# 	  clean-pre: Run just before `clean'.
# 	  ct-pre: Run just before `ct' (Common Test).
#
# Flag macros:
# 	  PROGRAM: Required. The program name, used to infer .app.src path
# 	      and for reporting.
# 	  PREREQ_BEAMS: BEAM files which should be compiled before all
# 	      others. Use with behaviours and such.
# 	  ERLC: Erlang compiler executable.
# 	  EFLAGS: Erlang compiler flags.
# 	  DIALYZER: Dialyzer executable.
# 	  CT_RUN: Common Test executable.
# 	  CT_FLAGS: Extra command-line flags for Common Test.
# 	  CT_SUITES: A list of test suites that Common Test should run.

# Options
ERLC ?= erlc
EFLAGS ?=
DIALYZER ?= dialyzer
DIALYZER_FLAGS ?= -Wno_opaque
CT_RUN ?= ct_run

SHELL := bash

# Files that require compilation.
BEAMS := $(shell find src -name '*.erl' \
	| sed -e s/\.erl$$/.beam/ -e s/src\\//ebin\\//)

# Raw distributed module list.
MAIN_ERLS := $(shell find src -name '*.erl' -not -name '*tests.erl')
MODS := $(shell find src -name '*.erl' -not -name '*tests.erl' \
	-exec basename {} .erl \;)

# Distributed module list, as an erlang term.
#
# Note that this uses an unsafe array expansion. Don't use it files
# could contain spaces (this is never the case for Erlang modules).
MODLIST := $(shell bash -c 'mods=($(MODS)) ; IFS=, ; echo "[$${mods[*]}]"')

# These are separate because eventually we want to use the -j flag (job
# count limit) in the main BEAMS compilation.
program: ebin/$(PROGRAM).app $(PREREQ_BEAMS)
	@$(MAKE) $(BEAMS)

ebin/$(PROGRAM).app: src/$(PROGRAM).app.src
	mkdir -p ebin
	sed 's/{modules, \[\]}/{modules, $(MODLIST)}/' $< > $@

# TODO: Make beam files depend on .hrl files.

ebin/%.beam: src/%.erl
	$(ERLC) $(EFLAGS) -o ebin -pa ebin -I include $?

clean: ct-clean clean-pre
	rm -rf ebin

ct-clean:
	rm -rf logs

check: ct

ct: ct-clean program ct-pre
	mkdir -p logs
	$(CT_RUN) $(EFLAGS) $(CT_FLAGS) -pa ebin \
		-I include -dir test -logdir logs -suite $(CT_SUITES)

plt:
	$(DIALYZER) --build_plt --output_plt .$(PROGRAM).plt \
		--apps kernel stdlib

dialyze:
	$(DIALYZER) $(DIALYZER_FLAGS) \
		--src $(MAIN_ERLS) --plt .$(PROGRAM).plt --no_native

# Hook stubs.
clean-pre:
ct-pre:

.PHONY: clean clean-pre ct-clean check ct ct-pre
