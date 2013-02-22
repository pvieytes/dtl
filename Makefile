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
#     CT_FLAGS: Extra options for Common Test.

PROGRAM = dtl
PREREQ_BEAMS = ebin/dtl_loader.beam

include Makefile.common

TEST_APP = test/eunit_SUITE_data/test_app

clean-pre:
	$(MAKE) -C $(TEST_APP) clean

CT_FLAGS = -pa $(TEST_APP)/ebin 
CT_SUITES = eunit_SUITE

ct-pre:
	$(MAKE) -C $(TEST_APP)

.PHONY: clean-pre ct-pre
