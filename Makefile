SHELL := bash

# Files that require compilation.
BEAMS := ebin/dtl_loader.beam \
	 $(shell find src -name '*.erl' \
		| sed -e s/\.erl$$/.beam/ -e s/src\\//ebin\\//)
# Raw distributed module list.
MODS := $(shell find src -name '*.erl' -not -name '*tests.erl' \
	-exec basename -s .erl {} \;)
# Distributed module list, as an erlang term.
MODLIST := $(shell bash -c 'mods=($(MODS)) ; IFS=, ; echo "[$${mods[*]}]"')

dtl: ebin/dtl.app $(BEAMS)

ebin/dtl.app: src/dtl.app.src
	mkdir -p ebin
	sed 's/{modules, \[\]}/{modules, $(MODLIST)}/' $< > $@

ebin/%.beam: src/%.erl
	erlc -Werror -o ebin -pa ebin -I include $?

clean: ct-clean
	rm -rf ebin

ct-clean:
	rm -rf logs

check: ct

ct: ct-clean dtl
	mkdir -p logs
	ct_run -pa ebin -I include -dir test -logdir logs -suite eunit_SUITE

build-plt: dtl
	dialyzer --build_plt --output_plt .dtl.plt --apps kernel stdlib

dialyze:
	dialyzer --src src --plt .dtl.plt --no_native

.PHONY: dtl clean ct-clean check ct
