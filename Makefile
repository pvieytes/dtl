REBAR   ?= rebar
RFLAGS  ?= skip_deps=true
CTFLAGS ?= suites=eunit verbose=1
DEPS    = deps

dtl:
	$(REBAR) compile $(RFLAGS)

ct: ct-clean dtl
	$(REBAR) ct $(RFLAGS) $(CTFLAGS)

clean: ct-clean
	$(REBAR) clean $(RFLAGS)

ct-clean:
	rm -rf logs

deps:
	$(REBAR) get-deps

.PHONY: dtl ct clean ct-clean deps
