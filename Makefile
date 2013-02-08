REBAR ?= rebar
REBAR_FLAGS ?= skip_deps=true

dtl:
	$(REBAR) compile $(REBAR_FLAGS)

test:
	$(REBAR) eunit $(REBAR_FLAGS)
eu: test

clean:
	$(REBAR) clean $(REBAR_FLAGS)

.PHONY: dtl test clean

