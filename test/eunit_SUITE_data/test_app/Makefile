PROGRAM = test_app

program: ebin/$(PROGRAM).app

ebin/$(PROGRAM).app: src/$(PROGRAM).app.src
	mkdir -p ebin
	cp $< $@

clean:
	rm -rf ebin
