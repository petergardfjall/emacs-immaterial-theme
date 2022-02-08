SANDBOX_DIR?=sandbox

# prepare emacs sandbox with local library and dependencies installed
sandbox:
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) --install-deps -vv

# run emacs in sandbox
.PHONY: emacs
emacs: sandbox
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive

clean:
	rm -rf $(SANDBOX_DIR)


.PHONY=test
test: sandbox
        # not sure why `interactive` is necessary, but the tests choke on
        # `batch`, because `color-name-to-rgb` returns `(1.0, 0.0, 0.0)` for
        # `#aa0000` rather than the expected `(0.666.., 0.0, 0.0)`
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) interactive -vv -- -l tests/immaterial-test.el --eval "(immaterial-test-suite)"
