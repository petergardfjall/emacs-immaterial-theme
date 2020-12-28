SANDBOX_DIR?=sandbox

.PHONE=test
test:
        # not sure why `interactive` is necessary, but the tests choke on
        # `batch`, because `color-name-to-rgb` returns `(1.0, 0.0, 0.0)` for
        # `#aa0000` rather than the expected `(0.666.., 0.0, 0.0)`
	./bin/makem.sh --sandbox=$(SANDBOX_DIR) --install-deps interactive -vv -- -l tests/immaterial-test.el --eval "(immaterial-test-suite)"
