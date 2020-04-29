.PHONY: all test-env test clean

all:
	@rebar3 compile

test-env:
	@scripts/setup-test-env.sh

test:
	@rebar3 ct -v

clean:
	@rebar3 clean
	@rm -rf _build
