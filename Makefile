
# Helper Makefile used for some common targets.
# Requires stack / cabal / hlint to be installed.

# TODO add targets for building demo, ...

build:
	@stack build -j 8

clean:
	@stack clean

test:
	@stack test -j 8

lint:
	@hlint src/ lib/

continuous:
	@stack build --fast --file-watch -j8

# TODO(ejconlon) Any other useful dev deps?
dev:
	@stack build hlint intero

install:
	@stack install idream

prepare_smoke_test:
	rm -rf test_project
	idream new test_project
	cd test_project && idream add --lib test_lib && idream add --exe test_exe

smoke_test:
	cd test_project && idream compile

integration_test:
	@docker build -t idream_tester .
	@docker run -v $(shell pwd)/test/integration_tests:/test -it idream_tester

.PHONY: build clean lint test continuous integration_test
