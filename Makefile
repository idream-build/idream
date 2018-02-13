
# Helper Makefile used for some common targets.
# Requires stack / cabal / hlint to be installed.

# TODO add targets for building demo, ...

build:
	@stack build

clean:
	@stack clean

test:
	@stack test

lint:
	@hlint .

.PHONY: build clean test lint
