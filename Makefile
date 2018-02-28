
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
	@hlint .

.PHONY: build clean test lint
