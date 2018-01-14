
# Helper Makefile used for some common targets.
# Requires stack / cabal / hlint to be installed.

# TODO add targets for testing, building demo, ...

build:
	@stack build

clean:
	@stack clean

lint:
	@hlint .


.PHONY: build clean lint
