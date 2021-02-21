include Makefile.base

.PHONY: install
install:
	stack build idream --test --no-run-tests --copy-bins

.PHONY: smoke_build
smoke_build:
	rm -rf test_project
	idream new test_project
	cd test_project && idream add --lib test_lib && idream add --exe test_exe

.PHONY: smoke_test_only
smoke_test_only:
	cd test_project && idream fetch && idream generate-ipkg && idream compile
	test_project/.idream-work/build/test_project/test_exe/test_exe

.PHONY: smoke_test
smoke_test: install smoke_build smoke_test_only

.PHONY: integration_build
integration_build:
	bash ./script/build_images.sh

.PHONY: integration_repl
integration_repl:
	bash ./script/integration_repl.sh

.PHONY: integration_test_only
integration_test_only:
	bash ./script/integration_test.sh

.PHONY: integration_test
integration_test: integration_build integration_test_only
