include Makefile.base

.PHONY: lint_all
lint_all:
	stack exec -- hlint -i 'Parse error' -i 'Reduce duplication' src lib

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
smoke_test: smoke_build smoke_test_only

.PHONY: integration_build
integration_build:
	docker build -t idream_tester .

.PHONY: integration_test_only
integration_test_only:
	docker run -v $(shell pwd)/test/integration_tests:/test -it idream_tester

.PHONY: integration_test
integration_test: integration_build integration_test_only
