include Makefile.base

.PHONY: install
install:
	stack build idream --test --no-run-tests --copy-bins

.PHONY: smoke_create
smoke_create:
	rm -rf smoke_project
	idream new smoke_project
	idream --project-dir smoke_project add smoke_lib --package-type library
	idream --project-dir smoke_project add smoke_test --package-type test
	idream --project-dir smoke_project add smoke_exe --package-type executable

.PHONY: smoke_test_only
smoke_test_only:
	idream --project-dir smoke_project fetch
	idream --project-dir smoke_project compile
	idream --project-dir smoke_project test

.PHONY: smoke_test
smoke_test: install smoke_create smoke_test_only

.PHONY: integration_build
integration_build:
	bash ./script/build_images.sh

.PHONY: integration_repl
integration_repl:
	bash ./script/integration_repl.sh

# .PHONY: integration_test_only
# integration_test_only:
# 	bash ./script/integration_test.sh

# .PHONY: integration_test
# integration_test: integration_build integration_test_only
