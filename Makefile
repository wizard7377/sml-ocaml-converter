SOURCES := $(wildcard lib/**/*.ml) $(wildcard lib/**/*.mli)
DUNE := dune
FILE_TEST_EXE := test/file_tests/file_tests.exe
UNIT_TEST_EXE := test/unit_tests/unit_tests.exe
DEV ?= 
DUNE_OPTS += $(if $(DEV), --profile dev, --profile release)
TEST_OPTS += 
.PHONY: test build install clean docs test_files format


test: build 
	@$(DUNE) exec $(DUNE_OPTS) $(FILE_TEST_EXE) -- $(TEST_OPTS)
	@$(DUNE) exec $(DUNE_OPTS) $(UNIT_TEST_EXE) -- $(TEST_OPTS)

test_files: 
	./tests.sh
build: 
	@$(DUNE) build $(DUNE_OPTS) 

install:
	@$(DUNE) install $(DUNE_OPTS)

clean:
	@$(DUNE) clean $(DUNE_OPTS)

docs: 
	@$(DUNE) build $(DUNE_OPTS) @doc @doc-private

format:
	dune fmt