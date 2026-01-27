SOURCES := $(wildcard lib/**/*.ml) $(wildcard lib/**/*.mli)
DUNE := dune
FILE_TEST_EXE := test/file_tests/file_tests.exe
UNIT_TEST_EXE := test/unit_tests/unit_tests.exe
DEV ?= 
BUILD_DOC?=odig odoc
DUNE_OPTS += $(if $(DEV), --profile dev, --profile release)
TEST_OPTS+=
DUNE_ROOT:=.

.PHONY: test build install clean docs test_files test_twelf format


test: build
	@$(DUNE) exec $(DUNE_OPTS) $(FILE_TEST_EXE) -- $(TEST_OPTS)
	@$(DUNE) exec $(DUNE_OPTS) $(UNIT_TEST_EXE) -- $(TEST_OPTS)

test_twelf: build
	@$(DUNE) exec $(DUNE_OPTS) $(UNIT_TEST_EXE) -- test "Twelf Integration" $(TEST_OPTS)

test_files: 
	DUNE_ROOT=1 dune runtest tests.t
test_group:
	dune exec shibboleth -- group $(TEST_OPTS) --guess-var "[A-Z]\(\(s\)?\([0-9]\)\|'\)?" --input $(DUNE_ROOT)/examples/twelf/src --force --output $(DUNE_ROOT)/examples/output/twelf     
test_group_smlnj:
	dune exec shibboleth -- group $(TEST_OPTS) --guess-var "[A-Z]\(\(s\)?\([0-9]\)\|'\)?" --input $(DUNE_ROOT)/examples/smlnj --force --output $(DUNE_ROOT)/examples/output/smlnj
# DUNE_ROOT=1 dune runtest files.t
build: 
	@$(DUNE) build $(DUNE_OPTS) 

install:
	@$(DUNE) install $(DUNE_OPTS)

clean:
	@$(DUNE) clean $(DUNE_OPTS)

docs: 
	$(BUILD_DOC) shibboleth

format:
	dune fmt

release: 
	rm ./*.opam 


include examples/TestSetup.mk
