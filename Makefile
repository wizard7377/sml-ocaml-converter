SOURCES := $(wildcard lib/**/*.ml) $(wildcard test/unit/**/*.ml)
DUNE := dune
TEST_EXE := test/unit/backend.exe
OPTS += 
.PHONY: test 
test: $(SOURCES)
	$(DUNE) exec $(TEST_EXE) -- $(OPTS)

build:
	$(DUNE) build

install:
	$(DUNE) install