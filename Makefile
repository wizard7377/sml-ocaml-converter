SOURCES := $(wildcard lib/**/*.ml) $(wildcard test/unit/**/*.ml)
DUNE := dune
TEST_EXE := test/unit/backend.exe
OPTS += 
.PHONY: test build install clean docs
test: $(SOURCES)
	$(DUNE) exec $(TEST_EXE) -- $(OPTS)

build:
	$(DUNE) build

install:
	$(DUNE) install

clean:
	$(DUNE) clean

docs: 
	@$(DUNE) build @doc @doc-private