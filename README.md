# Shibboleth: SML-to-OCaml Converter

Converts Standard ML (SML) source code to OCaml using a compiler pipeline: lexing → parsing → AST → code generation.

## Building

```bash
make build      # Build project
make install    # Install binary
make test       # Run tests
make docs       # Generate documentation
make clean      # Clean artifacts
```

## Usage

```bash
# Convert a single SML file
shibboleth file input.sml -o output.ml

# Convert multiple files
shibboleth file file1.sml file2.sml -o output.ml

# Use as a library
open Shibboleth
let ocaml_code = sml_to_ocaml sml_source_code
```

## Architecture

```
SML source → Lexer (ocamllex) → Parser (Menhir) → AST → Backend (Ppxlib) → OCaml code
```

**Key Components:**
- `lib/frontend/` - Lexical analysis and parsing
- `lib/backend/` - Converts SML AST to OCaml Parsetree
- `lib/cli/` - Command-line interface

## Requirements

- OCaml ≥ 4.14
- Dune ≥ 3.20
- Dependencies: ppxlib, menhir, cmdliner

## License

BSD-2-Clause - See [LICENSE](LICENSE)

**Author:** Asher Frost <wizardishlike@gmail.com>  
**Repository:** [github.com/wizard7377/sml-ocaml-converter](https://github.com/wizard7377/sml-ocaml-converter)

