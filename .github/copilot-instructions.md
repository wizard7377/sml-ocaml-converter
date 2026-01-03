# SML-to-OCaml Converter: AI Agent Instructions

## Project Purpose
This is an SML (Standard ML) to OCaml compiler/converter. It parses SML source code and translates it to equivalent OCaml code using a classic multi-phase compiler architecture.

## Architecture Overview

### Compiler Pipeline
The project follows **frontend → AST → backend** architecture:

```
SML source → Lexer (ocamllex) → Parser (Menhir) → AST → Backend (code generation) → OCaml code
```

### Key Modules

| Module | Location | Purpose |
|--------|----------|---------|
| **AST** | `lib/ast/ast.ml` | Complete SML type definitions: expressions, patterns, types, declarations, structures, signatures, functors |
| **Frontend** | `lib/frontend/` | Lexing (`lexer.mll`), parsing (`parser.mly`), produces AST from SML text |
| **Backend** | `lib/backend/backend.ml` | ~1350 lines: Converts SML AST to OCaml Parsetree for code generation |
| **Helpers** | `lib/helpers/` | `Ppxlib` integration for OCaml AST construction via `Builder` module |
| **Common** | `lib/common/` | Config types, file I/O, conversion flags for name/type/value transformations |
| **Main Library** | `lib/sml_ocaml_converter.ml` | Top-level API: `sml_to_ocaml`, `fun_to_ocaml`, `sig_to_ocaml` |
| **Binary** | `bin/main.ml` | CLI executable (uses `cmdliner`) |

## Critical Knowledge

### The AST is Comprehensive
The `Ast.prog` type recursively covers all SML language constructs:
- **Expressions**: Constants, identifiers, functions, records, pattern matching, exception handling, loops
- **Patterns**: All forms including constructor patterns, records, layered patterns, type annotations
- **Types**: Constructors, functions, tuples, records → OCaml objects
- **Declarations**: Values, functions, types, datatypes, exceptions, structures
- **Module System**: Structures, signatures, functors with where clauses and sharing constraints

### Backend Conversion Strategy
The backend uses **Ppxlib's Parsetree** (OCaml's AST type) for code generation:
- Maps SML AST nodes directly to `Parsetree` types using `Ppxlib.Ast_builder`
- Leverages OCaml's object types to represent SML records: `{x: int, y: int}` → `< x: int; y: int >`
- Handles keyword conflicts via name processing (when SML identifiers clash with OCaml keywords)

Key backend functions (mostly complete):
- `process_con` - Constants (integers, floats, strings, chars, words)
- `process_type` - Type conversions
- `process_exp` - Expression conversion (largest & most complex)
- `process_pat` - Pattern conversion
- `process_dec` - Declaration conversion (val, fun, datatype, exception, etc.)
- `process_prog` - Program-level entry point

### Parser & Lexer Architecture
- **Lexer** (`lexer.mll`): Tokenizes SML; should handle keywords, operators, literals
- **Parser** (`parser.mly`): Menhir grammar; builds `Ast.prog` from tokens
- Both are processed by dune during build and generate `Lexer` and `Parser` modules
- Frontend module exposes `parse : string -> Ast.prog`

## Development Workflow

### Build Commands
```bash
dune build                # Build entire project
dune test                 # Run tests
dune exec shibboleth -- <file>  # Run converter on SML file
dune build --watch       # Watch mode
dune clean               # Clean artifacts
```

### Configuration System
The `Common` module defines a `CONFIG` module type with a `config` value containing:
- Input/output file paths
- Verbosity level
- Conversion flags: `pattern_names`, `constructor_names_values`, `function_names`, `uncurry_types`, `uncurry_values`

The backend is parameterized on this config via `Make(Config : CONFIG)` functor pattern. This enables flexible conversion strategies.

### Testing
- Unit tests in `test/unit/`
- Makefile target: `make test` runs backend tests
- Example SML files in `examples/src/` (*.sml, *.fun, *.sig files)

## Code Patterns & Conventions

### Node Wrapping
SML AST nodes are wrapped in a `node` type containing value + comments metadata:
```ocaml
type 'a node = { value: 'a; comments: string list }
let box_node v = { value = v; comments = [] }
let unbox_node n = n.value
```
Use boxing/unboxing consistently when constructing/deconstructing AST.

### Location Handling
Backend uses **phantom locations** for generated code:
```ocaml
let ghost v = Location.mkloc v Location.none
```
This marks generated AST nodes as not from source.

### Name Processing
Identifiers go through `Process_names` module (signature: `CONFIG`-parameterized) to handle:
- SML-to-OCaml keyword conflicts
- Capitalization convention differences
- Conversion flags that may rename identifiers

### Record to Object Conversion
SML records convert to OCaml object types:
- Field order preserved
- Immutable object syntax: `< label: type; ... >`
- Record selector `#label expr` becomes lambda: `fun r -> r.label`

## Integration Points

### Frontend Output → Backend Input
Frontend `parse` function returns `Ast.prog`; backend expects same type. No intermediate representation—direct AST passing.

### Backend Output → Binary I/O
Backend produces `Parsetree.toplevel_phrase list`. Binary should:
1. Call top-level `sml_to_ocaml : string -> string` in main library
2. Write result to output file (currently stubbed in `common.ml`)
3. Use `cmdliner` for CLI argument parsing

### External Dependencies
- **ppxlib**: OCaml AST manipulation and code generation via quasiquotes
- **menhir**: Parser generator with --explain flag for conflict analysis
- **cmdliner**: CLI interface
- **alcotest**: Test framework (`:with-test` only)
- **ppx_deriving**: Auto-derive show/eq implementations for types

## Common Pitfalls & Tips

1. **Node wrapping**: Always `unbox_node` when extracting values from AST; always `box_node` when creating new nodes
2. **Operator associativity**: SML infix operators have different precedence than OCaml; parser must encode correctly
3. **Module system complexity**: Functors, signatures, and sharing constraints require careful mapping (see `process_str`, `process_sign` in backend)
4. **Pattern exhaustiveness**: OCaml's pattern checker is stricter; ensure SML patterns convert to exhaustive OCaml patterns
5. **Type variable syntax**: SML `'a list` vs OCaml `'a list` are identical, but type annotation ordering differs

## Files to Review First
1. [ast.ml](lib/ast/ast.ml) - Understand the complete SML language representation
2. [backend.ml](lib/backend/backend.ml) - Study completed functions to understand conversion patterns
3. [BACKEND_IMPLEMENTATION_STATUS.md](BACKEND_IMPLEMENTATION_STATUS.md) - Current backend completion status
4. [CLAUDE.md](CLAUDE.md) - Original project guidance (complements this file)
