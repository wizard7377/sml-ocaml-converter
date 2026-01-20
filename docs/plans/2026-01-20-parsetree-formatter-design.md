# Parsetree Pretty-Printer Design

**Date:** 2026-01-20
**Purpose:** Implement OCaml Parsetree pretty-printing for code generation using Fmt combinators
**Status:** Approved

## Overview

Create a complete pretty-printer for OCaml's `Parsetree` types in `lib/common/format.ml` that wraps the standard library's `Pprintast` module while providing Fmt-style interfaces consistent with the codebase.

## Goals

- **Primary:** Generate syntactically correct OCaml code from Parsetree AST nodes
- **Secondary:** Provide Fmt-compatible interface (`type 'a t = Format.formatter -> 'a -> unit`)
- **Constraint:** Minimal code (~60-80 lines), maximum reliability

## Architecture

### Core Approach

Wrap OCaml's battle-tested `Pprintast` module rather than implementing from scratch. This provides:

- Correct handling of all OCaml syntax edge cases
- Minimal maintenance burden
- Immediate compatibility with language evolution
- Type-safe interface matching Fmt conventions

### Type System

```ocaml
type 'a t = 'a Fmt.t = Format.formatter -> 'a -> unit
```

Both Fmt and Pprintast use `Format.formatter`, enabling direct aliasing without adaptation layers.

## Implementation

### Direct Aliases

Most functions are simple aliases to `Pprintast`:

```ocaml
let expression = Pprintast.expression
let pattern = Pprintast.pattern
let core_type = Pprintast.core_type
let signature = Pprintast.signature
let structure = Pprintast.structure
let module_expr = Pprintast.module_expr
let module_type = Pprintast.module_type
let class_expr = Pprintast.class_expr
let class_type = Pprintast.class_type
let class_field = Pprintast.class_field
let class_type_field = Pprintast.class_type_field
let structure_item = Pprintast.structure_item
let signature_item = Pprintast.signature_item
let binding = Pprintast.value_binding
let payload = Pprintast.payload
let toplevel_phrase = Pprintast.toplevel_phrase
let top_phrase = toplevel_phrase  (* Alias for compatibility *)
```

### String Conversion Helpers

Use `Fmt.str` (alias for `Format.asprintf`) to capture output:

```ocaml
let string_of_expression expr = Fmt.str "%a" expression expr
let string_of_structure struc = Fmt.str "%a" structure struc
```

### Special Cases

#### Longident Formatting

Check if `Pprintast.longident` is exposed. If not, implement using `Longident.flatten`:

```ocaml
let longident fmt lid =
  Fmt.list ~sep:(Fmt.any ".") Fmt.string fmt (Longident.flatten lid)
```

#### Constructor Formatting

Constructors use the same formatting as longidents:

```ocaml
let constr = longident
```

#### Type Variable Formatting

Per the interface specification, `tyvar` must:
- Pass through `"_"` unchanged (no-op)
- Escape OCaml keywords by prefixing with `\#`
- Handle special treatment for single quote in second position

Implementation:

```ocaml
let tyvar_of_name name =
  if name = "_" then name
  else
    (* Check if name is an OCaml keyword and escape *)
    (* Handle quote character in second position *)
    (* Return transformed name *)
    ...

let tyvar fmt name = Fmt.string fmt (tyvar_of_name name)
```

Export both `tyvar_of_name : string -> string` and `tyvar : string t` per the interface.

## Testing Strategy

1. **Integration test:** Use the formatter in backend code to print converted AST nodes
2. **Syntax validation:** Pipe generated output through `ocamlformat` or compile with `ocaml -c`
3. **Smoke test:** Convert a small SML snippet, verify OCaml output is valid

Example test:
```ocaml
let test_simple_expression () =
  let expr = (* construct simple Parsetree.expression *) in
  let output = Format.string_of_expression expr in
  (* Verify output is valid OCaml *)
  assert (String.length output > 0)
```

## Dependencies

- **ppxlib:** Provides `Parsetree` types
- **fmt:** Provides combinator interface
- **compiler-libs:** Provides `Pprintast` module (already in OCaml stdlib)

## Estimated Complexity

- **Lines of code:** ~60-80 lines
- **Implementation time:** ~30 minutes
- **Risk level:** Low (wrapping existing, stable code)

## Alternatives Considered

### Build from Scratch with Fmt

Implement a complete pretty-printer using Fmt combinators following the pattern in the reference `notes.mli_`.

**Rejected because:**
- ~1000+ lines of code
- High complexity with many edge cases
- Maintenance burden as OCaml syntax evolves
- No benefit over using the standard library

### Hybrid Approach

Use `Pprintast` as fallback but implement custom Fmt-based formatters for common constructs.

**Rejected because:**
- Unnecessary complexity
- Inconsistent output formatting
- Still requires maintaining custom code
- No clear benefit for code generation use case

## Future Enhancements

If custom formatting control is needed later:
- Add configuration options for indentation style
- Implement syntax highlighting via Fmt's styled output
- Add compact vs. pretty formatting modes

These can be layered on top of the current design without major refactoring.
