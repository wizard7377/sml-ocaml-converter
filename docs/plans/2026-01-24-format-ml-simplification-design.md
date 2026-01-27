# Format.ml Simplification Design

**Date:** 2026-01-24
**Goal:** Simplify `lib/common/format.ml` by removing complex precedence logic and adding parentheses liberally when ambiguous

## Current State

- ~1800 lines of OCaml Parsetree pretty-printer
- Complex context tracking system (`ctxt` record with `pipe`, `semi`, `ifthenelse` flags)
- Multiple precedence levels (`expression`, `expression1`, `expression2`, etc.)
- Intricate logic to minimize parentheses in output

## Problems

- Hard to understand and maintain
- Complex precedence rules make it difficult to predict output
- Context tracking adds cognitive overhead
- Over-engineered for a converter tool (not a production formatter)

## Solution: Liberal Parenthesization

### Core Strategy

**Remove all context tracking:**
- Eliminate `ctxt` parameter from all functions
- Remove `reset_ctxt`, `under_pipe`, `under_semi`, `under_ifthenelse`, etc.
- No more precedence-aware formatting

**Simple parenthesization rules:**
- **Atomic expressions** (variables, constants, constructors): no parens
- **Application-like** (function calls, constructor apps): parens if nested
- **Operators** (infix, prefix): always parenthesize
- **Control flow** (if/match/let): always parenthesize when nested
- **Simple structures** (tuples, lists, records in isolation): no parens
- **When in doubt**: add parentheses

**Keep unchanged:**
- Utility functions: `list`, `option`, `paren`, `protect_ident`, `longident`
- Name mangling and identifier protection logic
- Overall mutual recursion structure
- Format library usage and pretty-printing combinators

## Implementation Plan

### 1. Add Helper Functions (30 lines)

```ocaml
(* Returns true if expression needs parens when nested *)
let needs_parens_expr = function
  | { pexp_desc = Pexp_ident _ | Pexp_constant _
    | Pexp_construct (_, None) | Pexp_tuple _
    | Pexp_record _ | Pexp_array _ } -> false
  | _ -> true

let needs_parens_pat = function
  | { ppat_desc = Ppat_var _ | Ppat_any | Ppat_constant _
    | Ppat_construct (_, None) } -> false
  | _ -> true

let needs_parens_type = function
  | { ptyp_desc = Ptyp_var _ | Ptyp_constr (_, []) } -> false
  | _ -> true
```

### 2. Function Signature Changes

**Before:**
```ocaml
and expression ctxt f x = ...
and core_type ctxt f x = ...
and pattern ctxt f x = ...
```

**After:**
```ocaml
and expression f x = ...
and core_type f x = ...
and pattern f x = ...
```

All ~45 mutually recursive functions lose the `ctxt` parameter.

### 3. Remove Context Infrastructure

Delete:
- `type ctxt = { pipe : bool; semi : bool; ifthenelse : bool }`
- `reset_ctxt`, `under_pipe`, `under_semi`, `under_ifthenelse`
- `reset_semi`, `reset_ifthenelse`, `reset_pipe`

### 4. Simplify Expression/Pattern/Type Functions

Remove intermediate precedence functions:
- `expression1`, `expression2` → just `expression`
- `core_type1` → just `core_type`
- `pattern1`, `simple_pattern` → just `pattern`

Replace complex branching with simple checks:
```ocaml
let print_nested_expr f e =
  if needs_parens_expr e then
    paren true expression f e
  else
    expression f e
```

### 5. Specific AST Construct Handling

**Expressions:**
- Simple (ident, constant): no parens
- Application: `f x` → parens if args need them
- Infix: always parens `(x + y)`
- Let/match/if: always parens when nested
- Function: always parens `(fun x -> x)`

**Patterns:**
- Variables, wildcards, constants: no parens
- Constructor with args: parens if args need them
- Or patterns: always parens `(A | B)`
- Cons: always parens `(x :: xs)`

**Types:**
- Type vars, simple constructors: no parens
- Arrow: parens when nested `(int -> int) -> int`
- Tuple: always parens `(int * string)`
- Application: parens if arg needs them `(int * int) list`

## Expected Outcomes

**Code size:** ~1800 lines → ~600 lines (66% reduction)

**Output change:** More parentheses, but semantically identical
- Before: `(fun x -> x + 1) 5`
- After: `((fun x -> (x + 1)) 5)`

**Maintainability:** Much simpler to understand and modify

**Correctness:** Easier to verify - parentheses ensure no ambiguity

## Testing Strategy

Run existing Alcotest suite:
- Backend conversion tests should still pass
- Output will be more parenthesized but semantically equivalent
- Manual inspection of a few sample outputs to verify readability

## Trade-offs

**Pros:**
- Dramatic simplification (~1200 lines removed)
- Predictable, obvious behavior
- Easier to maintain and extend
- No ambiguity in output

**Cons:**
- More parentheses in output (acceptable for converter tool)
- Slightly less "pretty" output (but still correct and readable)

## Migration Notes

This is a pure refactoring - no API changes to the module interface. The `PrintAst.structure` function signature remains the same. All consumers of this module continue to work without modification.
