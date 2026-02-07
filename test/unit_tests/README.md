# Backend Unit Tests

This directory contains Alcotest-based unit tests for the SML to OCaml converter backend.

## Overview

The tests comprehensively validate the SML to OCaml conversion pipeline, including both unit tests for individual backend functions and integration tests against real-world SML codebases.

## Test Suites

### 1. Backend Unit Tests
Fine-grained tests for individual backend conversion functions.

### 2. Twelf Integration Tests (NEW)
End-to-end integration tests validating the converter against the Twelf project codebase.

**Test Count:** 468 tests (one per source file: *.sml, *.fun, *.sig)
**Location:** `examples/input/twelf/src/`

Each test:
1. Parses the SML source file
2. Converts to OCaml AST
3. Pretty-prints the OCaml code
4. Validates the generated OCaml is syntactically valid

**Current Status:**
- ✅ **217 tests passing** (46.4%) - Files that convert to valid OCaml
- ❌ **251 tests failing** (53.6%) - Files exposing converter limitations

Failing tests help identify missing features and bugs in the converter.

## Test Coverage

### Type Processing Tests
- **Type variables**: Basic type variables (`'a`) and equality type variables (`''eq`)
- **Type constructors**: Simple types (`int`), parameterized types (`'a list`), multi-parameter types
- **Parenthesized types**: Ensuring proper handling of parentheses
- **Function types**: Both simple (`'a -> 'b`) and nested function types
- **Tuple types**: Two-element and multi-element tuples
- **Record types**: Single and multiple field records

### Object Field Processing Tests
- **Single fields**: Testing individual object field conversions
- **Multiple fields**: Testing chained object field conversions

### Expression Processing Tests
- **Identifiers**: Simple identifier expressions
- **Function application**: Testing function application expressions
- **Infix operators**: Testing infix operator applications

### Complex Type Tests
- **Complex function types**: Functions with tuple arguments
- **List type transformations**: Polymorphic and monomorphic list types
- **Higher-order functions**: Functions that take and return functions

## Running the Tests

### Run all tests (recommended):
```bash
make test
```

### Run only Twelf integration tests:
```bash
make test_twelf
```

### Run all unit tests (including Twelf):
```bash
dune exec test/unit_tests/unit_tests.exe
```

### Run specific test suites:
```bash
# Run only type processing tests
dune exec test/unit_tests/unit_tests.exe -- test "Type Processing"

# Run only expression tests
dune exec test/unit_tests/unit_tests.exe -- test "Expression Processing"

# Run only Twelf integration tests
dune exec test/unit_tests/unit_tests.exe -- test "Twelf Integration"
```

### Run with verbose output:
```bash
dune exec test/unit_tests/unit_tests.exe -- test "Twelf Integration" --verbose
```

### Run specific file from Twelf tests:
```bash
dune exec test/unit_tests/unit_tests.exe -- test "Twelf Integration" --match "modes/modetable.fun"
```

### Build the tests without running:
```bash
dune build test/unit_tests/unit_tests.exe
```

## Test Structure

Each test follows this pattern:
1. Create an SML AST node (e.g., `TypVar (IdxVar "a")`)
2. Process it through the backend function (e.g., `process_type_value input`)
3. Verify the output matches expectations using Alcotest assertions

## Dependencies

- **alcotest**: The testing framework
- **ppxlib**: For OCaml AST manipulation and pretty-printing
- **backend**: The module under test
- **ast**: SML AST definitions
- **helpers**: Helper utilities for AST construction

## Adding New Twelf Integration Tests

The Twelf integration tests are **automatically discovered** at runtime! Simply:
1. Place your `.sml`, `.fun`, or `.sig` file in `examples/input/twelf/src/` (or any subdirectory)
2. Run the tests - the new file will be automatically tested
3. No code changes needed!

## Adding New Unit Tests

To add new backend unit tests:

1. Create a test function:
```ocaml
let test_new_feature () =
  let input = (* create SML AST *) in
  let result = process_function input in
  check testable_type "description"
    expected_value
    result
```

2. Add it to the appropriate test suite list:
```ocaml
let my_test_suite = [
  "test description", `Quick, test_new_feature;
  (* ... *)
]
```

3. Register the suite in the main runner:
```ocaml
let () =
  run "Backend" [
    "My Test Suite", my_test_suite;
    (* ... *)
  ]
```

## Current Status

Some tests may fail as the backend implementation is still in development. Failed tests indicate:
- Unimplemented features (functions with `assert false`)
- Implementation bugs that need fixing
- Edge cases that need handling

Use test failures to guide implementation work!
