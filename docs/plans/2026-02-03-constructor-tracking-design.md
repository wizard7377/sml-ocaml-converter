# Constructor Tracking and Name Transformation System

**Date:** 2026-02-03
**Status:** Approved
**Scope:** Cross-module constructor tracking with strict OCaml capitalization enforcement

## Overview

Implement a constructor registry system that tracks constructor definitions across modules and enforces OCaml's strict capitalization conventions during SML to OCaml conversion. This addresses SML's lenient capitalization (lowercase constructors allowed) versus OCaml's strict requirements (constructors must be uppercase).

## Problem Statement

SML allows constructors to be lowercase (`datatype foo = bar | baz`), while OCaml requires constructors to be uppercase. The converter must:

1. Track all constructor definitions with their module paths
2. Distinguish constructor references from variable bindings in patterns/expressions
3. Handle `open` statements that bring constructors into scope
4. Transform names consistently: lowercase constructors → uppercase, variables → lowercase
5. Support multi-module compilation with dependency resolution

## Architecture

### 1. Constructor Registry

**Location:** `lib/source/context/constructor_registry.ml`

**Data Structure:**
```ocaml
type constructor_info = {
  name: string;              (* Original SML name: "SOME", "true", "Cons" *)
  path: string list;         (* Module path: ["List", "Cons"] or ["Cons"] *)
  ocaml_name: string;        (* Transformed name: "Some", "true", "Cons_" *)
}

type constructor_registry = {
  (* Fully qualified: ["M", "Cons"] -> info *)
  qualified: (string list, constructor_info) Hashtbl.t;

  (* Opened/unqualified: "Cons" -> info list (multiple opens possible) *)
  unqualified: (string, constructor_info list) Hashtbl.t;
}
```

**Operations:**
- `add_constructor : registry -> path:string list -> name:string -> unit`
- `open_module : registry -> module_path:string list -> unit`
- `lookup : registry -> path:string list option -> name:string -> constructor_info option`

**Integration:**
- Registry stored in `Context.t` (modify `lib/source/context/context.ml`)
- Passed through Backend functor as part of Context
- Pre-populated with Basis Library constructors on initialization

### 2. Constructor Collection (Backend Phase)

**Modified:** `lib/source/backend/backend.ml`

**Collection Points:**

1. **Datatype declarations** (`process_dat_bind`):
   ```ocaml
   (* SML: datatype 'a option = NONE | SOME of 'a *)
   (* Register: "NONE" at path !current_path @ ["NONE"] *)
   (* Register: "SOME" at path !current_path @ ["SOME"] *)
   ```

2. **Exception declarations** (`process_exn_bind`):
   ```ocaml
   (* SML: exception NotFound of string *)
   (* Register: "NotFound" at path !current_path @ ["NotFound"] *)
   ```

3. **Module nesting tracking:**
   ```ocaml
   (* SML: structure List = struct ... end *)
   (* Push "List" onto current_path *)
   (* Process nested declarations *)
   (* Pop from current_path *)
   ```

**Implementation:**
- Add `current_path : string list ref` to track module nesting
- Add `register_constructor : string -> unit` helper
- Call during constructor-defining declarations

### 3. Name Resolution and Transformation

**Modified:** `lib/source/backend/backend.ml` (process_pat, process_exp)

**Pattern Processing:**
```ocaml
let process_pat ?(is_arg=false) ?(is_head=false) (pat : Ast.pat node) =
  match unbox_node pat with
  | PatIdx idx ->
      let name_parts = idx_to_name idx in
      let qual_path = extract_module_path name_parts in
      let name = extract_final_name name_parts in

      match lookup_constructor ~path:qual_path name with
      | Some ctor_info when is_head ->
          (* Constructor pattern - use transformed name *)
          build_constructor_pattern ctor_info.ocaml_name
      | Some ctor_info when not is_head ->
          (* Head of constructor application *)
          build_constructor_pattern ctor_info.ocaml_name
      | None ->
          (* Variable binding - force lowercase *)
          let var_name = transform_to_lowercase name in
          build_variable_pattern var_name
```

**Expression Processing:**
```ocaml
let process_exp (exp : Ast.expression node) =
  match unbox_node exp with
  | ExpIdx idx ->
      let name_parts = idx_to_name idx in

      match lookup_constructor name_parts with
      | Some ctor_info ->
          (* Constructor reference - use transformed name *)
          build_constructor_expr ctor_info.ocaml_name
      | None ->
          (* Value reference - apply value name rules *)
          build_value_expr name_parts
```

**Transformation Rules:**

*Location:* `lib/source/backend/constructor_transform.ml`

- **Lowercase constructors:** `a` → `A_`, `some` → `Some_`
- **Uppercase with trailing `_`:** `B_` → `B__`
- **Already valid OCaml:** `Foo` → `Foo` (unchanged)
- **Variables:** Always lowercase - `SOME` → `some_`, `Foo` → `foo_`

### 4. Open Statement Handling

**Modified:** `lib/source/backend/backend.ml` (process_dec)

```ocaml
let process_dec (dec : Ast.declaration) =
  match dec with
  | OpenDec module_path ->
      (* Bring all constructors from module M into unqualified scope *)
      let path = longident_to_path module_path in
      Context.Constructor_registry.open_module registry ~module_path:path;

      (* Generate: open M *)
      [%stri open [%m module_ident_of_path path]]
```

**Registry behavior:**
- Query `qualified` table for entries matching `module_path.*`
- Add each to `unqualified` table (duplicates from multiple opens allowed)
- Lookup checks `unqualified` first, then `qualified`

### 5. Dependency Resolution (Hybrid Strategy)

**Priority order:**

1. **Pre-populated Basis Library** (in Context initialization):
   - `option`: NONE, SOME
   - `bool`: true, false
   - `list`: nil, ::
   - Standard exceptions

2. **Manifest files** (`.shibboleth-constructors`):
   - JSON format: `{"constructors": [{"name": "SOME", "path": ["SOME"], "ocaml_name": "Some"}]}`
   - Written after successful compilation
   - Searched in: same directory, configured include paths
   - *New module:* `lib/source/context/constructor_manifest.ml`

3. **Source parsing** (fallback):
   - Locate `.sml` file for module
   - Parse and extract constructor definitions
   - Cache in registry

4. **Best-effort failure:**
   - Log warning if module not found
   - Continue compilation (may produce incorrect casing)

## File Structure

### New Modules

```
lib/source/context/
  constructor_registry.ml      # Registry data structure and operations
  constructor_registry.mli
  constructor_manifest.ml      # Manifest file I/O
  constructor_manifest.mli

lib/source/backend/
  constructor_transform.ml     # Name transformation utilities
  constructor_transform.mli
```

### Modified Modules

```
lib/source/context/
  context.ml                   # Add constructor_registry field
  context.mli                  # Update type signature

lib/source/backend/
  backend.ml                   # Collection + resolution integration
  backend_sig.ml              # Update signatures if needed
```

## Implementation Steps

1. **Create constructor_registry.ml**
   - Implement registry data structure
   - Implement add/open/lookup operations
   - Add unit tests

2. **Create constructor_manifest.ml**
   - JSON serialization/deserialization
   - File I/O with search paths
   - Add unit tests

3. **Create constructor_transform.ml**
   - Name transformation functions
   - Add unit tests for all transformation rules

4. **Modify context.ml**
   - Add `constructor_registry` field
   - Initialize with Basis Library constructors

5. **Modify backend.ml - Collection phase**
   - Add `current_path` tracking
   - Add `register_constructor` helper
   - Integrate into `process_dat_bind`, `process_exn_bind`
   - Track module nesting

6. **Modify backend.ml - Resolution phase**
   - Update `process_pat` with constructor lookup
   - Update `process_exp` with constructor lookup
   - Apply transformation rules

7. **Modify backend.ml - Open handling**
   - Update `process_dec` for `OpenDec`
   - Integrate with registry

8. **Manifest generation**
   - After `process_prog`, write manifest file if output specified
   - Include all registered constructors

9. **Integration testing**
   - Single-module tests with mixed-case constructors
   - Multi-module tests with `open` statements
   - Dependency resolution tests (manifest, source, fallback)

## Testing Strategy

**Unit tests:**
- Registry operations (add, lookup, open_module)
- Name transformations (all rule cases)
- Manifest serialization round-trips

**Integration tests:**
- Pattern matching with lowercase constructors
- Expression references to constructors
- Module nesting (`structure M = struct ... end`)
- `open` bringing constructors into scope
- Multi-file compilation with dependencies
- Basis Library constructor resolution

**Test cases:**
```sml
(* Test 1: Lowercase constructors *)
datatype foo = bar | baz of int
val x = bar
val y = case bar of bar => 1 | baz n => n

(* Test 2: Mixed with variables *)
datatype result = ok | error
val ok = 5  (* Variable shadowing constructor *)

(* Test 3: Qualified access *)
structure M = struct
  datatype t = foo | bar
end
val x = M.foo

(* Test 4: Open statement *)
open M
val y = foo  (* Now refers to M.foo *)
```

## Design Decisions

**Q: Scope of tracking?**
A: Full cross-module tracking with manifest files and dependency resolution.

**Q: Storage mechanism?**
A: Context-based in-memory registry, persisted via manifest files.

**Q: Dependency discovery?**
A: Hybrid - pre-populated Basis, manifests, source parsing, best-effort failure.

**Q: Already-uppercase constructors?**
A: Leave unchanged if they match OCaml conventions (minimal transformation).

**Q: Variable shadowing constructors?**
A: Variable bindings always lowercase, regardless of constructor existence.

## Future Enhancements

- Incremental compilation support (cache registry between runs)
- Warning when variables shadow constructors
- Configuration option to control transformation aggressiveness
- Integration with build systems for automatic manifest generation
