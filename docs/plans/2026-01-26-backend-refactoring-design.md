# Backend Refactoring Design

**Date:** 2026-01-26
**Status:** Approved
**Goal:** Refactor backend.ml (2112 lines, 66 functions) into modular, maintainable components using recursive modules and functors

## Objectives

1. **Reduce code duplication** - Extract common patterns into shared utilities
2. **Increase modularity** - Split large files into focused, single-purpose modules
3. **Hide implementation details** - Use proper module signatures to encapsulate internals
4. **Use library utilities** - Replace custom implementations with ppxlib/stdlib functions
5. **Remove unused code** - Clean up dead code identified in compiler warnings
6. **Improve readability** - Break large functions into small (≤15 lines), well-named helpers

## Current State

### Problems
- `backend.ml`: 2112 lines with ~66 functions
- `process_exp`: ~500 lines - monolithic expression converter
- Scattered state: 3+ `ref` variables throughout
- Duplicated name processing across 3 files
- Unused code generating compiler warnings

### Architecture
```
SML AST → Backend.Make(Context)(Config) → OCaml Parsetree
```

## Proposed Architecture

### Core Design: Recursive Modules + Functors

Each AST node type gets its own converter module implementing:

```ocaml
module type CONVERTER = sig
  type input
  type output
  type env
  type state

  val convert : env -> state -> input -> output
end
```

**Shared Environment & State:**

```ocaml
(* backend_common.ml *)
type env = {
  config : Common.options ref;
  labeller : Process_label.process_label;
  name_processor : Process_names.process_names;
  lexbuf : Lexing.lexbuf;
}

type state = {
  mutable temp_counter : int;
  mutable current_path : string list;
  mutable trace_depth : int;
}

let create_state () = {
  temp_counter = 0;
  current_path = [];
  trace_depth = 0;
}

let get_temp state =
  let t = state.temp_counter in
  state.temp_counter <- t + 1;
  t

let fresh_var state prefix =
  Printf.sprintf "%s__%d" prefix (get_temp state)
```

### File Structure

```
lib/source/backend/
├── backend_sig.ml              (* Main signature - public API *)
├── backend_common.ml           (* Shared types: env, state, utilities *)
├── backend_names.ml            (* Consolidated name processing *)
├── backend_const.ml            (* Constant conversion *)
├── backend_type.ml             (* Type conversion *)
├── backend_pat.ml              (* Pattern conversion *)
├── backend_expr.ml             (* Expression conversion *)
├── backend_expr_helpers.ml     (* Complex expression sub-conversions *)
├── backend_dec.ml              (* Declaration conversion *)
├── backend_str.ml              (* Structure/module conversion *)
└── backend.ml                  (* Recursive module tie-together *)
```

### Converter Modules

#### Independent Converters (No Mutual Recursion)

**1. backend_const.ml** - Constants
```ocaml
module Make(Env : sig type env type state end) = struct
  type input = Ast.constant Ast.node
  type output = Parsetree.constant
  type env = Env.env
  type state = Env.state

  let convert _env _state const =
    match const.value with
    | ConstInt i -> Pconst_integer (string_of_int i, None)
    | ConstString s -> Pconst_string (s, Location.none, None)
    | (* ... *)
end
```

**2. backend_type.ml** - Types
```ocaml
module Make(Env : sig type env type state end) = struct
  type input = Ast.typ Ast.node
  type output = Parsetree.core_type
  type env = Env.env
  type state = Env.state

  let convert env state typ = (* ... *)

  (* Helper functions (each ≤15 lines) *)
  let convert_type_var env state tv = (* ... *)
  let convert_tuple env state types = (* ... *)
  let convert_arrow env state t1 t2 = (* ... *)
  let convert_record env state rows = (* ... *)
end
```

#### Mutually Recursive Converters

**3. backend_pat.ml** - Patterns
```ocaml
module Make
  (Env : sig type env type state end)
  (Type : CONVERTER with type env = Env.env
                     and type state = Env.state
                     and type input = Ast.typ Ast.node
                     and type output = Parsetree.core_type)
  (Const : CONVERTER with type env = Env.env
                      and type state = Env.state
                      and type input = Ast.constant Ast.node
                      and type output = Parsetree.constant)
= struct
  type input = Ast.pat Ast.node
  type output = Parsetree.pattern
  type env = Env.env
  type state = Env.state

  let convert env state pat =
    match pat.value with
    | PatConst c -> Builder.ppat_constant (Const.convert env state c)
    | PatVar id -> convert_var env state id
    | PatTuple ps -> convert_tuple env state ps
    | (* ... *)

  and convert_var env state id = (* ≤15 lines *)
  and convert_tuple env state pats = (* ≤15 lines *)
  and convert_record env state rows = (* ≤15 lines *)
  and convert_as env state name pat = (* ≤15 lines *)
end
```

**4. backend_expr.ml** - Expressions (Breaking down 500 lines)
```ocaml
module Make
  (Env : sig type env type state end)
  (Pat : CONVERTER with type input = Ast.pat Ast.node
                    and type output = Parsetree.pattern
                    and (* ... *))
  (Const : CONVERTER with (* ... *))
  (Type : CONVERTER with (* ... *))
  (Dec : CONVERTER with type input = Ast.declaration
                    and type output = Parsetree.structure_item list
                    and (* ... *))
= struct
  type input = Ast.expression Ast.node
  type output = Parsetree.expression
  type env = Env.env
  type state = Env.state

  (* Main dispatcher - routes to focused helpers *)
  let convert env state expr =
    match expr.value with
    | ConstExp c -> convert_const env state c
    | ExpIdx id -> convert_ident env state id
    | ExpApp (e1, e2) -> convert_app env state e1 e2
    | InfixExp (e1, op, e2) -> convert_infix env state e1 op e2
    | CaseExp (e, m) -> convert_case env state e m
    | LetExp (decs, exps) -> convert_let env state decs exps
    | RecordExp rows -> convert_record env state rows
    | SeqExp exps -> convert_seq env state exps
    | TupleExp exps -> convert_tuple env state exps
    | ListExp exps -> convert_list env state exps
    | IfExp (e1, e2, e3) -> convert_if env state e1 e2 e3
    | (* ... each case has its own function *)

  (* Each helper is focused and small *)
  and convert_const env state c =
    Builder.pexp_constant (Const.convert env state c)

  and convert_ident env state id =
    let longid = process_identifier env id in
    Builder.pexp_ident (Location.mknoloc longid)

  and convert_app env state e1 e2 =
    let e1' = convert env state e1 in
    let e2' = convert env state e2 in
    Builder.pexp_apply e1' [(Nolabel, e2')]

  and convert_infix env state e1 op e2 =
    let op_exp = convert_ident env state op in
    let e1' = convert env state e1 in
    let e2' = convert env state e2 in
    Builder.pexp_apply op_exp [
      (Nolabel, e1');
      (Nolabel, e2')
    ]

  and convert_case env state scrutinee matching =
    let e = convert env state scrutinee in
    let cases = convert_matching env state matching in
    Builder.pexp_match e cases

  and convert_matching env state matching =
    (* Convert match cases - may need Dec for nested let *)
    (* ... *)

  (* Complex sub-conversions can go to _helpers.ml if needed *)
end
```

**5. backend_dec.ml** - Declarations
```ocaml
module Make
  (Env : sig type env type state end)
  (Expr : CONVERTER with (* ... *))
  (Pat : CONVERTER with (* ... *))
  (Type : CONVERTER with (* ... *))
= struct
  type input = Ast.declaration
  type output = Parsetree.structure_item list
  type env = Env.env
  type state = Env.state

  let convert env state dec =
    match dec with
    | ValDec (tvars, vb) -> convert_val env state tvars vb
    | FunDec (tvars, fb) -> convert_fun env state tvars fb
    | DatDec (db, tb) -> convert_datatype env state db tb
    | ExnDec eb -> convert_exception env state eb
    | (* ... *)

  and convert_val env state tvars vb = (* ... *)
  and convert_fun env state tvars fb = (* ... *)
end
```

**6. backend_str.ml** - Structures
```ocaml
module Make
  (Env : sig type env type state end)
  (Dec : CONVERTER with (* ... *))
= struct
  type input = Ast.structure
  type output = Parsetree.structure_item list
  type env = Env.env
  type state = Env.state

  let convert env state structure = (* ... *)
end
```

### Recursive Module Tie-Together

**backend.ml** - Constructs the mutually recursive modules:

```ocaml
module Make (Context : CONTEXT) (Config : CONFIG) = struct
  (* Create shared environment *)
  let env = {
    config = Config.config;
    labeller = new Process_label.process_label Config.config Context.lexbuf;
    name_processor = new Process_names.process_names Config.config store;
    lexbuf = Context.lexbuf;
  }

  (* Create initial state *)
  let state = Backend_common.create_state ()

  (* Build recursive module graph *)
  module rec Const : CONVERTER
    with type input = Ast.constant Ast.node
    and type output = Parsetree.constant
    and type env = Backend_common.env
    and type state = Backend_common.state
  = Backend_const.Make(struct
      type env = Backend_common.env
      type state = Backend_common.state
    end)

  and Type : CONVERTER
    with type input = Ast.typ Ast.node
    and type output = Parsetree.core_type
    and (* ... *)
  = Backend_type.Make(struct
      type env = Backend_common.env
      type state = Backend_common.state
    end)

  and Pat : CONVERTER
    with type input = Ast.pat Ast.node
    and type output = Parsetree.pattern
    and (* ... *)
  = Backend_pat.Make(struct
      type env = Backend_common.env
      type state = Backend_common.state
    end)(Type)(Const)

  and Expr : CONVERTER
    with type input = Ast.expression Ast.node
    and type output = Parsetree.expression
    and (* ... *)
  = Backend_expr.Make(struct
      type env = Backend_common.env
      type state = Backend_common.state
    end)(Pat)(Const)(Type)(Dec)

  and Dec : CONVERTER
    with type input = Ast.declaration
    and type output = Parsetree.structure_item list
    and (* ... *)
  = Backend_dec.Make(struct
      type env = Backend_common.env
      type state = Backend_common.state
    end)(Expr)(Pat)(Type)

  and Str : CONVERTER
    with type input = Ast.structure
    and type output = Parsetree.structure_item list
    and (* ... *)
  = Backend_str.Make(struct
      type env = Backend_common.env
      type state = Backend_common.state
    end)(Dec)

  (* Export public API *)
  let process_exp = Expr.convert env state
  let process_pat = Pat.convert env state
  let process_type = Type.convert env state
  let process_const = Const.convert env state
  let process_dec = Dec.convert env state
  let process_prog structure = Str.convert env state structure
end
```

### Dependency Graph

```
Const, Type (independent - no external converter dependencies)
    ↓
   Pat (needs Type, Const)
    ↓
   Expr (needs Pat, Const, Type, Dec)
    ↓ ↑ (mutual recursion)
   Dec (needs Expr, Pat, Type)
    ↓
   Str (needs Dec)
```

## Utility Consolidation

### Name Processing (backend_names.ml)

Consolidate from:
- `capital_utils.ml` - Basic string operations
- `process_names.ml` - Context-aware name transformation
- `name_processor.ml` - (check if duplicate of above)

Into single module exposing:
```ocaml
val process_name : context -> string list -> Ppxlib.Longident.t * bool
val process_lowercase : string -> string
val process_uppercase : string -> string
val is_constructor : string -> bool
val is_variable : string -> bool
```

### Use Ppxlib Utilities

Replace custom implementations:

| Custom | Replace With |
|--------|-------------|
| Manual longident construction | `Ppxlib.Longident.parse` where possible |
| Custom location helpers | `Ppxlib.Location.none`, `Location.mknoloc` |
| Custom attribute builders | `Ppxlib.Ast_builder.Default.attribute` |
| Keyword checking | `Ppxlib.Keyword.is_keyword` (already used) |

### Remove Unused Code

From compiler warnings:
- `quoter` variable - unused Ppxlib quoter
- `get_signature_attr` - unused function
- `get_structure_attr` - unused function
- `process_uppercase` in backend.ml - after consolidation
- `process_anotate` - verify usage, likely dead
- `process_typ_refine` - verify usage, likely dead

## Implementation Strategy

### Phase 1: Infrastructure Setup (1-2 days)

**Goal:** Set up skeleton without breaking existing code

1. Create `backend_common.ml` with types:
   ```ocaml
   type env = { ... }
   type state = { ... }
   val create_state : unit -> state
   val get_temp : state -> int
   val fresh_var : state -> string -> string
   ```

2. Extract `backend_const.ml`:
   - Independent converter, no dependencies
   - Copy constant conversion from backend.ml
   - Test in isolation

3. Extract `backend_type.ml`:
   - Independent converter
   - Copy type conversion from backend.ml
   - Break down into helpers: `convert_type_var`, `convert_tuple`, etc.
   - Test in isolation

4. Set up recursive module skeleton in `backend.ml`:
   - Create module structure
   - Wire up Const and Type
   - Verify compilation

**Commit:** "refactor: set up backend converter infrastructure"

### Phase 2: Pattern Converter (1 day)

1. Create `backend_pat.ml`:
   - Implement functor taking Type, Const
   - Extract pattern conversion logic
   - Break into helpers: `convert_var`, `convert_tuple`, `convert_as`, etc.

2. Wire into backend.ml recursive modules

3. Run tests: `make test`

**Commit:** "refactor: extract pattern converter to backend_pat.ml"

### Phase 3: Expression Converter (2 days)

**This is the big one - 500 lines to break down**

1. Create `backend_expr.ml`:
   - Main `convert` function dispatches by case
   - Each case gets dedicated helper (≤15 lines each):
     - `convert_const`
     - `convert_ident`
     - `convert_app`
     - `convert_infix`
     - `convert_case`
     - `convert_let`
     - `convert_record`
     - `convert_seq`
     - `convert_tuple`
     - `convert_list`
     - `convert_if`
     - (20+ more cases...)

2. Create `backend_expr_helpers.ml` for complex sub-conversions:
   - Application type resolution
   - Tuple handling
   - Record field processing
   - Selector functions

3. Wire into backend.ml (now depends on Dec - mutual recursion!)

4. Run tests continuously

**Commit:** "refactor: extract expression converter to backend_expr.ml"

### Phase 4: Declaration Converter (1 day)

1. Create `backend_dec.ml`:
   - Functor takes Expr, Pat, Type
   - Extract declaration conversion
   - Helpers: `convert_val`, `convert_fun`, `convert_datatype`, etc.

2. Complete mutual recursion between Expr and Dec in backend.ml

3. Run tests

**Commit:** "refactor: extract declaration converter to backend_dec.ml"

### Phase 5: Structure Converter (0.5 day)

1. Create `backend_str.ml`:
   - Takes Dec
   - Extract structure conversion

2. Wire into backend.ml

3. Run tests

**Commit:** "refactor: extract structure converter to backend_str.ml"

### Phase 6: Consolidate Utilities (1 day)

1. Create `backend_names.ml`:
   - Merge capital_utils.ml functionality
   - Merge process_names.ml functionality
   - Expose clean API

2. Remove duplicated code

3. Run tests

**Commit:** "refactor: consolidate name processing utilities"

### Phase 7: Use Ppxlib Utilities (0.5 day)

1. Replace custom longident construction where possible

2. Use ppxlib attribute helpers

3. Remove custom implementations that duplicate ppxlib

**Commit:** "refactor: use ppxlib utilities instead of custom implementations"

### Phase 8: Remove Unused Code (0.5 day)

1. Remove functions identified in compiler warnings:
   - `quoter`
   - `get_signature_attr`
   - `get_structure_attr`
   - Others as identified

2. Clean up exports in signatures

3. Verify tests still pass

**Commit:** "refactor: remove unused code"

### Phase 9: Cleanup & Documentation (0.5 day)

1. Remove `backend_old.ml` if kept as backup

2. Update CLAUDE.md with new structure

3. Generate and review odoc documentation

4. Final test run

**Commit:** "docs: update documentation for refactored backend"

## Migration Safety

### Safety Measures
- Keep original backend.ml as backend_old.ml during refactoring
- Run `make test` after every phase
- Each converter extraction is a separate commit (can revert)
- Use git worktree for isolated development
- Verify zero behavioral changes (compare outputs)

### Testing Strategy
- Existing Alcotest suite must pass after each phase
- Add converter-specific tests as extracted
- Compare outputs of old vs new backend on example files
- Run full Twelf conversion suite as integration test

### Rollback Plan
If a phase breaks tests:
1. Revert the phase commit
2. Analyze the failure
3. Fix the converter module
4. Retry the phase

## Success Criteria

### Quantitative
- ✅ No file over 300 lines (currently backend.ml is 2112)
- ✅ No function over 15 lines without justification comment
- ✅ All existing tests pass
- ✅ Zero behavioral changes (output identical on test suite)

### Qualitative
- ✅ Clear separation of concerns (each converter in own file)
- ✅ Mutual dependencies explicit via functor parameters
- ✅ Implementation details hidden (only convert function exported)
- ✅ Code duplication eliminated
- ✅ Unused code removed
- ✅ Small, readable functions with clear names

## Estimated Timeline

- **Phase 1-2:** 2 days (infrastructure + patterns)
- **Phase 3:** 2 days (expressions - the big one)
- **Phase 4-5:** 1.5 days (declarations + structures)
- **Phase 6-8:** 2 days (utilities + cleanup)
- **Phase 9:** 0.5 days (documentation)

**Total:** ~8 days with buffer for testing and issues

## Future Enhancements

Once refactoring is complete, easier to add:
- **Better error messages** - Each converter can have focused error handling
- **Incremental conversion** - State tracking per converter
- **Parallel conversion** - Independent converters can run in parallel
- **Plugin architecture** - New converters can be added modularly
- **Better testing** - Each converter can be tested in isolation

## References

- [OCaml Manual: Recursive Modules](https://ocaml.org/manual/recursivemodules.html)
- [Real World OCaml: Functors](https://dev.realworldocaml.org/functors.html)
- [Ppxlib Documentation](https://ocaml-ppx.github.io/ppxlib/)
- CLAUDE.md - Current architecture documentation
