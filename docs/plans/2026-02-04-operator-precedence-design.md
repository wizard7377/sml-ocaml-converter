# Operator Precedence Implementation Design

**Date**: 2026-02-04
**Status**: Design Complete - Ready for Implementation

## Overview

Implement correct SML operator precedence handling using a two-phase approach: flat sequence parsing followed by backend precedence resolution.

## Problem Statement

Currently, the parser has a single precedence level `%left INFIX_APP` for all infix operators, meaning expressions like `1 + 2 * 3` parse incorrectly. SML requires operators to follow this precedence table:

```
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before
```

Additionally, multi-argument function application (juxtaposition) must be left-associative: `f x y` parses as `(f x) y`.

## Approach: Hybrid with Flat Sequences

**Selected Strategy**: Hybrid approach with:
- Static precedence for built-in SML operators
- Flat sequence representation in AST
- Backend-based precedence resolution
- Custom operators use default precedence (future enhancement)

### Phase 1: Parser - Flat Sequences

The parser produces flat sequences in `ExpApp` nodes:
- `f x y + z * w` → `ExpApp [f; x; y; +; z; *; w]`
- `f (x + y) z` → `ExpApp [f; ExpApp[x; +; y]; z]`

### Phase 2: Backend - Precedence Resolution

The backend restructures sequences based on operator precedence:
- Scans for operators using heuristics (symbolic characters)
- Finds lowest-precedence operator (respecting associativity)
- Recursively splits and rebuilds expression tree
- Converts to OCaml with correct precedence

## Architecture

### 1. AST Changes (ast_core.ml)

**Remove**:
```ocaml
| InfixApp of expression node * idx node * expression node
```

**Modify**:
```ocaml
| ExpApp of expression node list
  (* Flat juxtaposition sequences.
     Can contain mixed values, functions, and operators.
     Examples:
       - [f; x; y] → function application f x y
       - [x; +; y] → infix operator application
       - [f; x; +; y; *; z] → mixed: (f x) + (y * z)
  *)
```

**Keep Unchanged** (special control flow constructs):
- `AndExp` - for `andalso` (short-circuit semantics)
- `OrExp` - for `orelse` (short-circuit semantics)
- `HandleExp` - for `handle` (exception handling)
- `IfExp`, `WhileExp`, `CaseExp`, `FnExp`

**Pattern Changes**:
```ocaml
| PatApp of pat node list  (* Was: pat node * pat node *)
```

Similarly, `PatInfix` is removed. Patterns like `x :: y :: z` become `PatApp [x; ::; y; ::; z]`.

### 2. Parser Changes (parser.mly)

**Remove operator precedence declarations**:
```ocaml
(* DELETE: *)
%left INFIX_APP
%right CONS
%nonassoc EQUAL
%right STAR
```

**Keep control flow precedence**:
```ocaml
(* KEEP: *)
%right ANDALSO
%right ORELSE
%right HANDLE
%nonassoc ELSE
%nonassoc DO
```

**New expression grammar**:
```ocaml
expression:
  | exp_item_seq {
      match $1 with
      | [single] -> single.value
      | items -> ExpApp items
    }
  | expression "andalso" expression { AndExp ($1, $2) }
  | expression "orelse" expression { OrExp ($1, $2) }
  | "if" expression "then" expression "else" expression { IfExp ... }
  | "while" expression "do" expression { WhileExp ... }
  | "case" expression "of" match_clause { CaseExp ... }
  | "fn" match_clause { FnExp ... }
  | expression "handle" match_clause { HandleExp ... }
  | "raise" expression { RaiseExp ... }
  | expression ":" typ { TypedExp ... }
  ;

exp_item_seq:
  | atomic_exp exp_item_seq { bp $1 :: $2 }
  | SYMBOL_IDENT exp_item_seq { bp (ExpIdx (IdxIdx $1)) :: $2 }
  | CONS exp_item_seq { bp (ExpIdx (IdxIdx "::")) :: $2 }
  | EQUAL exp_item_seq { bp (ExpIdx (IdxIdx "=")) :: $2 }
  | atomic_exp { [bp $1] }
  | SYMBOL_IDENT { [bp (ExpIdx (IdxIdx $1))] }
  | CONS { [bp (ExpIdx (IdxIdx "::"))] }
  | EQUAL { [bp (ExpIdx (IdxIdx "="))] }
  ;
```

### 3. Backend Precedence Resolver (precedence_resolver.ml)

**New module** to restructure flat sequences.

**Operator Precedence Table**:
```ocaml
type assoc = Left | Right

let precedence_table = [
  (7, Left,  ["*"; "/"; "div"; "mod"]);
  (6, Left,  ["+"; "-"; "^"]);
  (5, Right, ["::"; "@"]);
  (4, Left,  ["="; "<>"; ">"; ">="; "<"; "<="]);
  (3, Left,  [":="; "o"]);
  (0, Left,  ["before"]);
]

let get_precedence op_name =
  (* Lookup in table, return (precedence, assoc) *)
  (* Default: None (treat as value, not operator) *)
```

**Operator Detection** (heuristic-based):
```ocaml
let is_operator_char c =
  match c with
  | '!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':'
  | '<' | '=' | '>' | '?' | '@' | '\\' | '~' | '`' | '^' | '|' | '*' -> true
  | _ -> false

let is_operator name =
  String.length name > 0 && is_operator_char name.[0]
```

**Core Algorithm**:
```ocaml
type resolved_exp =
  | ResolvedApp of expression * expression list
  | ResolvedInfix of expression * idx * expression
  | ResolvedSingle of expression

let rec resolve_precedence (items : expression node list) : resolved_exp =
  match items with
  | [] -> error "empty sequence"
  | [single] -> ResolvedSingle single.value
  | items ->
      (* 1. Find all operators with their positions *)
      let operators = find_operators items in

      match operators with
      | [] ->
          (* No operators - pure application: f x y z *)
          build_left_assoc_app items
      | ops ->
          (* 2. Find lowest precedence operator *)
          let split_pos = find_split_position ops in

          (* 3. Split at that position *)
          let left = take split_pos items in
          let op = items.(split_pos) in
          let right = drop (split_pos + 1) items in

          (* 4. Recursively resolve left and right *)
          let left_exp = resolve_precedence left in
          let right_exp = resolve_precedence right in

          (* 5. Build infix application *)
          ResolvedInfix (left_exp, extract_op op, right_exp)
```

**Split Position Algorithm**:
```ocaml
let find_split_position operators =
  (* Find operator with lowest precedence *)
  let lowest_prec = min_precedence operators in

  (* Among same precedence, use associativity: *)
  (* - Left-assoc: pick RIGHTMOST *)
  (* - Right-assoc: pick LEFTMOST *)
  match assoc_of lowest_prec with
  | Left -> rightmost_with_prec operators lowest_prec
  | Right -> leftmost_with_prec operators lowest_prec
```

**Left-Associative Application**:
```ocaml
let build_left_assoc_app items =
  (* f x y z becomes ((f x) y) z *)
  match items with
  | [] -> error "empty"
  | [single] -> ResolvedSingle single.value
  | f :: args ->
      let rec fold_apps acc = function
        | [] -> acc
        | arg :: rest ->
            fold_apps (ResolvedApp (acc, [arg])) rest
      in
      fold_apps (ResolvedSingle f.value) args
```

### 4. Backend Integration (backend.ml)

**Modified `process_exp`**:
```ocaml
| ExpApp exp_list ->
    (* 1. Resolve precedence to get structured expression *)
    let resolved = Precedence_resolver.resolve exp_list in

    (* 2. Process the resolved expression *)
    process_resolved_exp resolved

and process_resolved_exp = function
  | ResolvedSingle e ->
      process_exp e

  | ResolvedApp (f, args) ->
      (* Left-associative function application *)
      let f_exp = process_resolved_exp f in
      let arg_exps = List.map (fun a -> process_exp a) args in
      List.fold_left (fun acc arg ->
        Builder.pexp_apply acc [(Nolabel, arg)]
      ) f_exp arg_exps

  | ResolvedInfix (left, op, right) ->
      (* Binary operator - check constructor registry *)
      let op_name = idx_to_string op in
      let left_exp = process_resolved_exp left in
      let right_exp = process_resolved_exp right in

      match Constructor_registry.lookup op_name with
      | Some ctor ->
          (* Constructor like :: *)
          let name_longident = build_longident [ctor.ocaml_name] in
          let tuple = Builder.pexp_tuple [left_exp; right_exp] in
          Builder.pexp_construct (ghost name_longident) (Some tuple)
      | None ->
          (* Regular operator like + *)
          let op_longident = build_longident (idx_to_name op) in
          Builder.pexp_apply
            (Builder.pexp_ident (ghost op_longident))
            [(Nolabel, left_exp); (Nolabel, right_exp)]
```

## Edge Cases

1. **Single item**: `ExpApp [x]` → `ResolvedSingle x`
2. **Empty list**: Should not occur from parser, but error gracefully
3. **Nested sequences**: `f (x + y) z` → `ExpApp [f; ExpApp[x; +; y]; z]`
   - Recursively resolve inner sequences first
4. **Operators at boundaries**: `+ x` or `x +` → treated as values in sequence, not operators
5. **Multiple operators, same precedence**: `1 + 2 - 3 + 4`
   - Left-assoc: split at rightmost → `((1 + 2) - 3) + 4`
6. **Constructor vs operator**: `x :: xs`
   - Check constructor registry
   - Build `pexp_construct` for constructors
7. **Parenthesized operators**: `op +` or `(+)` → treat as value (WithOp marker)

## Testing Strategy

### Unit Tests (test/unit_tests/test_precedence_resolver.ml)

```ocaml
let test_basic_precedence () =
  (* 1 + 2 * 3 should parse as 1 + (2 * 3) *)

let test_left_assoc () =
  (* 1 - 2 - 3 should parse as (1 - 2) - 3 *)

let test_right_assoc () =
  (* x :: y :: z should parse as x :: (y :: z) *)

let test_same_level () =
  (* 1 + 2 - 3 should parse as (1 + 2) - 3 *)

let test_multi_arg_app () =
  (* f x y should parse as (f x) y *)

let test_mixed () =
  (* f x + g y z should parse as (f x) + ((g y) z) *)
```

### File-Based Integration Tests (test/file_tests/)

- `precedence_basic.sml` - arithmetic operators
- `precedence_comparison.sml` - relational operators
- `precedence_cons.sml` - list construction with `::`
- `precedence_mixed.sml` - mixed operators and applications
- `precedence_parens.sml` - parenthesized expressions

### Validation Approach

1. **Unit tests** - test precedence resolver in isolation
2. **File tests** - end-to-end SML → OCaml conversion
3. **Manual verification** - compare against SML/NJ or MLton parses for complex cases

## Known Limitations

Acceptable for initial implementation:
- Custom operators declared with `infix N name` use default precedence (not level N)
- Fixity declarations are parsed but don't affect parsing precedence
- Future enhancement: track fixity environment for custom operators

## Implementation Plan

1. **AST Changes**
   - Remove `InfixApp` and `PatInfix` variants
   - Modify `ExpApp` and `PatApp` to handle lists
   - Update AST documentation

2. **Create Precedence Resolver Module**
   - Define precedence table
   - Implement operator detection
   - Implement split algorithm
   - Handle edge cases

3. **Update Parser**
   - Remove operator precedence declarations
   - Implement `exp_item_seq` rule
   - Keep special construct handling

4. **Update Backend**
   - Remove old `InfixApp` handling
   - Integrate precedence resolver
   - Update `process_exp` to use resolved expressions

5. **Testing**
   - Write unit tests for resolver
   - Create file-based integration tests
   - Validate against known SML expressions

6. **Documentation**
   - Update CLAUDE.md with new architecture
   - Document precedence table
   - Add examples to odoc comments

## Success Criteria

- `1 + 2 * 3` converts to `1 + (2 * 3)` in OCaml
- `f x y + g z` converts to `(f x y) + (g z)` in OCaml
- `x :: y :: z` converts correctly with right-associativity
- All existing tests continue to pass
- New precedence tests pass
