# Operator Precedence Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement correct SML operator precedence using flat sequence parsing and backend precedence resolution.

**Architecture:** Two-phase approach - parser creates flat `ExpApp` sequences, backend resolves precedence based on operator table with left/right associativity rules.

**Tech Stack:** OCaml, Menhir (parser generator), Ppxlib (OCaml AST), Alcotest (testing)

---

## Task 1: Create Precedence Resolver Module

**Files:**
- Create: `lib/source/backend/precedence_resolver.ml`
- Create: `lib/source/backend/precedence_resolver.mli`
- Modify: `lib/source/backend/dune` (add new module)

**Step 1: Create interface file**

Create `lib/source/backend/precedence_resolver.mli`:

```ocaml
(** Precedence-based expression resolver for flat sequences.

    Takes flat [ExpApp] sequences from the parser and restructures them
    based on SML operator precedence rules. *)

open Ast

(** Resolved expression structure after precedence analysis *)
type resolved_exp =
  | ResolvedSingle of expression
      (** Single expression with no operators *)
  | ResolvedApp of expression * expression list
      (** Left-associative function application: f x y → (f x) y *)
  | ResolvedInfix of expression * idx * expression
      (** Binary operator application: x + y *)

(** Resolve precedence in a flat expression sequence.

    @param items Flat list from [ExpApp]
    @return Structured expression respecting operator precedence
    @raise Failure if sequence is empty *)
val resolve : expression node list -> resolved_exp
```

**Step 2: Create implementation skeleton**

Create `lib/source/backend/precedence_resolver.ml`:

```ocaml
open Ast
open Ast_node

type resolved_exp =
  | ResolvedSingle of expression
  | ResolvedApp of expression * expression list
  | ResolvedInfix of expression * idx * expression

let resolve (items : expression node list) : resolved_exp =
  match items with
  | [] -> failwith "precedence_resolver: empty sequence"
  | [single] -> ResolvedSingle (unbox_node single)
  | _ -> failwith "precedence_resolver: not implemented yet"
```

**Step 3: Update dune file**

Modify `lib/source/backend/dune` to add the new module (add to modules list):

```ocaml
(library
 (name backend)
 (modules
  backend
  backend_sig
  backend_context
  constructor_transform
  name_processor
  precedence_resolver  ; ADD THIS LINE
  process_label
  process_names)
 ...)
```

**Step 4: Build to verify no errors**

Run: `dune build lib/source/backend/precedence_resolver.mli`

Expected: Success (or existing build errors unrelated to new files)

**Step 5: Commit**

```bash
git add lib/source/backend/precedence_resolver.ml lib/source/backend/precedence_resolver.mli lib/source/backend/dune
git commit -m "feat: add precedence resolver module skeleton

Create new module for resolving operator precedence in flat expression
sequences. Initial implementation handles only single-item sequences.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 2: Implement Operator Detection

**Files:**
- Modify: `lib/source/backend/precedence_resolver.ml`
- Modify: `lib/source/backend/precedence_resolver.mli`

**Step 1: Add operator detection to interface**

Modify `lib/source/backend/precedence_resolver.mli`, add:

```ocaml
(** Check if a string represents an operator based on its first character.

    Operators start with symbolic characters: ! % & $ # + - / : < = > ? @ \ ~ ` ^ | *

    @param name Identifier name to check
    @return true if name is an operator *)
val is_operator : string -> bool
```

**Step 2: Implement operator detection**

Modify `lib/source/backend/precedence_resolver.ml`, add helper functions after type definition:

```ocaml
(** Check if character is an operator symbol *)
let is_operator_char c =
  match c with
  | '!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':'
  | '<' | '=' | '>' | '?' | '@' | '\\' | '~' | '`' | '^' | '|' | '*' -> true
  | _ -> false

(** Check if string is an operator based on first character *)
let is_operator name =
  String.length name > 0 && is_operator_char name.[0]

(** Extract identifier string from idx *)
let idx_to_string idx =
  match idx with
  | IdxIdx name -> unbox_node name
  | IdxLong _ -> ""  (* Long identifiers are not operators *)
  | IdxVar _ -> ""   (* Type variables are not operators *)
  | IdxNum _ -> ""   (* Numbers are not operators *)
  | IdxLab _ -> ""   (* Labels are not operators *)

(** Find operators in expression sequence with their positions *)
let find_operators (items : expression node list) : (int * string) list =
  let rec aux pos acc = function
    | [] -> List.rev acc
    | item :: rest ->
        let op_opt = match (unbox_node item) with
          | ExpIdx idx_node ->
              let idx = unbox_node idx_node in
              let name = idx_to_string idx in
              if is_operator name then Some name else None
          | _ -> None
        in
        let new_acc = match op_opt with
          | Some op -> (pos, op) :: acc
          | None -> acc
        in
        aux (pos + 1) new_acc rest
  in
  aux 0 [] items
```

**Step 3: Build to verify**

Run: `dune build lib/source/backend/precedence_resolver.ml`

Expected: Success

**Step 4: Commit**

```bash
git add lib/source/backend/precedence_resolver.ml lib/source/backend/precedence_resolver.mli
git commit -m "feat: add operator detection to precedence resolver

Implement is_operator to detect symbolic operators based on first
character. Add find_operators to scan expression sequences and
locate operator positions.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 3: Implement Precedence Table

**Files:**
- Modify: `lib/source/backend/precedence_resolver.ml`

**Step 1: Add precedence table type and data**

Modify `lib/source/backend/precedence_resolver.ml`, add after operator detection:

```ocaml
(** Operator associativity *)
type assoc = Left | Right

(** SML operator precedence table.

    Format: (precedence_level, associativity, operator_list)
    Higher precedence = tighter binding *)
let precedence_table = [
  (7, Left,  ["*"; "/"; "div"; "mod"]);
  (6, Left,  ["+"; "-"; "^"]);
  (5, Right, ["::"; "@"]);
  (4, Left,  ["="; "<>"; ">"; ">="; "<"; "<="]);
  (3, Left,  [":="; "o"]);
  (0, Left,  ["before"]);
]

(** Get precedence and associativity for an operator.

    @param op_name Operator string (e.g., "+", "::")
    @return Some (precedence, associativity) or None if not in table *)
let get_precedence op_name =
  let rec search = function
    | [] -> None
    | (prec, assoc, ops) :: rest ->
        if List.mem op_name ops then Some (prec, assoc)
        else search rest
  in
  search precedence_table

(** Compare two operators by precedence.

    @return -1 if op1 < op2, 0 if equal, 1 if op1 > op2
    Lower precedence = weaker binding (should split here first) *)
let compare_precedence op1 op2 =
  match get_precedence op1, get_precedence op2 with
  | None, None -> 0
  | None, Some _ -> -1  (* Unknown ops have lowest precedence *)
  | Some _, None -> 1
  | Some (p1, _), Some (p2, _) -> compare p1 p2
```

**Step 2: Build to verify**

Run: `dune build lib/source/backend/precedence_resolver.ml`

Expected: Success

**Step 3: Commit**

```bash
git add lib/source/backend/precedence_resolver.ml
git commit -m "feat: add SML operator precedence table

Implement precedence_table with all standard SML operators at their
correct precedence levels (0-7) with left/right associativity.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 4: Implement Split Position Algorithm

**Files:**
- Modify: `lib/source/backend/precedence_resolver.ml`

**Step 1: Add split position finder**

Modify `lib/source/backend/precedence_resolver.ml`, add:

```ocaml
(** Find the position to split the expression sequence.

    For left-associative operators at same precedence, pick rightmost.
    For right-associative operators at same precedence, pick leftmost.

    @param operators List of (position, operator_name) pairs
    @return Position to split at *)
let find_split_position operators =
  match operators with
  | [] -> failwith "find_split_position: no operators"
  | [(pos, _)] -> pos
  | (first_pos, first_op) :: rest ->
      (* Find minimum precedence *)
      let min_prec = List.fold_left (fun acc (_, op) ->
        min acc (match get_precedence op with
          | Some (p, _) -> p
          | None -> -1  (* Treat unknown as very low precedence *)
        )
      ) (match get_precedence first_op with Some (p, _) -> p | None -> -1) rest in

      (* Get associativity of minimum precedence level *)
      let assoc_of_min = List.fold_left (fun acc (_, op) ->
        match get_precedence op with
        | Some (p, a) when p = min_prec -> Some a
        | _ -> acc
      ) None ((first_pos, first_op) :: rest) in

      (* Find appropriate position based on associativity *)
      match assoc_of_min with
      | Some Left ->
          (* Left-assoc: pick rightmost at min precedence *)
          List.fold_left (fun (best_pos, best_op) (pos, op) ->
            match get_precedence op with
            | Some (p, _) when p = min_prec && pos >= best_pos -> (pos, op)
            | _ -> (best_pos, best_op)
          ) (first_pos, first_op) rest |> fst
      | Some Right ->
          (* Right-assoc: pick leftmost at min precedence *)
          List.fold_left (fun (best_pos, best_op) (pos, op) ->
            match get_precedence op with
            | Some (p, _) when p = min_prec && (best_pos < 0 || pos < best_pos) -> (pos, op)
            | _ -> (best_pos, best_op)
          ) (-1, first_op) ((first_pos, first_op) :: rest) |> fst
      | None ->
          (* Default to leftmost *)
          first_pos
```

**Step 2: Build to verify**

Run: `dune build lib/source/backend/precedence_resolver.ml`

Expected: Success

**Step 3: Commit**

```bash
git add lib/source/backend/precedence_resolver.ml
git commit -m "feat: add split position algorithm

Implement find_split_position to find the lowest-precedence operator
respecting associativity: leftmost for right-assoc, rightmost for
left-assoc.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 5: Implement Left-Associative Application Builder

**Files:**
- Modify: `lib/source/backend/precedence_resolver.ml`

**Step 1: Add application builder**

Modify `lib/source/backend/precedence_resolver.ml`, add:

```ocaml
(** Build left-associative application from sequence with no operators.

    f x y z becomes (f x) applied to y applied to z.

    @param items Expression sequence with no operators
    @return ResolvedApp structure *)
let build_left_assoc_app items =
  match items with
  | [] -> failwith "build_left_assoc_app: empty"
  | [single] -> ResolvedSingle (unbox_node single)
  | first :: rest ->
      (* Build nested applications left-to-right *)
      let rec fold_apps acc = function
        | [] -> acc
        | arg :: remaining ->
            fold_apps (ResolvedApp (acc, [unbox_node arg])) remaining
      in
      fold_apps (ResolvedSingle (unbox_node first)) rest
```

**Step 2: Build to verify**

Run: `dune build lib/source/backend/precedence_resolver.ml`

Expected: Success

**Step 3: Commit**

```bash
git add lib/source/backend/precedence_resolver.ml
git commit -m "feat: add left-associative application builder

Implement build_left_assoc_app to handle function application
sequences like 'f x y' → '(f x) y'.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 6: Implement Main Resolution Algorithm

**Files:**
- Modify: `lib/source/backend/precedence_resolver.ml`

**Step 1: Add list helper functions**

Modify `lib/source/backend/precedence_resolver.ml`, add:

```ocaml
(** Take first n elements from list *)
let take n lst =
  let rec aux acc n = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: xs -> aux (x :: acc) (n - 1) xs
  in
  aux [] n lst

(** Drop first n elements from list *)
let drop n lst =
  let rec aux n = function
    | [] -> []
    | lst when n <= 0 -> lst
    | _ :: xs -> aux (n - 1) xs
  in
  aux n lst
```

**Step 2: Complete resolve function**

Modify `lib/source/backend/precedence_resolver.ml`, replace the `resolve` function:

```ocaml
let rec resolve (items : expression node list) : resolved_exp =
  match items with
  | [] -> failwith "precedence_resolver: empty sequence"
  | [single] -> ResolvedSingle (unbox_node single)
  | items ->
      (* Find all operators *)
      let operators = find_operators items in

      match operators with
      | [] ->
          (* No operators - pure function application *)
          build_left_assoc_app items
      | ops ->
          (* Split at lowest-precedence operator *)
          let split_pos = find_split_position ops in

          (* Extract operator at split position *)
          let op_expr = List.nth items split_pos in
          let op_idx = match unbox_node op_expr with
            | ExpIdx idx_node -> unbox_node idx_node
            | _ -> failwith "precedence_resolver: expected operator"
          in

          (* Split into left and right *)
          let left = take split_pos items in
          let right = drop (split_pos + 1) items in

          (* Recursively resolve both sides *)
          let left_resolved = match left with
            | [] -> failwith "precedence_resolver: empty left operand"
            | _ -> resolve left
          in
          let right_resolved = match right with
            | [] -> failwith "precedence_resolver: empty right operand"
            | _ -> resolve right
          in

          (* Convert resolved expressions back to expression type *)
          let left_exp = match left_resolved with
            | ResolvedSingle e -> e
            | ResolvedApp (f, args) ->
                (* Rebuild as ExpApp for backend *)
                ExpApp (box_node f :: List.map box_node args)
            | ResolvedInfix (e1, op, e2) ->
                (* Rebuild as ExpApp with operator *)
                ExpApp [box_node e1; box_node (ExpIdx (box_node op)); box_node e2]
          in
          let right_exp = match right_resolved with
            | ResolvedSingle e -> e
            | ResolvedApp (f, args) ->
                ExpApp (box_node f :: List.map box_node args)
            | ResolvedInfix (e1, op, e2) ->
                ExpApp [box_node e1; box_node (ExpIdx (box_node op)); box_node e2]
          in

          ResolvedInfix (left_exp, op_idx, right_exp)
```

**Step 3: Build to verify**

Run: `dune build lib/source/backend/precedence_resolver.ml`

Expected: Success (warnings about unused functions are OK)

**Step 4: Commit**

```bash
git add lib/source/backend/precedence_resolver.ml
git commit -m "feat: implement complete precedence resolution algorithm

Complete the resolve function to handle operator precedence by:
1. Finding all operators in sequence
2. Splitting at lowest-precedence operator
3. Recursively resolving left and right sides
4. Building ResolvedInfix structure

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 7: Remove InfixApp from AST

**Files:**
- Modify: `lib/source/ast/ast_core.ml`

**Step 1: Comment out InfixApp variant**

Modify `lib/source/ast/ast_core.ml`, find the `InfixApp` variant (around line 641) and comment it out:

```ocaml
(* REMOVED: Replaced by flat ExpApp sequences
  | InfixApp of expression node * idx node * expression node
*)
```

**Step 2: Build to see all locations that need updating**

Run: `dune build 2>&1 | grep "InfixApp" | head -20`

Expected: Multiple compile errors showing where InfixApp is used

**Step 3: Commit**

```bash
git add lib/source/ast/ast_core.ml
git commit -m "refactor: remove InfixApp from AST

Comment out InfixApp variant in preparation for flat sequence
approach. This will cause compile errors that will be fixed in
subsequent commits.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 8: Update Parser to Generate Flat Sequences

**Files:**
- Modify: `lib/source/frontend/parser.mly`

**Step 1: Remove operator precedence declarations**

Modify `lib/source/frontend/parser.mly`, in the precedence section (lines 119-146), comment out:

```ocaml
(* REMOVED: Operators now handled in backend
%left INFIX_APP
%right CONS
%nonassoc EQUAL
%right STAR
*)
```

**Step 2: Update expression rule to build sequences**

Modify `lib/source/frontend/parser.mly`, replace the `expression` rule (starting around line 420):

```ocaml
expression:
  | exp_item_seq {
      match $1 with
      | [single] -> single.value
      | items -> ExpApp items
    }
  | expression "andalso" expression { AndExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | expression "orelse" expression { OrExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | "if" expression "then" expression "else" expression { IfExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3), bp $5 $startpos($5) $endpos($5)) }
  | "while" expression "do" expression { WhileExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | "case" expression "of" match_clause { CaseExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | "fn" match_clause { FnExp (bp $1 $startpos($1) $endpos($1)) }
  | expression "handle" match_clause { HandleExp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | "raise" expression { RaiseExp (bp $1 $startpos($1) $endpos($1)) }
  | expression ":" typ { TypedExp (bp $1 $startpos $endpos, bp $3 $startpos($3) $endpos($3)) }
  ;

exp_item_seq:
  | atomic_exp exp_item_seq { bp $1 $startpos($1) $endpos($1) :: $2 }
  | SYMBOL_IDENT exp_item_seq { bp (ExpIdx (bp (ident_to_idx $1) $startpos($1) $endpos($1))) $startpos($1) $endpos($1) :: $2 }
  | CONS exp_item_seq { bp (ExpIdx (bp (IdxIdx (b "::")) $startpos $endpos)) $startpos $endpos :: $2 }
  | EQUAL exp_item_seq { bp (ExpIdx (bp (IdxIdx (b "=")) $startpos $endpos)) $startpos $endpos :: $2 }
  | atomic_exp { [bp $1 $startpos($1) $endpos($1)] }
  | SYMBOL_IDENT { [bp (ExpIdx (bp (ident_to_idx $1) $startpos($1) $endpos($1))) $startpos($1) $endpos($1)] }
  | CONS { [bp (ExpIdx (bp (IdxIdx (b "::")) $startpos $endpos)) $startpos $endpos] }
  | EQUAL { [bp (ExpIdx (bp (IdxIdx (b "=")) $startpos $endpos)) $startpos $endpos] }
  ;
```

**Step 3: Remove old infix expression rules**

Delete or comment out these lines from the original `expression` rule:
- `| expression SYMBOL_IDENT expression %prec INFIX_APP { InfixApp ... }`
- `| expression CONS expression { InfixApp ... }`
- `| head=SYMBOL_IDENT arg=expression %prec PREFIX_APP { ExpApp ... }`

**Step 4: Build parser to verify**

Run: `dune build lib/source/frontend/parser.mly`

Expected: Parser compiles successfully

**Step 5: Commit**

```bash
git add lib/source/frontend/parser.mly
git commit -m "refactor: update parser to generate flat sequences

Replace InfixApp generation with flat ExpApp sequences. Remove
operator precedence declarations and implement exp_item_seq rule
to collect mixed expressions and operators.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 9: Update Backend to Use Precedence Resolver

**Files:**
- Modify: `lib/source/backend/backend.ml`

**Step 1: Add precedence resolver integration**

Modify `lib/source/backend/backend.ml`, find the `process_exp` function's `ExpApp` case (around line 352), and replace:

```ocaml
    | ExpApp exp_list ->
        (* Resolve precedence in flat sequence *)
        let resolved = Precedence_resolver.resolve exp_list in
        process_resolved_exp resolved
```

**Step 2: Add helper to process resolved expressions**

Add new function after `process_exp`:

```ocaml
  and process_resolved_exp (resolved : Precedence_resolver.resolved_exp) : Parsetree.expression =
    match resolved with
    | Precedence_resolver.ResolvedSingle e ->
        process_exp (box_node e)

    | Precedence_resolver.ResolvedApp (f, args) ->
        (* Left-associative application: (f x) y *)
        let f_exp = process_exp (box_node f) in
        List.fold_left (fun acc arg ->
          let arg_exp = process_exp (box_node arg) in
          Builder.pexp_apply acc [(Nolabel, arg_exp)]
        ) f_exp args

    | Precedence_resolver.ResolvedInfix (left, op, right) ->
        (* Binary operator *)
        let op_name = idx_to_string op in
        let left_exp = process_exp (box_node left) in
        let right_exp = process_exp (box_node right) in

        (* Check constructor registry for operators like :: *)
        let lookup_result = Context.Constructor_registry.lookup
          Ctx.context.constructor_registry ~path:None op_name in

        (match lookup_result with
        | Some ctor_info ->
            (* Constructor (e.g., ::) *)
            let name_longident = build_longident [ctor_info.Context.Constructor_registry.ocaml_name] in
            let tuple = Builder.pexp_tuple [left_exp; right_exp] in
            Builder.pexp_construct (ghost name_longident) (Some tuple)
        | None ->
            (* Regular operator (e.g., +, -, *) *)
            let op_longident = build_longident (idx_to_name op) in
            Builder.pexp_apply
              (Builder.pexp_ident (ghost op_longident))
              [(Nolabel, left_exp); (Nolabel, right_exp)])
```

**Step 3: Remove old InfixApp case**

Find and delete the `| InfixApp (e1, op, e2) ->` case (around line 355-370)

**Step 4: Build to verify**

Run: `dune build lib/source/backend/backend.ml 2>&1 | head -30`

Expected: Compiles (may have warnings)

**Step 5: Commit**

```bash
git add lib/source/backend/backend.ml
git commit -m "refactor: integrate precedence resolver in backend

Update process_exp to use Precedence_resolver.resolve for ExpApp
sequences. Add process_resolved_exp to handle ResolvedSingle,
ResolvedApp, and ResolvedInfix cases. Remove old InfixApp handling.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 10: Update Pattern Handling

**Files:**
- Modify: `lib/source/ast/ast_core.ml`
- Modify: `lib/source/frontend/parser.mly`
- Modify: `lib/source/backend/backend.ml`

**Step 1: Remove PatInfix from AST**

Modify `lib/source/ast/ast_core.ml`, find `PatInfix` variant and comment out:

```ocaml
(* REMOVED: Replaced by flat PatApp sequences
  | PatInfix of pat node * idx node * pat node
*)
```

**Step 2: Update PatApp to use lists**

Modify `lib/source/ast/ast_core.ml`, find `PatApp` variant and update:

```ocaml
  | PatApp of pat node list
      (** Pattern application - constructor with arguments or flat sequence.

          Can represent both constructor applications and sequences that need
          precedence resolution similar to expressions. *)
```

**Step 3: Update parser pattern rules**

Modify `lib/source/frontend/parser.mly`, update `pat` rule (around line 512):

```ocaml
pat:
  | atomic_pat_seq1 {
      match $1 with
      | [p] -> p.value
      | items -> PatApp items
    }
  | pat BAR pat { PatOr (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | pat ":" typ { PatTyp (bp $1 $startpos($1) $endpos($1), bp $3 $startpos($3) $endpos($3)) }
  | pat "as" ident {
      match $1 with
      | PatIdx op -> PatAs (op, None, bp (PatIdx (bp $3 $startpos($3) $endpos($3))) $startpos($3) $endpos($3))
      | PatTyp (p, ty) ->
          (match p.value with
           | PatIdx op -> PatAs (op, Some ty, bp (PatIdx (bp $3 $startpos($3) $endpos($3))) $startpos($3) $endpos($3))
           | _ -> failwith "invalid layered pattern")
      | _ -> failwith "invalid layered pattern"
    }
  ;
```

Remove old pattern infix rules:
- `| pat SYMBOL_IDENT pat %prec INFIX_APP { PatInfix ... }`
- `| pat CONS pat { PatInfix ... }`
- `| SYMBOL_IDENT pat %prec PREFIX_APP { PatApp ... }`

**Step 4: Update backend pattern processing**

Modify `lib/source/backend/backend.ml`, find `process_pat` function's `PatApp` case and update to handle lists (this will be similar to expression handling but simpler for now - just treat as constructor application):

```ocaml
    | PatApp pat_list ->
        (* For now, treat as simple constructor application *)
        (* TODO: Add pattern precedence resolution if needed *)
        (match pat_list with
        | [] -> failwith "Empty pattern application"
        | [single] -> process_pat ~is_head:false single
        | ctor :: args ->
            (* First item is constructor, rest are arguments *)
            let ctor_pat = process_pat ~is_head:true ctor in
            let arg_pats = List.map (process_pat ~is_head:false) args in
            (* Build tuple of arguments *)
            let args_tuple = Builder.ppat_tuple arg_pats in
            (* Combine with constructor *)
            (match ctor_pat.ppat_desc with
            | Ppat_construct (lid, None) ->
                Builder.ppat_construct lid (Some args_tuple)
            | _ -> ctor_pat))
```

Remove old `PatInfix` case.

**Step 5: Build to verify**

Run: `dune build 2>&1 | grep -i "error" | head -10`

Expected: May have some errors in test files (will fix next)

**Step 6: Commit**

```bash
git add lib/source/ast/ast_core.ml lib/source/frontend/parser.mly lib/source/backend/backend.ml
git commit -m "refactor: update pattern handling for flat sequences

Remove PatInfix and update PatApp to use lists. Update parser to
generate flat pattern sequences. Update backend to handle pattern
lists (simple constructor application for now).

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 11: Fix Test Files

**Files:**
- Modify: `test/unit_tests/unit_tests_.ml`
- Modify: Other test files as needed

**Step 1: Update test file to use new ExpApp signature**

Modify `test/unit_tests/unit_tests_.ml`, find all `ExpApp` usages and update from tuple to list:

Change:
```ocaml
ExpApp (b (ExpIdx ...), b (ExpIdx ...))
```

To:
```ocaml
ExpApp [b (ExpIdx ...); b (ExpIdx ...)]
```

**Step 2: Build tests**

Run: `dune build test/unit_tests/unit_tests_.ml`

Expected: Success

**Step 3: Run tests to see what breaks**

Run: `dune test 2>&1 | head -50`

Expected: Some tests may fail due to AST changes

**Step 4: Commit**

```bash
git add test/unit_tests/unit_tests_.ml
git commit -m "test: fix unit tests for new ExpApp list signature

Update all ExpApp usages in tests to use list syntax instead of
tuple syntax.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 12: Write Precedence Resolver Tests

**Files:**
- Create: `test/unit_tests/test_precedence_resolver.ml`
- Modify: `test/unit_tests/dune`

**Step 1: Create test file**

Create `test/unit_tests/test_precedence_resolver.ml`:

```ocaml
open Ast
open Ast_node
open Precedence_resolver

(** Helper to create expression node *)
let exp e = box_node e

(** Helper to create int constant *)
let int_exp s = exp (ExpCon (box_node (ConInt (box_node s))))

(** Helper to create identifier *)
let id_exp s = exp (ExpIdx (box_node (IdxIdx (box_node s))))

(** Helper to create operator *)
let op_exp s = exp (ExpIdx (box_node (IdxIdx (box_node s))))

(** Test single expression *)
let test_single () =
  let input = [int_exp "42"] in
  let result = resolve input in
  match result with
  | ResolvedSingle (ExpCon _) -> ()
  | _ -> failwith "Expected ResolvedSingle"

(** Test simple application *)
let test_simple_app () =
  let input = [id_exp "f"; int_exp "1"; int_exp "2"] in
  let result = resolve input in
  match result with
  | ResolvedApp _ -> ()
  | _ -> failwith "Expected ResolvedApp"

(** Test basic infix: 1 + 2 *)
let test_basic_infix () =
  let input = [int_exp "1"; op_exp "+"; int_exp "2"] in
  let result = resolve input in
  match result with
  | ResolvedInfix (ExpCon _, IdxIdx _, ExpCon _) -> ()
  | _ -> failwith "Expected ResolvedInfix"

(** Test precedence: 1 + 2 * 3 should be 1 + (2 * 3) *)
let test_precedence () =
  let input = [
    int_exp "1";
    op_exp "+";
    int_exp "2";
    op_exp "*";
    int_exp "3"
  ] in
  let result = resolve input in
  match result with
  | ResolvedInfix (ExpCon _, IdxIdx op1, ExpApp _) ->
      if unbox_node op1 <> "+" then
        failwith "Expected + as outer operator"
  | _ -> failwith "Expected + as outer operator with ExpApp on right"

(** Test left associativity: 1 - 2 - 3 should be (1 - 2) - 3 *)
let test_left_assoc () =
  let input = [
    int_exp "1";
    op_exp "-";
    int_exp "2";
    op_exp "-";
    int_exp "3"
  ] in
  let result = resolve input in
  match result with
  | ResolvedInfix (ExpApp _, IdxIdx op, ExpCon _) ->
      if unbox_node op <> "-" then
        failwith "Expected rightmost - as split point"
  | _ -> failwith "Expected left-assoc split"

(** Test right associativity: 1 :: 2 :: [] should be 1 :: (2 :: []) *)
let test_right_assoc () =
  let input = [
    int_exp "1";
    op_exp "::";
    int_exp "2";
    op_exp "::";
    id_exp "[]"
  ] in
  let result = resolve input in
  match result with
  | ResolvedInfix (ExpCon _, IdxIdx op, ExpApp _) ->
      if unbox_node op <> "::" then
        failwith "Expected leftmost :: as split point"
  | _ -> failwith "Expected right-assoc split"

(** Run all tests *)
let () =
  test_single ();
  test_simple_app ();
  test_basic_infix ();
  test_precedence ();
  test_left_assoc ();
  test_right_assoc ();
  print_endline "All precedence resolver tests passed!"
```

**Step 2: Update dune file**

Modify `test/unit_tests/dune`:

```ocaml
(executable
 (name test_precedence_resolver)
 (modules test_precedence_resolver)
 (libraries ast backend))

(rule
 (alias runtest)
 (action (run ./test_precedence_resolver.exe)))
```

**Step 3: Run tests**

Run: `dune test`

Expected: Tests should pass

**Step 4: Commit**

```bash
git add test/unit_tests/test_precedence_resolver.ml test/unit_tests/dune
git commit -m "test: add precedence resolver unit tests

Add comprehensive tests for precedence resolver covering:
- Single expressions
- Simple application
- Basic infix
- Precedence rules (1 + 2 * 3)
- Left associativity (1 - 2 - 3)
- Right associativity (1 :: 2 :: [])

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 13: Create Integration Test Files

**Files:**
- Create: `test/file_tests/precedence_basic.sml`
- Create: `test/file_tests/precedence_basic.expected.ml`

**Step 1: Create basic precedence test**

Create `test/file_tests/precedence_basic.sml`:

```sml
(* Test basic operator precedence *)
val a = 1 + 2 * 3
val b = 1 * 2 + 3
val c = 1 + 2 + 3
val d = 1 * 2 * 3
```

**Step 2: Create expected output**

Create `test/file_tests/precedence_basic.expected.ml`:

```ocaml
let a = 1 + (2 * 3)
let b = (1 * 2) + 3
let c = (1 + 2) + 3
let d = (1 * 2) * 3
```

**Step 3: Run conversion**

Run: `dune exec shibboleth -- file test/file_tests/precedence_basic.sml`

Expected: Output matches expected (may need adjustment)

**Step 4: Commit**

```bash
git add test/file_tests/precedence_basic.sml test/file_tests/precedence_basic.expected.ml
git commit -m "test: add basic precedence integration test

Add file test for basic operator precedence covering:
- Multiplication before addition
- Left-associative operators at same level

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 14: Test Comparison Operators

**Files:**
- Create: `test/file_tests/precedence_comparison.sml`
- Create: `test/file_tests/precedence_comparison.expected.ml`

**Step 1: Create comparison test**

Create `test/file_tests/precedence_comparison.sml`:

```sml
(* Test comparison operator precedence *)
val a = 1 + 2 > 3
val b = x < y andalso y < z
val c = a = b orelse c <> d
```

**Step 2: Create expected output**

Create `test/file_tests/precedence_comparison.expected.ml`:

```ocaml
let a = (1 + 2) > 3
let b = (x < y) && (y < z)
let c = (a = b) || (c <> d)
```

**Step 3: Test and commit**

Run: `dune exec shibboleth -- file test/file_tests/precedence_comparison.sml`

```bash
git add test/file_tests/precedence_comparison.sml test/file_tests/precedence_comparison.expected.ml
git commit -m "test: add comparison operator precedence test

Test that comparison operators have lower precedence than
arithmetic, and interact correctly with andalso/orelse.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 15: Test List Construction

**Files:**
- Create: `test/file_tests/precedence_cons.sml`
- Create: `test/file_tests/precedence_cons.expected.ml`

**Step 1: Create cons test**

Create `test/file_tests/precedence_cons.sml`:

```sml
(* Test :: operator precedence and associativity *)
val a = 1 :: 2 :: 3 :: []
val b = x :: xs
val c = f x :: g y :: []
```

**Step 2: Create expected output**

Create `test/file_tests/precedence_cons.expected.ml`:

```ocaml
let a = 1 :: (2 :: (3 :: []))
let b = x :: xs
let c = (f x) :: ((g y) :: [])
```

**Step 3: Test and commit**

Run: `dune exec shibboleth -- file test/file_tests/precedence_cons.sml`

```bash
git add test/file_tests/precedence_cons.sml test/file_tests/precedence_cons.expected.ml
git commit -m "test: add list construction precedence test

Test that :: is right-associative and interacts correctly with
function application.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 16: Test Mixed Expressions

**Files:**
- Create: `test/file_tests/precedence_mixed.sml`
- Create: `test/file_tests/precedence_mixed.expected.ml`

**Step 1: Create mixed test**

Create `test/file_tests/precedence_mixed.sml`:

```sml
(* Test complex mixed expressions *)
val a = f x y + g z
val b = f (x + y) z
val c = (f x) + (g y)
val d = f x + y * z
```

**Step 2: Create expected output**

Create `test/file_tests/precedence_mixed.expected.ml`:

```ocaml
let a = ((f x) y) + (g z)
let b = (f (x + y)) z
let c = (f x) + (g y)
let d = ((f x) + (y * z))
```

**Step 3: Test and commit**

Run: `dune exec shibboleth -- file test/file_tests/precedence_mixed.sml`

```bash
git add test/file_tests/precedence_mixed.sml test/file_tests/precedence_mixed.expected.ml
git commit -m "test: add mixed expression precedence test

Test complex interactions between function application,
operators, and parentheses.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 17: Update Documentation

**Files:**
- Modify: `docs/plans/2026-02-04-operator-precedence-design.md`
- Modify: `CLAUDE.md`

**Step 1: Mark design as implemented**

Modify `docs/plans/2026-02-04-operator-precedence-design.md`, update status:

```markdown
**Date**: 2026-02-04
**Status**: ✅ Implemented

## Implementation Summary

Successfully implemented operator precedence using two-phase approach:
1. Parser generates flat sequences in ExpApp
2. Backend resolves precedence via Precedence_resolver module

All tests passing. See commit history for details.
```

**Step 2: Update CLAUDE.md**

Modify `CLAUDE.md`, add to Backend Architecture section:

```markdown
### Precedence Resolution

`lib/backend/precedence_resolver.ml` handles operator precedence for flat expression sequences:
- Scans for operators using heuristic detection (symbolic first character)
- Implements SML operator precedence table (levels 0-7)
- Respects left/right associativity
- Splits at lowest-precedence operator and recursively resolves
```

**Step 3: Commit**

```bash
git add docs/plans/2026-02-04-operator-precedence-design.md CLAUDE.md
git commit -m "docs: update documentation for precedence implementation

Mark design document as implemented and add precedence resolver
section to CLAUDE.md architecture overview.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Task 18: Final Integration Test

**Files:**
- None (testing only)

**Step 1: Run all tests**

Run: `make test`

Expected: All tests pass

**Step 2: Build full project**

Run: `dune build`

Expected: Clean build with no errors

**Step 3: Test a complex expression manually**

Run: `echo 'val x = 1 + 2 * 3 - 4 / 2' | dune exec shibboleth -- file /dev/stdin`

Expected: Correct OCaml output with proper precedence

**Step 4: Create final commit**

```bash
git add -A
git commit -m "feat: complete operator precedence implementation

Final integration of operator precedence feature:
- All unit tests passing
- All integration tests passing
- Full build clean
- Manual testing successful

Implements correct SML operator precedence for all standard
operators with proper left/right associativity.

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Success Criteria

✅ `1 + 2 * 3` converts to `1 + (2 * 3)` in OCaml
✅ `f x y + g z` converts to `((f x) y) + (g z)` in OCaml
✅ `x :: y :: z` converts correctly with right-associativity
✅ All existing tests continue to pass
✅ New precedence tests pass
✅ Clean dune build
✅ Documentation updated

---

## Notes

- Custom operators declared with `infix N` are parsed but use default precedence (future enhancement)
- Pattern precedence resolution is simplified (constructor application only)
- Parentheses create nested ExpApp sequences for proper grouping
