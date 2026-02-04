# Constructor Tracking Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement cross-module constructor tracking to enforce OCaml's strict capitalization conventions during SML to OCaml conversion.

**Architecture:** Three-phase approach: (1) Constructor registry in Context for tracking definitions, (2) Collection during backend declaration processing, (3) Resolution during pattern/expression processing with name transformation.

**Tech Stack:** OCaml 5.x, Ppxlib, Yojson (for manifest files), Alcotest (testing)

---

## Phase 1: Foundation - Constructor Registry

### Task 1: Create Constructor Registry Module

**Files:**
- Create: `lib/source/context/constructor_registry.ml`
- Create: `lib/source/context/constructor_registry.mli`
- Create: `test/unit/test_constructor_registry.ml`
- Modify: `lib/source/context/dune`

**Step 1: Write the failing test**

Create `test/unit/test_constructor_registry.ml`:

```ocaml
open Alcotest

let test_add_and_lookup () =
  let registry = Constructor_registry.create () in
  Constructor_registry.add_constructor registry
    ~path:["SOME"] ~name:"SOME" ~ocaml_name:"Some";

  let result = Constructor_registry.lookup registry ~path:None "SOME" in
  check (option string) "lookup unqualified SOME"
    (Some "Some") (Option.map (fun info -> info.Constructor_registry.ocaml_name) result)

let test_qualified_lookup () =
  let registry = Constructor_registry.create () in
  Constructor_registry.add_constructor registry
    ~path:["M"; "Cons"] ~name:"Cons" ~ocaml_name:"Cons_";

  let result = Constructor_registry.lookup registry ~path:(Some ["M"]) "Cons" in
  check (option string) "lookup M.Cons"
    (Some "Cons_") (Option.map (fun info -> info.Constructor_registry.ocaml_name) result)

let test_open_module () =
  let registry = Constructor_registry.create () in
  Constructor_registry.add_constructor registry
    ~path:["M"; "Foo"] ~name:"Foo" ~ocaml_name:"Foo_";

  Constructor_registry.open_module registry ~module_path:["M"];

  let result = Constructor_registry.lookup registry ~path:None "Foo" in
  check (option string) "lookup Foo after open M"
    (Some "Foo_") (Option.map (fun info -> info.Constructor_registry.ocaml_name) result)

let () =
  run "Constructor_registry" [
    "basic", [
      test_case "add and lookup" `Quick test_add_and_lookup;
      test_case "qualified lookup" `Quick test_qualified_lookup;
      test_case "open module" `Quick test_open_module;
    ]
  ]
```

**Step 2: Run test to verify it fails**

Run: `dune exec test/unit/test_constructor_registry.exe`
Expected: FAIL with "Unbound module Constructor_registry"

**Step 3: Write minimal interface**

Create `lib/source/context/constructor_registry.mli`:

```ocaml
(** Constructor registry for tracking SML constructors across modules *)

type constructor_info = {
  name: string;         (** Original SML name *)
  path: string list;    (** Module path including constructor name *)
  ocaml_name: string;   (** Transformed OCaml name *)
}

type t
(** The constructor registry type *)

val create : unit -> t
(** Create a new empty registry *)

val add_constructor : t -> path:string list -> name:string -> ocaml_name:string -> unit
(** Add a constructor to the registry with its fully qualified path *)

val lookup : t -> path:string list option -> string -> constructor_info option
(** Look up a constructor by name, optionally with module path qualifier *)

val open_module : t -> module_path:string list -> unit
(** Bring all constructors from a module into unqualified scope *)
```

**Step 4: Write minimal implementation**

Create `lib/source/context/constructor_registry.ml`:

```ocaml
type constructor_info = {
  name: string;
  path: string list;
  ocaml_name: string;
}

type t = {
  qualified: (string list, constructor_info) Hashtbl.t;
  unqualified: (string, constructor_info list) Hashtbl.t;
}

let create () = {
  qualified = Hashtbl.create 128;
  unqualified = Hashtbl.create 128;
}

let add_constructor registry ~path ~name ~ocaml_name =
  let info = { name; path; ocaml_name } in
  Hashtbl.add registry.qualified path info;

  (* Also add to unqualified if path is just [name] *)
  if path = [name] then
    let existing = Hashtbl.find_opt registry.unqualified name |> Option.value ~default:[] in
    Hashtbl.replace registry.unqualified name (info :: existing)

let lookup registry ~path name =
  match path with
  | Some module_path ->
      let full_path = module_path @ [name] in
      Hashtbl.find_opt registry.qualified full_path
  | None ->
      (* Check unqualified first *)
      (match Hashtbl.find_opt registry.unqualified name with
      | Some (info :: _) -> Some info
      | Some [] | None ->
          (* Fall back to qualified with just [name] *)
          Hashtbl.find_opt registry.qualified [name])

let open_module registry ~module_path =
  let prefix_len = List.length module_path in
  Hashtbl.iter (fun path info ->
    (* Check if this constructor is in the opened module *)
    let rec has_prefix prefix lst =
      match prefix, lst with
      | [], _ -> true
      | _, [] -> false
      | p :: ps, l :: ls -> p = l && has_prefix ps ls
    in
    if has_prefix module_path path && List.length path > prefix_len then
      (* Add to unqualified scope *)
      let existing = Hashtbl.find_opt registry.unqualified info.name |> Option.value ~default:[] in
      Hashtbl.replace registry.unqualified info.name (info :: existing)
  ) registry.qualified
```

**Step 5: Update dune file**

Modify `lib/source/context/dune`:

```ocaml
(library
 (name context)
 (libraries common)
 (modules context info basis get_context constructor_registry))
```

**Step 6: Run test to verify it passes**

Run: `dune exec test/unit/test_constructor_registry.exe`
Expected: PASS (3 tests)

**Step 7: Commit**

```bash
git add lib/source/context/constructor_registry.ml
git add lib/source/context/constructor_registry.mli
git add test/unit/test_constructor_registry.ml
git add lib/source/context/dune
git commit -m "feat: add constructor registry for tracking SML constructors

- Qualified and unqualified lookup tables
- Support for open module bringing constructors into scope
- Unit tests for basic operations

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

### Task 2: Create Constructor Transformation Module

**Files:**
- Create: `lib/source/backend/constructor_transform.ml`
- Create: `lib/source/backend/constructor_transform.mli`
- Create: `test/unit/test_constructor_transform.ml`
- Modify: `lib/source/backend/dune`

**Step 1: Write the failing test**

Create `test/unit/test_constructor_transform.ml`:

```ocaml
open Alcotest

let test_lowercase_constructor () =
  check string "lowercase a -> A_"
    "A_" (Constructor_transform.transform_constructor "a");
  check string "lowercase some -> Some_"
    "Some_" (Constructor_transform.transform_constructor "some")

let test_uppercase_with_underscore () =
  check string "B_ -> B__"
    "B__" (Constructor_transform.transform_constructor "B_")

let test_already_valid () =
  check string "Foo unchanged"
    "Foo" (Constructor_transform.transform_constructor "Foo");
  check string "SOME -> Some (capitalize)"
    "Some" (Constructor_transform.transform_constructor "SOME")

let test_variable_lowercase () =
  check string "SOME -> some_"
    "some_" (Constructor_transform.transform_to_lowercase "SOME");
  check string "Foo -> foo_"
    "foo_" (Constructor_transform.transform_to_lowercase "Foo");
  check string "bar -> bar"
    "bar" (Constructor_transform.transform_to_lowercase "bar")

let () =
  run "Constructor_transform" [
    "constructor", [
      test_case "lowercase constructor" `Quick test_lowercase_constructor;
      test_case "uppercase with underscore" `Quick test_uppercase_with_underscore;
      test_case "already valid" `Quick test_already_valid;
    ];
    "variable", [
      test_case "variable lowercase" `Quick test_variable_lowercase;
    ]
  ]
```

**Step 2: Run test to verify it fails**

Run: `dune exec test/unit/test_constructor_transform.exe`
Expected: FAIL with "Unbound module Constructor_transform"

**Step 3: Write interface**

Create `lib/source/backend/constructor_transform.mli`:

```ocaml
(** Name transformation for SML to OCaml constructor conversion *)

val transform_constructor : string -> string
(** Transform a constructor name to valid OCaml format:
    - Lowercase: a -> A_, some -> Some_
    - Uppercase with trailing _: B_ -> B__
    - Already valid: Foo -> Foo, SOME -> Some *)

val transform_to_lowercase : string -> string
(** Transform a variable name to lowercase:
    - SOME -> some_
    - Foo -> foo_
    - bar -> bar (no change) *)
```

**Step 4: Write implementation**

Create `lib/source/backend/constructor_transform.ml`:

```ocaml
let is_all_uppercase s =
  String.for_all (fun c -> not (Char.lowercase_ascii c <> c)) s

let transform_constructor name =
  if String.length name = 0 then name
  else if String.ends_with ~suffix:"_" name then
    (* B_ -> B__ *)
    name ^ "_"
  else if Char.uppercase_ascii name.[0] = name.[0] then
    (* Already starts with uppercase *)
    if is_all_uppercase name && String.length name > 1 then
      (* SOME -> Some *)
      String.capitalize_ascii (String.lowercase_ascii name)
    else
      (* Foo -> Foo *)
      name
  else
    (* Lowercase: a -> A_, some -> Some_ *)
    String.capitalize_ascii name ^ "_"

let transform_to_lowercase name =
  if String.length name = 0 then name
  else if Char.lowercase_ascii name.[0] = name.[0] then
    (* Already lowercase *)
    name
  else
    (* Uppercase: SOME -> some_, Foo -> foo_ *)
    String.uncapitalize_ascii name ^ "_"
```

**Step 5: Update dune file**

Modify `lib/source/backend/dune` to add `constructor_transform` to modules list.

**Step 6: Run test to verify it passes**

Run: `dune exec test/unit/test_constructor_transform.exe`
Expected: PASS (4 tests)

**Step 7: Commit**

```bash
git add lib/source/backend/constructor_transform.ml
git add lib/source/backend/constructor_transform.mli
git add test/unit/test_constructor_transform.ml
git add lib/source/backend/dune
git commit -m "feat: add constructor name transformation utilities

- Transform lowercase constructors to uppercase
- Handle trailing underscore doubling
- Transform variables to lowercase
- Unit tests for all transformation rules

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

### Task 3: Create Constructor Manifest Module

**Files:**
- Create: `lib/source/context/constructor_manifest.ml`
- Create: `lib/source/context/constructor_manifest.mli`
- Create: `test/unit/test_constructor_manifest.ml`
- Modify: `lib/source/context/dune` (add yojson dependency)
- Modify: `shibboleth.opam` (add yojson dependency)

**Step 1: Write the failing test**

Create `test/unit/test_constructor_manifest.ml`:

```ocaml
open Alcotest

let test_roundtrip () =
  let constructors = [
    { Constructor_registry.name = "SOME";
      path = ["SOME"];
      ocaml_name = "Some" };
    { Constructor_registry.name = "Cons";
      path = ["List"; "Cons"];
      ocaml_name = "Cons_" }
  ] in

  let json = Constructor_manifest.to_json constructors in
  let parsed = Constructor_manifest.from_json json in

  check int "same count" 2 (List.length parsed);
  check string "first name" "SOME" (List.nth parsed 0).Constructor_registry.name;
  check string "second ocaml_name" "Cons_" (List.nth parsed 1).Constructor_registry.ocaml_name

let test_write_and_read (switch, temp_dir) =
  let () = switch in
  let file_path = Filename.concat temp_dir "test.shibboleth-constructors" in
  let constructors = [
    { Constructor_registry.name = "Foo";
      path = ["M"; "Foo"];
      ocaml_name = "Foo_" }
  ] in

  Constructor_manifest.write_file file_path constructors;
  let loaded = Constructor_manifest.read_file file_path in

  check int "loaded count" 1 (List.length loaded);
  check string "loaded name" "Foo" (List.nth loaded 0).Constructor_registry.name

let () =
  run "Constructor_manifest" [
    "json", [
      test_case "roundtrip" `Quick test_roundtrip;
    ];
    "file", [
      test_case "write and read" `Quick (fun () ->
        let temp_dir = Filename.get_temp_dir_name () in
        test_write_and_read ((), temp_dir));
    ]
  ]
```

**Step 2: Run test to verify it fails**

Run: `dune exec test/unit/test_constructor_manifest.exe`
Expected: FAIL with "Unbound module Constructor_manifest"

**Step 3: Add yojson dependency**

Modify `shibboleth.opam`, add to depends section:

```
  "yojson" {>= "2.0.0"}
```

Then run: `opam install yojson --yes`

Modify `lib/source/context/dune`:

```ocaml
(library
 (name context)
 (libraries common yojson)
 (modules context info basis get_context constructor_registry constructor_manifest))
```

**Step 4: Write interface**

Create `lib/source/context/constructor_manifest.mli`:

```ocaml
(** Manifest file I/O for constructor information *)

val to_json : Constructor_registry.constructor_info list -> Yojson.Safe.t
(** Convert constructor list to JSON *)

val from_json : Yojson.Safe.t -> Constructor_registry.constructor_info list
(** Parse constructor list from JSON *)

val write_file : string -> Constructor_registry.constructor_info list -> unit
(** Write constructors to a manifest file *)

val read_file : string -> Constructor_registry.constructor_info list
(** Read constructors from a manifest file *)

val find_manifest : search_paths:string list -> module_name:string -> string option
(** Find a manifest file for a module in the search paths *)
```

**Step 5: Write implementation**

Create `lib/source/context/constructor_manifest.ml`:

```ocaml
open Yojson.Safe

let to_json (constructors : Constructor_registry.constructor_info list) : t =
  `Assoc [
    "constructors", `List (
      List.map (fun info ->
        `Assoc [
          "name", `String info.Constructor_registry.name;
          "path", `List (List.map (fun s -> `String s) info.path);
          "ocaml_name", `String info.ocaml_name;
        ]
      ) constructors
    )
  ]

let from_json (json : t) : Constructor_registry.constructor_info list =
  match json with
  | `Assoc assoc ->
      (match List.assoc_opt "constructors" assoc with
      | Some (`List items) ->
          List.map (fun item ->
            match item with
            | `Assoc fields ->
                let name = match List.assoc_opt "name" fields with
                  | Some (`String s) -> s
                  | _ -> failwith "missing name" in
                let path = match List.assoc_opt "path" fields with
                  | Some (`List parts) ->
                      List.map (function `String s -> s | _ -> failwith "invalid path") parts
                  | _ -> failwith "missing path" in
                let ocaml_name = match List.assoc_opt "ocaml_name" fields with
                  | Some (`String s) -> s
                  | _ -> failwith "missing ocaml_name" in
                { Constructor_registry.name; path; ocaml_name }
            | _ -> failwith "invalid constructor entry"
          ) items
      | _ -> failwith "missing constructors array")
  | _ -> failwith "invalid manifest format"

let write_file path constructors =
  let json = to_json constructors in
  let oc = open_out path in
  try
    Yojson.Safe.pretty_to_channel oc json;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let read_file path =
  let json = from_file path in
  from_json json

let find_manifest ~search_paths ~module_name =
  let filename = module_name ^ ".shibboleth-constructors" in
  List.find_map (fun dir ->
    let path = Filename.concat dir filename in
    if Sys.file_exists path then Some path else None
  ) search_paths
```

**Step 6: Run test to verify it passes**

Run: `dune exec test/unit/test_constructor_manifest.exe`
Expected: PASS (2 tests)

**Step 7: Commit**

```bash
git add lib/source/context/constructor_manifest.ml
git add lib/source/context/constructor_manifest.mli
git add test/unit/test_constructor_manifest.ml
git add lib/source/context/dune
git add shibboleth.opam
git commit -m "feat: add constructor manifest file I/O

- JSON serialization for constructor information
- File reading/writing with search path support
- Unit tests for roundtrip serialization

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Phase 2: Context Integration

### Task 4: Add Registry to Context

**Files:**
- Modify: `lib/source/context/context.ml`
- Modify: `lib/source/context/context.mli`

**Step 1: Read current Context module**

Read both files to understand current structure.

**Step 2: Add registry field to Context.t**

Modify `lib/source/context/context.mli`:

Add to the `t` type:

```ocaml
  constructor_registry : Constructor_registry.t;
  (** Registry of constructors across modules *)
```

**Step 3: Update create function**

Modify `lib/source/context/context.ml`:

Update the `create` or initialization function to include:

```ocaml
  let registry = Constructor_registry.create () in

  (* Pre-populate Basis Library constructors *)
  let basis_constructors = [
    ("SOME", ["SOME"], "Some");
    ("NONE", ["NONE"], "None");
    ("true", ["true"], "true");
    ("false", ["false"], "false");
    ("nil", ["nil"], "[]");
    ("::", ["::"], "::");
  ] in
  List.iter (fun (name, path, ocaml_name) ->
    Constructor_registry.add_constructor registry ~path ~name ~ocaml_name
  ) basis_constructors;

  { (* existing fields... *)
    constructor_registry = registry;
  }
```

**Step 4: Build and verify**

Run: `dune build`
Expected: SUCCESS

**Step 5: Commit**

```bash
git add lib/source/context/context.ml
git add lib/source/context/context.mli
git commit -m "feat: integrate constructor registry into Context

- Add constructor_registry field to Context.t
- Pre-populate with Basis Library constructors
- Provides foundation for backend collection phase

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Phase 3: Backend Collection

### Task 5: Add Constructor Collection to Backend

**Files:**
- Modify: `lib/source/backend/backend.ml`
- Modify: `lib/source/backend/backend_sig.ml`

**Step 1: Add current_path tracking**

In `lib/source/backend/backend.ml`, inside the `Make` functor, add after existing state variables:

```ocaml
  let current_path : string list ref = ref []

  let push_module name =
    current_path := !current_path @ [name]

  let pop_module () =
    match List.rev !current_path with
    | _ :: rest -> current_path := List.rev rest
    | [] -> ()

  let register_constructor name =
    let ocaml_name = Constructor_transform.transform_constructor name in
    let full_path = !current_path @ [name] in
    Constructor_registry.add_constructor
      Context.context.constructor_registry
      ~path:full_path
      ~name
      ~ocaml_name
```

**Step 2: Modify process_dat_bind**

Find the `process_dat_bind` function and add constructor registration:

```ocaml
let rec process_dat_bind (db : Ast.data_binding) : Parsetree.type_declaration list =
  match db with
  | DatBind (tyvarseq, tycon, conbind) ->
      (* Existing code... *)

      (* Register all constructors *)
      let rec register_constructors cb =
        match unbox_node cb with
        | ConBind (con, _, next_opt) ->
            let con_name = idx_to_string con in
            register_constructor con_name;
            (match next_opt with
            | Some next -> register_constructors next
            | None -> ())
      in
      register_constructors conbind;

      (* Rest of existing code... *)
```

**Step 3: Modify process_exn_bind**

Find the `process_exn_bind` function and add constructor registration:

```ocaml
let process_exn_bind (eb : Ast.exn_bind) : Parsetree.extension_constructor list =
  match eb with
  | ExnBind (con, _, next_opt) ->
      let con_name = idx_to_string con in
      register_constructor con_name;

      (* Rest of existing code... *)
```

**Step 4: Add module tracking to structure processing**

Find where structures are processed and add push/pop:

```ocaml
(* When entering a structure *)
let process_structure name body =
  push_module name;
  let result = (* existing processing *) in
  pop_module ();
  result
```

**Step 5: Build and verify**

Run: `dune build`
Expected: SUCCESS

**Step 6: Commit**

```bash
git add lib/source/backend/backend.ml
git commit -m "feat: add constructor collection during backend processing

- Track current module path with push/pop
- Register constructors from datatype declarations
- Register constructors from exception declarations
- Apply transformation rules during registration

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Phase 4: Backend Resolution

### Task 6: Add Constructor Resolution to Patterns

**Files:**
- Modify: `lib/source/backend/backend.ml`
- Create: `test/unit/test_pattern_constructor.ml`

**Step 1: Write integration test**

Create `test/unit/test_pattern_constructor.ml`:

```ocaml
(* Test that lowercase constructors in patterns are transformed *)

module TestConfig = struct
  let config = { Common.input_file = ""; output_file = None; (* ... *) }
end

module TestContext = struct
  let lexbuf = ""
  let context =
    let ctx = Context.create () in
    (* Add test constructor *)
    Constructor_registry.add_constructor ctx.constructor_registry
      ~path:["foo"] ~name:"foo" ~ocaml_name:"Foo_";
    ctx
end

module TestBackend = Backend.Make(TestContext)(TestConfig)

let test_lowercase_constructor_pattern () =
  (* SML: case x of foo => 1 *)
  let pat = Ast.box_node (Ast.PatIdx (Ast.box_node (Ast.IdxIdx (Ast.box_node "foo")))) in
  let result = TestBackend.process_pat ~is_head:true pat in

  (* Should transform to Foo_ *)
  match result.ppat_desc with
  | Ppat_construct ({ txt = Lident "Foo_"; _ }, None) -> ()
  | _ -> Alcotest.fail "Expected constructor pattern Foo_"

(* Additional test cases... *)
```

**Step 2: Run test to verify it fails**

Run: `dune exec test/unit/test_pattern_constructor.exe`
Expected: FAIL (constructor not yet resolved)

**Step 3: Modify process_pat**

In `lib/source/backend/backend.ml`, find `process_pat` function:

```ocaml
let rec process_pat ?(is_arg = false) ?(is_head = false) (pat : Ast.pat node)
    : Parsetree.pattern =
  match unbox_node pat with
  | PatIdx idx ->
      let name_parts = idx_to_name idx in
      let name = List.nth name_parts (List.length name_parts - 1) in

      (* Extract module path if qualified *)
      let qual_path =
        if List.length name_parts > 1 then
          Some (List.rev (List.tl (List.rev name_parts)))
        else None
      in

      (* Try to look up as constructor *)
      let lookup_result = Constructor_registry.lookup
        Context.context.constructor_registry ~path:qual_path name in

      (match lookup_result with
      | Some ctor_info when is_head || not is_arg ->
          (* It's a constructor - use transformed name *)
          let lid =
            match qual_path with
            | Some path ->
                Ppxlib.Longident.unflatten (path @ [ctor_info.ocaml_name])
                |> Option.get
            | None -> Ppxlib.Longident.Lident ctor_info.ocaml_name
          in
          Builder.ppat_construct ~loc:Location.none (ghost lid) None
      | _ ->
          (* It's a variable - force lowercase *)
          let var_name = Constructor_transform.transform_to_lowercase name in
          Builder.ppat_var ~loc:Location.none (ghost var_name))

  (* Rest of pattern cases... *)
```

**Step 4: Run test to verify it passes**

Run: `dune exec test/unit/test_pattern_constructor.exe`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/source/backend/backend.ml
git add test/unit/test_pattern_constructor.ml
git commit -m "feat: add constructor resolution in patterns

- Lookup names in constructor registry
- Apply transformation for constructors
- Force lowercase for variable bindings
- Integration test for pattern transformation

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

### Task 7: Add Constructor Resolution to Expressions

**Files:**
- Modify: `lib/source/backend/backend.ml`
- Create: `test/unit/test_expression_constructor.ml`

**Step 1: Write integration test**

Create `test/unit/test_expression_constructor.ml`:

```ocaml
(* Test that lowercase constructors in expressions are transformed *)

(* Similar structure to test_pattern_constructor.ml *)

let test_lowercase_constructor_expr () =
  (* SML: val x = foo *)
  let exp = Ast.box_node (Ast.ExpIdx (Ast.box_node (Ast.IdxIdx (Ast.box_node "foo")))) in
  let result = TestBackend.process_exp exp in

  (* Should transform to Foo_ *)
  match result.pexp_desc with
  | Pexp_construct ({ txt = Lident "Foo_"; _ }, None) -> ()
  | _ -> Alcotest.fail "Expected constructor expression Foo_"
```

**Step 2: Run test to verify it fails**

Run: `dune exec test/unit/test_expression_constructor.exe`
Expected: FAIL

**Step 3: Modify process_exp**

In `lib/source/backend/backend.ml`, find `process_exp`:

```ocaml
let rec process_exp (exp : Ast.expression node) : Parsetree.expression =
  match unbox_node exp with
  | ExpIdx idx ->
      let name_parts = idx_to_name idx in
      let name = List.nth name_parts (List.length name_parts - 1) in

      let qual_path =
        if List.length name_parts > 1 then
          Some (List.rev (List.tl (List.rev name_parts)))
        else None
      in

      let lookup_result = Constructor_registry.lookup
        Context.context.constructor_registry ~path:qual_path name in

      (match lookup_result with
      | Some ctor_info ->
          (* Constructor reference *)
          let lid =
            match qual_path with
            | Some path ->
                Ppxlib.Longident.unflatten (path @ [ctor_info.ocaml_name])
                |> Option.get
            | None -> Ppxlib.Longident.Lident ctor_info.ocaml_name
          in
          Builder.pexp_construct ~loc:Location.none (ghost lid) None
      | None ->
          (* Value reference - use existing logic *)
          let lid = build_longident name_parts in
          Builder.pexp_ident ~loc:Location.none (ghost lid))

  (* Rest of expression cases... *)
```

**Step 4: Run test to verify it passes**

Run: `dune exec test/unit/test_expression_constructor.exe`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/source/backend/backend.ml
git add test/unit/test_expression_constructor.ml
git commit -m "feat: add constructor resolution in expressions

- Lookup constructor names in registry
- Apply transformation for constructor references
- Preserve value references for non-constructors
- Integration test for expression transformation

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

### Task 8: Add Open Statement Handling

**Files:**
- Modify: `lib/source/backend/backend.ml`
- Create: `test/unit/test_open_module.ml`

**Step 1: Write integration test**

Create `test/unit/test_open_module.ml`:

```ocaml
let test_open_brings_constructors_into_scope () =
  let ctx = Context.create () in
  Constructor_registry.add_constructor ctx.constructor_registry
    ~path:["M"; "Foo"] ~name:"Foo" ~ocaml_name:"Foo_";

  (* Before open: Foo not found unqualified *)
  let before = Constructor_registry.lookup ctx.constructor_registry ~path:None "Foo" in
  Alcotest.(check (option reject)) "Foo not found before open" None before;

  (* Open M *)
  Constructor_registry.open_module ctx.constructor_registry ~module_path:["M"];

  (* After open: Foo found unqualified *)
  let after = Constructor_registry.lookup ctx.constructor_registry ~path:None "Foo" in
  Alcotest.(check (option string)) "Foo found after open"
    (Some "Foo_") (Option.map (fun i -> i.Constructor_registry.ocaml_name) after)
```

**Step 2: Run test to verify it fails**

Run: `dune exec test/unit/test_open_module.exe`
Expected: FAIL (if test expects integration with backend)

**Step 3: Modify process_dec for OpenDec**

In `lib/source/backend/backend.ml`, find where declarations are processed:

```ocaml
let process_dec (dec : Ast.declaration) : Parsetree.structure_item list =
  match dec with
  | OpenDec module_idx ->
      let module_path = idx_to_name module_idx in

      (* Bring constructors into scope *)
      Constructor_registry.open_module
        Context.context.constructor_registry
        ~module_path;

      (* Generate open statement *)
      let lid = build_longident module_path in
      [Builder.pstr_open ~loc:Location.none
        (Builder.open_infos ~loc:Location.none ~expr:(ghost lid) ~override:Fresh)]

  (* Other declaration cases... *)
```

**Step 4: Run test to verify it passes**

Run: `dune exec test/unit/test_open_module.exe`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/source/backend/backend.ml
git add test/unit/test_open_module.ml
git commit -m "feat: handle open statements with constructor scope

- Process OpenDec to bring module constructors into scope
- Update registry when open is encountered
- Test that open makes constructors available unqualified

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Phase 5: Manifest Generation and Dependency Loading

### Task 9: Generate Manifest Files After Compilation

**Files:**
- Modify: `lib/source/backend/backend.ml`
- Modify: `lib/cli/cmd_convert_file.ml`

**Step 1: Add manifest generation to backend**

In `lib/source/backend/backend.ml`, add function to collect all constructors:

```ocaml
let get_all_constructors () : Constructor_registry.constructor_info list =
  let constructors = ref [] in
  Hashtbl.iter (fun _path info ->
    constructors := info :: !constructors
  ) Context.context.constructor_registry.qualified;
  !constructors
```

**Step 2: Modify CLI to write manifest**

In `lib/cli/cmd_convert_file.ml`, after successful compilation:

```ocaml
(* After calling Backend.process_sml *)
let output_file = (* determine output file path *) in

(* Write manifest if we produced output *)
(match output_file with
| Some path ->
    let manifest_path = path ^ ".shibboleth-constructors" in
    let constructors = Backend.get_all_constructors () in
    Constructor_manifest.write_file manifest_path constructors
| None -> ())
```

**Step 3: Build and test**

Run: `dune build`
Run: `dune exec shibboleth -- file examples/simple.sml`
Check: `ls examples/simple.ml.shibboleth-constructors`
Expected: Manifest file created

**Step 4: Commit**

```bash
git add lib/source/backend/backend.ml
git add lib/cli/cmd_convert_file.ml
git commit -m "feat: generate constructor manifest after compilation

- Collect all registered constructors after backend processing
- Write manifest file alongside output OCaml file
- Enable downstream modules to load constructor information

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

### Task 10: Load Constructors from Dependencies

**Files:**
- Modify: `lib/source/backend/backend.ml`
- Modify: `lib/source/context/context.ml`

**Step 1: Add dependency loading function**

In `lib/source/context/context.ml`:

```ocaml
let load_module_constructors context ~module_name ~search_paths =
  (* Try manifest file first *)
  match Constructor_manifest.find_manifest ~search_paths ~module_name with
  | Some manifest_path ->
      let constructors = Constructor_manifest.read_file manifest_path in
      List.iter (fun info ->
        Constructor_registry.add_constructor context.constructor_registry
          ~path:info.path ~name:info.name ~ocaml_name:info.ocaml_name
      ) constructors;
      true
  | None ->
      (* TODO: Try parsing source file as fallback *)
      false
```

**Step 2: Call on open if module not in registry**

In `lib/source/backend/backend.ml`, modify open handling:

```ocaml
let process_dec (dec : Ast.declaration) : Parsetree.structure_item list =
  match dec with
  | OpenDec module_idx ->
      let module_path = idx_to_name module_idx in
      let module_name = String.concat "." module_path in

      (* Try to load module if not already known *)
      let search_paths = ["."; (* add configured include paths *)] in
      let _loaded = Context.load_module_constructors
        Context.context ~module_name ~search_paths in

      (* Bring constructors into scope *)
      Constructor_registry.open_module
        Context.context.constructor_registry
        ~module_path;

      (* Generate open statement *)
      (* ... existing code ... *)
```

**Step 3: Build and test with multi-module example**

Create test files demonstrating dependency loading.

**Step 4: Commit**

```bash
git add lib/source/context/context.ml
git add lib/source/backend/backend.ml
git commit -m "feat: load constructor info from dependencies

- Search for manifest files when module is opened
- Load constructor definitions into registry
- Support cross-module constructor resolution

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Phase 6: Integration Testing and Refinement

### Task 11: End-to-End Integration Tests

**Files:**
- Create: `test/integration/test_constructor_tracking.ml`
- Create: `examples/constructor_test.sml`

**Step 1: Create comprehensive SML test file**

Create `examples/constructor_test.sml`:

```sml
(* Test lowercase constructors *)
datatype foo = bar | baz of int

val x = bar
val y = case bar of
    bar => 1
  | baz n => n

(* Test with qualified access *)
structure M = struct
  datatype result = ok | error of string
end

val z = M.ok

(* Test with open *)
open M
val w = ok

(* Test variable shadowing constructor *)
datatype opt = some | none
val some = 42  (* Variable named 'some' *)
```

**Step 2: Write integration test**

Create `test/integration/test_constructor_tracking.ml`:

```ocaml
(* Run full compilation and verify output *)
let test_full_compilation () =
  let input = "examples/constructor_test.sml" in
  let output = "examples/constructor_test.ml" in

  (* Run compiler *)
  let result = Cli.run ["file"; input; "-o"; output] in
  Alcotest.(check int) "compilation succeeds" 0 result;

  (* Check manifest was created *)
  Alcotest.(check bool) "manifest exists"
    true (Sys.file_exists (output ^ ".shibboleth-constructors"));

  (* Read and verify output contains transformed names *)
  let ocaml_code = EzFile.read_file output in
  Alcotest.(check bool) "contains Bar_"
    true (String.contains_s ocaml_code "Bar_");
  Alcotest.(check bool) "contains some_ variable"
    true (String.contains_s ocaml_code "let some_ = 42")
```

**Step 3: Run test**

Run: `dune exec test/integration/test_constructor_tracking.exe`
Expected: PASS (or identify issues to fix)

**Step 4: Fix any issues found**

Iterate on implementation based on test failures.

**Step 5: Commit**

```bash
git add test/integration/test_constructor_tracking.ml
git add examples/constructor_test.sml
git commit -m "test: add end-to-end integration tests

- Comprehensive SML test file with various constructor cases
- Integration test verifying full compilation pipeline
- Check manifest generation and name transformation

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

### Task 12: Documentation and Cleanup

**Files:**
- Create: `docs/constructor-tracking.md`
- Modify: `README.md`
- Modify: `CLAUDE.md`

**Step 1: Write feature documentation**

Create `docs/constructor-tracking.md` explaining:
- How constructor tracking works
- Manifest file format
- Name transformation rules
- How to debug issues

**Step 2: Update README**

Add section about constructor tracking feature.

**Step 3: Update CLAUDE.md**

Add notes about constructor registry for future AI assistance.

**Step 4: Final cleanup**

Remove any debug logging, clean up comments, run formatter.

**Step 5: Final commit**

```bash
git add docs/constructor-tracking.md
git add README.md
git add CLAUDE.md
git commit -m "docs: document constructor tracking feature

- Comprehensive feature documentation
- Update README with new capability
- Update CLAUDE.md for future development

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
```

---

## Testing Strategy Summary

**Unit Tests:**
- Constructor_registry operations (Task 1)
- Constructor_transform name rules (Task 2)
- Constructor_manifest serialization (Task 3)
- Pattern transformation (Task 6)
- Expression transformation (Task 7)
- Open statement handling (Task 8)

**Integration Tests:**
- End-to-end compilation (Task 11)
- Multi-module dependencies (Task 10)
- Manifest file generation and loading (Tasks 9-10)

**Run all tests:**
```bash
make test
dune runtest
```

---

## Rollback Plan

If issues arise, rollback order:
1. Task 12 (docs) - safe to revert
2. Task 11 (integration tests) - safe to revert
3. Tasks 9-10 (manifest generation/loading) - can disable
4. Tasks 6-8 (resolution) - core functionality
5. Task 5 (collection) - core functionality
6. Tasks 1-4 (foundation) - only if fundamentally broken

Use: `git revert <commit-hash>` or `git reset --hard <commit-hash>`

---

## Success Criteria

- [ ] All unit tests pass
- [ ] Integration tests pass
- [ ] Can compile multi-module SML code with lowercase constructors
- [ ] Generated OCaml code uses correct capitalization
- [ ] Manifest files are created and loaded properly
- [ ] No regressions in existing functionality
- [ ] Documentation is complete and clear
