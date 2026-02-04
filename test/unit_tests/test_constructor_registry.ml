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
