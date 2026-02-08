open Alcotest

let test_roundtrip () =
  let constructors =
    [
      {
        Context.Constructor_registry.name = "SOME";
        path = [ "SOME" ];
        ocaml_name = "Some";
      };
      {
        Context.Constructor_registry.name = "Cons";
        path = [ "List"; "Cons" ];
        ocaml_name = "Cons_";
      };
    ]
  in

  let sexp = Context.Constructor_manifest.to_sexp constructors in
  let parsed = Context.Constructor_manifest.from_sexp sexp in

  check int "same count" 2 (List.length parsed);
  check string "first name" "SOME"
    (List.nth parsed 0).Context.Constructor_registry.name;
  check string "second ocaml_name" "Cons_"
    (List.nth parsed 1).Context.Constructor_registry.ocaml_name

let test_write_and_read (switch, temp_dir) =
  let () = switch in
  let file_path = Filename.concat temp_dir "test.sctx" in
  let constructors =
    [
      {
        Context.Constructor_registry.name = "Foo";
        path = [ "M"; "Foo" ];
        ocaml_name = "Foo_";
      };
    ]
  in

  Context.Constructor_manifest.write_file file_path constructors;
  let loaded = Context.Constructor_manifest.read_file file_path in

  check int "loaded count" 1 (List.length loaded);
  check string "loaded name" "Foo"
    (List.nth loaded 0).Context.Constructor_registry.name

let test_combined_roundtrip () =
  let modules =
    [
      {
        Context.Constructor_manifest.module_path = "path/to/output1.ml";
        constructors =
          [
            {
              Context.Constructor_registry.name = "ok";
              path = [ "Result"; "ok" ];
              ocaml_name = "Ok_";
            };
          ];
      };
      {
        Context.Constructor_manifest.module_path = "path/to/output2.ml";
        constructors =
          [
            {
              Context.Constructor_registry.name = "Foo";
              path = [ "M"; "Foo" ];
              ocaml_name = "Foo_";
            };
            {
              Context.Constructor_registry.name = "Bar";
              path = [ "M"; "Bar" ];
              ocaml_name = "Bar_";
            };
          ];
      };
    ]
  in

  let sexp = Context.Constructor_manifest.combined_to_sexp modules in
  let parsed = Context.Constructor_manifest.combined_from_sexp sexp in

  check int "same module count" 2 (List.length parsed);
  check string "first module path" "path/to/output1.ml"
    (List.nth parsed 0).Context.Constructor_manifest.module_path;
  check int "first module constructor count" 1
    (List.length (List.nth parsed 0).Context.Constructor_manifest.constructors);
  check string "second module path" "path/to/output2.ml"
    (List.nth parsed 1).Context.Constructor_manifest.module_path;
  check int "second module constructor count" 2
    (List.length (List.nth parsed 1).Context.Constructor_manifest.constructors)

let test_combined_write_and_read () =
  let temp_dir = Filename.get_temp_dir_name () in
  let file_path = Filename.concat temp_dir "test_combined.sctx" in
  let modules =
    [
      {
        Context.Constructor_manifest.module_path = "mod_a.ml";
        constructors =
          [
            {
              Context.Constructor_registry.name = "A";
              path = [ "A" ];
              ocaml_name = "A";
            };
          ];
      };
      {
        Context.Constructor_manifest.module_path = "mod_b.ml";
        constructors =
          [
            {
              Context.Constructor_registry.name = "B";
              path = [ "B" ];
              ocaml_name = "B";
            };
          ];
      };
    ]
  in

  Context.Constructor_manifest.write_combined_file file_path modules;
  let loaded = Context.Constructor_manifest.read_combined_file file_path in

  check int "loaded module count" 2 (List.length loaded);
  check string "first loaded module path" "mod_a.ml"
    (List.nth loaded 0).Context.Constructor_manifest.module_path;
  check string "first loaded constructor name" "A"
    (List.nth
       (List.nth loaded 0).Context.Constructor_manifest.constructors
       0)
      .Context.Constructor_registry.name;
  check string "second loaded module path" "mod_b.ml"
    (List.nth loaded 1).Context.Constructor_manifest.module_path

let () =
  run "Constructor_manifest"
    [
      ("sexp", [ test_case "roundtrip" `Quick test_roundtrip ]);
      ( "file",
        [
          test_case "write and read" `Quick (fun () ->
              let temp_dir = Filename.get_temp_dir_name () in
              test_write_and_read ((), temp_dir));
        ] );
      ( "combined_sexp",
        [
          test_case "combined roundtrip" `Quick test_combined_roundtrip;
        ] );
      ( "combined_file",
        [
          test_case "combined write and read" `Quick
            test_combined_write_and_read;
        ] );
    ]
