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
