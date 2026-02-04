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
