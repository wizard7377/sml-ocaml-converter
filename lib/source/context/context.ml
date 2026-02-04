module Info = Info
module Basis = Basis
module Get_context = Get_context

type t = {
  info : Info.t;
  constructor_registry : Constructor_registry.t;
}

let create info =
  let registry = Constructor_registry.create () in

  (* Pre-populate Basis Library constructors *)
  let basis_constructors = [
    ("SOME", ["SOME"], "Some");
    ("NONE", ["NONE"], "None");
    ("true", ["true"], "true");
    ("false", ["false"], "false");
    ("nil", ["nil"], "[]");
    ("::", ["::"], "::");
    (* Standard exceptions *)
    ("Chr", ["Chr"], "Chr");
    ("Div", ["Div"], "Div");
    ("Domain", ["Domain"], "Domain");
    ("Empty", ["Empty"], "Empty");
    ("Fail", ["Fail"], "Fail");
    ("Match", ["Match"], "Match");
    ("Option", ["Option"], "Option");
    ("Overflow", ["Overflow"], "Overflow");
    ("Size", ["Size"], "Size");
    ("Span", ["Span"], "Span");
    ("Subscript", ["Subscript"], "Subscript");
    ("LESS", ["LESS"], "LESS");
    ("EQUAL", ["EQUAL"], "EQUAL");
    ("GREATER", ["GREATER"], "GREATER");
  ] in
  List.iter (fun (name, path, ocaml_name) ->
    Constructor_registry.add_constructor registry ~path ~name ~ocaml_name
  ) basis_constructors;

  { info; constructor_registry = registry }

let merge t1 t2 =
  (* Merge info *)
  let merged_info = Info.merge t1.info t2.info in
  (* Create new context with merged info *)
  let merged_ctx = create merged_info in
  (* Copy constructors from both registries into the merged one *)
  Hashtbl.iter (fun path info ->
    Constructor_registry.add_constructor merged_ctx.constructor_registry
      ~path:info.Constructor_registry.path
      ~name:info.name
      ~ocaml_name:info.ocaml_name
  ) t1.constructor_registry.qualified;
  Hashtbl.iter (fun path info ->
    Constructor_registry.add_constructor merged_ctx.constructor_registry
      ~path:info.Constructor_registry.path
      ~name:info.name
      ~ocaml_name:info.ocaml_name
  ) t2.constructor_registry.qualified;
  merged_ctx

let basis_context = create Basis.basis_context

let load_module_constructors context ~module_name ~search_paths =
  (* Try manifest file first *)
  match Constructor_manifest.find_manifest ~search_paths ~module_name with
  | Some manifest_path ->
      (try
        let constructors = Constructor_manifest.read_file manifest_path in
        List.iter (fun info ->
          Constructor_registry.add_constructor context.constructor_registry
            ~path:info.Constructor_registry.path
            ~name:info.name
            ~ocaml_name:info.ocaml_name
        ) constructors;
        true
      with _ -> false)
  | None -> false
