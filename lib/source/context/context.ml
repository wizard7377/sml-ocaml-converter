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

let basis_context = create Basis.basis_context
