open Sexplib0.Sexp_conv

let to_sexp (constructors : Constructor_registry.constructor_info list) :
    Sexplib0.Sexp.t =
  sexp_of_list Constructor_registry.sexp_of_constructor_info constructors

let from_sexp (sexp : Sexplib0.Sexp.t) :
    Constructor_registry.constructor_info list =
  list_of_sexp Constructor_registry.constructor_info_of_sexp sexp

let write_file path constructors =
  let sexp = to_sexp constructors in
  let content = Sexplib0.Sexp.to_string_hum sexp in
  let oc = open_out path in
  try
    output_string oc content;
    output_char oc '\n';
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let read_file path =
  let ic = open_in path in
  try
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let sexp = Sexplib.Sexp.of_string content in
    from_sexp sexp
  with e ->
    close_in_noerr ic;
    raise e

let find_manifest ~search_paths ~module_name =
  let filename = module_name ^ ".sctx" in
  List.find_map
    (fun dir ->
      let path = Filename.concat dir filename in
      if Sys.file_exists path then Some path else None)
    search_paths
