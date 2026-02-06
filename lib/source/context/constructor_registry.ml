open Sexplib0.Sexp_conv

type constructor_info = {
  name : string;
  path : string list;
  ocaml_name : string;
}
[@@deriving sexp]

type t = {
  qualified : (string list, constructor_info) Hashtbl.t;
  unqualified : (string, constructor_info list) Hashtbl.t;
}

let create () =
  { qualified = Hashtbl.create 128; unqualified = Hashtbl.create 128 }

let add_constructor registry ~path ~name ~ocaml_name =
  let info = { name; path; ocaml_name } in
  Hashtbl.add registry.qualified path info;

  (* Also add to unqualified lookup table for local scope access *)
  let existing =
    Hashtbl.find_opt registry.unqualified name |> Option.value ~default:[]
  in
  Hashtbl.replace registry.unqualified name (info :: existing)

let lookup registry ~path name =
  match path with
  | Some module_path ->
      let full_path = module_path @ [ name ] in
      Hashtbl.find_opt registry.qualified full_path
  | None -> (
      (* Check unqualified first *)
      match Hashtbl.find_opt registry.unqualified name with
      | Some (info :: _) -> Some info
      | Some [] | None ->
          (* Fall back to qualified with just [name] *)
          Hashtbl.find_opt registry.qualified [ name ])

let open_module registry ~module_path =
  let prefix_len = List.length module_path in
  Hashtbl.iter
    (fun path info ->
      (* Check if this constructor is in the opened module *)
      let rec has_prefix prefix lst =
        match (prefix, lst) with
        | [], _ -> true
        | _, [] -> false
        | p :: ps, l :: ls -> p = l && has_prefix ps ls
      in
      if has_prefix module_path path && List.length path > prefix_len then
        (* Add to unqualified scope *)
        let existing =
          Hashtbl.find_opt registry.unqualified info.name
          |> Option.value ~default:[]
        in
        Hashtbl.replace registry.unqualified info.name (info :: existing))
    registry.qualified

let add_module_alias registry ~alias ~target =
  (* Copy all constructors from target module to also be accessible under alias.
     For example, if target = ["M"] and alias = ["I"], then M.Root becomes
     accessible as I.Root. *)
  let target_len = List.length target in
  let entries_to_add = ref [] in
  Hashtbl.iter
    (fun path info ->
      (* Check if this constructor's path starts with target *)
      let rec has_prefix prefix lst =
        match (prefix, lst) with
        | [], _ -> true
        | _, [] -> false
        | p :: ps, l :: ls -> p = l && has_prefix ps ls
      in
      if has_prefix target path then
        (* Replace target prefix with alias prefix *)
        let suffix =
          let rec drop n lst =
            match (n, lst) with
            | 0, lst -> lst
            | _, [] -> []
            | n, _ :: rest -> drop (n - 1) rest
          in
          drop target_len path
        in
        let new_path = alias @ suffix in
        entries_to_add := (new_path, info) :: !entries_to_add)
    registry.qualified;
  (* Add the aliased entries *)
  List.iter
    (fun (new_path, info) ->
      let new_info = { info with path = new_path } in
      Hashtbl.add registry.qualified new_path new_info)
    !entries_to_add

let get_all_constructors registry =
  let constructors = ref [] in
  Hashtbl.iter
    (fun _path info -> constructors := info :: !constructors)
    registry.qualified;
  !constructors
