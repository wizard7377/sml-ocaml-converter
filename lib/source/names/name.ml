(** Implementation of qualified name representation.

    See {!Name} for the public interface documentation. *)

(** @inline *)
type package = Global | Private of string | Public of string | Local of string

type path = string list
type name = string

(** Internal record representation of a qualified name. *)
type t = {
  package : package;
  path : path;
  root : name;
}

let make_name ~package ~path ~root = {
  package = package;
  path = path;
  root = root;
}

let parse_name : t -> (package * path * name) = fun n ->
  (n.package, n.path, n.root)

let map_name : f:(name -> name) -> t -> t = fun ~f n ->
  { n with root = f n.root }

let name_to_string ?(qualified = false) (n : t) : string =
  if qualified then
    let path_str = match n.path with
      | [] -> ""
      | p_list -> String.concat "." p_list ^ "."
    in
    path_str ^ n.root
  else
    n.root
