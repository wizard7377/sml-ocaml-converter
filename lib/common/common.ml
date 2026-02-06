let get_file filename = Result.get_ok @@ Bos.OS.File.read @@ Fpath.v filename
let write_file filename content = () (* TODO *)

type ident = Name of string | Symbol of string

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

let higher : int option -> int -> bool -> bool =
 fun x y def -> match x with None -> def | Some v -> v > y

let lower : int option -> int -> bool -> bool =
 fun x y def -> match x with None -> def | Some v -> v < y

let last (lst : 'a list) : 'a =
  assert (List.length lst > 0);
  List.nth lst (List.length lst - 1)

let rec map_last (f : 'a -> 'a) (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | [ x ] -> [ f x ]
  | first :: rest -> first :: map_last f rest

(** Convert dashes to underscores in path components (filenames and directories)
    without affecting path separators *)
let convert_path_dashes_to_underscores (p : Fpath.t) : Fpath.t =
  let segments = Fpath.segs p in
  let converted_segments =
    List.map (String.map (fun c -> if c = '-' then '_' else c)) segments
  in
  Fpath.v (String.concat Fpath.dir_sep converted_segments)

include Config_lib
include Logger

(* Override Logger.S and Make to use Common.t (= Config_lib.t) *)
module type S = sig
  val config : t
  val group : string
end

module Make (C : S) = Logger.Make (struct
  let config = C.config
  let group = C.group
end)

module Format = Format
