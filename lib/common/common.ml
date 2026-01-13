
let get_file filename = Result.get_ok @@ Bos.OS.File.read @@ Fpath.v filename
let write_file filename content = () (* TODO *)

type ident = Name of string | Symbol of string
type convert_flag = Dont_convert | Do_convert | Debug_convert
type do_convert = { convert_names : convert_flag }

type config = {
  input_file : string;
  output_file : string option;
  verbosity : int option;
  conversions : do_convert;
}

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

let higher : int option -> int -> bool -> bool = fun x y def ->
  match x with
  | None -> def
  | Some v -> v > y
let lower : int option -> int -> bool -> bool = fun x y def ->
  match x with
  | None -> def
  | Some v -> v < y
include Options
include Logger