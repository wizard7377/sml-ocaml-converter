type convert_flag = Enable | Warn | Note | Disable
type source = File of string list | StdIn
type target = FileOut of string | StdOut | Silent

type _ flag = 
  | Convert_names : convert_flag flag
  | Convert_comments : convert_flag flag
  | Add_line_numbers : convert_flag flag
  | Convert_keywords : convert_flag flag
  | Rename_types : convert_flag flag
  | Make_make_functor : convert_flag flag
  | Rename_constructors : convert_flag flag
  | Guess_pattern : convert_flag flag
  | Deref_pattern : convert_flag flag
  | Curry_expressions : convert_flag flag
  | Curry_types : convert_flag flag
  | Tuple_select : convert_flag flag
  | Toplevel_names : convert_flag flag
  | Verbosity : int flag
  | Concat_output : bool flag
  | Force : bool flag
  | Quiet : bool flag
  | Guess_var : string option flag
  | Debug : string list flag
  | Check_ocaml : bool flag
  | Variable_regex : string flag
  | Dash_to_underscore : bool flag
  | Basis_shim : string list flag
  | Input_file : source flag
  | Output_file : target flag

(** Check if a conversion flag is active (not Disable). *)
let is_flag_enabled = function Enable | Warn | Note -> true | Disable -> false

(** [engaged flag] returns true if the conversion flag is Enable or Warn. *)
let engaged = function Enable | Warn -> true | _ -> false

(** [noted flag] returns true if the conversion flag is Warn or Note. *)
let noted = function Warn | Note -> true | _ -> false 