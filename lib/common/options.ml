type source = File of string list | StdIn
type target = FileOut of string | StdOut | Silent
type convert_flag = Enable | Warn | Note | Disable

(** Check if a conversion flag is active (not Disable). *)
let is_flag_enabled = function Enable | Warn | Note -> true | Disable -> false

type conversions = {
  convert_names : convert_flag;
  convert_comments : convert_flag;
  add_line_numbers : convert_flag;
  convert_keywords : convert_flag;
  rename_types : convert_flag;
  make_make_functor : convert_flag;
  rename_constructors : convert_flag;
  guess_pattern : convert_flag;
}

let mkConversions ?(convert_names = Disable) ?(convert_comments = Warn)
    ?(add_line_numbers = Disable) ?(convert_keywords = Warn)
    ?(rename_types = Warn) ?(make_make_functor = Warn)
    ?(rename_constructors = Note) ?(guess_pattern = Warn) (_ : unit) :
    conversions =
  {
    convert_names;
    convert_comments;
    add_line_numbers;
    convert_keywords;
    rename_types;
    make_make_functor;
    rename_constructors;
    guess_pattern;
  }

type options = {
  input_file : source;  (** Path to the input SML source file. *)
  output_file : target;
      (** Optional path to the output OCaml file. If [None], output is written
          to stdout. *)
  verbosity : int option;
      (** Optional verbosity level:
          - [None] or [Some 0]: Minimal output (errors only)
          - [Some 1]: Normal output (warnings and progress)
          - [Some 2]: Verbose output (detailed transformation info)
          - [Some 3+]: Debug output (all intermediate representations) *)
  conversions : conversions;
      (** Transformation policy specifying which conversions to apply. *)
  concat_output : bool;
  force : bool;
  quiet : bool;
  guess_var : string option;
  debug : string list;
}

let mkOptions ?(input_file = StdIn) ?(output_file = Silent) ?(verbosity = None)
    ?(conversions = mkConversions ()) ?(concat_output = true) ?(force = false)
    ?(quiet = false) ?(guess_var : string option = None)
    ?(debug : string list = []) (_ : unit) : options =
  {
    input_file;
    output_file;
    verbosity;
    conversions;
    concat_output;
    force;
    quiet;
    guess_var;
    debug;
  }

let get_verbosity opts = opts.verbosity

let get_verbosity_default opts def =
  match opts.verbosity with None -> def | Some v -> v

let get_input_file opts = opts.input_file
let get_output_file opts = opts.output_file
let get_conversions opts = opts.conversions
let get_convert_names opts = opts.conversions.convert_names
let get_convert_comments opts = opts.conversions.convert_comments
let get_line_numbers opts = opts.conversions.add_line_numbers
let get_concat_output opts = opts.concat_output
let get_force opts = opts.force
let get_quiet opts = opts.quiet
let get_convert_keywords opts = opts.conversions.convert_keywords
let get_rename_types opts = opts.conversions.rename_types
let get_make_make_functor opts = opts.conversions.make_make_functor
let get_rename_constructors opts = opts.conversions.rename_constructors
let get_guess_pattern opts = opts.conversions.guess_pattern
let get_guess_var opts = opts.guess_var
let get_debug opts = opts.debug
let engaged = function Enable | Warn -> true | _ -> false
let noted = function Warn | Note -> true | _ -> false

module type CONFIG = sig
  val config : options
  (** The configuration value to use for conversion. *)
end
