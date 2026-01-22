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
  deref_pattern : convert_flag;
  curry_expressions : convert_flag;
  curry_types : convert_flag;
  tuple_select : convert_flag;
  toplevel_names : convert_flag;
}

let mkConversions ?(convert_names = Disable) ?(convert_comments = Warn)
    ?(add_line_numbers = Disable) ?(convert_keywords = Warn)
    ?(rename_types = Warn) ?(make_make_functor = Disable)
    ?(rename_constructors = Disable) ?(guess_pattern = Disable)
    ?(deref_pattern = Disable) ?(curry_expressions = Disable)
    ?(curry_types = Disable) ?(tuple_select = Disable) ?(toplevel_names = Enable) () :
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
    deref_pattern;
    curry_expressions;
    curry_types;
    tuple_select;
    toplevel_names;
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
  check_ocaml : bool;
  variable_regex : string; 
  dash_to_underscore : bool;
}

let mkOptions ?(input_file = StdIn) ?(output_file = Silent) ?(verbosity = None)
    ?(conversions = mkConversions ()) ?(concat_output = true) ?(force = false)
    ?(quiet = false) ?(guess_var : string option = None) 
    ?(debug : string list = []) ?(check_ocaml = false) ?(variable_regex : string = "")  
    ?(dash_to_underscore : bool = false)
    (_ : unit) : options =
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
    check_ocaml;
    variable_regex;
    dash_to_underscore;
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
let get_check_ocaml opts = opts.check_ocaml
let get_variable_regex opts = opts.variable_regex
let get_deref_pattern opts = opts.conversions.deref_pattern
let get_curry_expressions opts = opts.conversions.curry_expressions
let get_curry_types opts = opts.conversions.curry_types
let get_tuple_select opts = opts.conversions.tuple_select
let get_toplevel_names opts = opts.conversions.toplevel_names
let get_dash_to_underscore opts = opts.dash_to_underscore
let engaged = function Enable | Warn -> true | _ -> false
let noted = function Warn | Note -> true | _ -> false

module type CONFIG = sig
  val config : options
  (** The configuration value to use for conversion. *)
end
