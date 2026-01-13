type source = File of string list | StdIn
type target = FileOut of string | StdOut | Silent
type conversions = { convert_names : bool; convert_comments : bool; add_line_numbers : bool; convert_keywords : bool; rename_types : bool }

let mkConversions ?(convert_names = true) ?(convert_comments = true) ?(add_line_numbers = true)
    ?(convert_keywords = true) ?(rename_types = true)
    (_ : unit) : conversions =
  { convert_names; convert_comments; add_line_numbers; convert_keywords; rename_types }
type options = {
  input_file : source;  (** Path to the input SML source file. *)
  output_file : target;
      (** Optional path to the output OCaml file. If [None], output is
          written to stdout. *)
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
      
}

let mkOptions 
  ?(input_file = StdIn) 
  ?(output_file = Silent) 
  ?(verbosity = None)
  ?(conversions = mkConversions ()) 
  ?(concat_output = true)
  ?(force = false)
  ?(quiet = true)

  (_ : unit) : options =
  { input_file; output_file; verbosity; conversions; concat_output; force; quiet }

let get_verbosity opts = opts.verbosity
let get_verbosity_default opts def =
  match opts.verbosity with
  | None -> def
  | Some v -> v
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
module type CONFIG = sig
  val config : options
  (** The configuration value to use for conversion. *)
end
