include Config_lib_
type t = {
  convert_names : convert_flag; [@default Enable]
  convert_comments : convert_flag; [@default Enable]
  add_line_numbers : convert_flag; [@default Enable]
  convert_keywords : convert_flag; [@default Enable]
  rename_types : convert_flag; [@default Enable]
  make_make_functor : convert_flag; [@default Enable]
  rename_constructors : convert_flag; [@default Enable]
  guess_pattern : convert_flag; [@default Enable]
  deref_pattern : convert_flag; [@default Enable]
  curry_expressions : convert_flag; [@default Enable]
  curry_types : convert_flag; [@default Enable]
  tuple_select : convert_flag;  [@default Enable]
  toplevel_names : convert_flag;  [@default Enable]
  verbosity : int; [@default 0]
  concat_output : bool; [@default true]
  force : bool; [@default false]
  quiet : bool; [@default false]
  guess_var : string option; [@default None]
  debug : string list; [@default []]
  check_ocaml : bool; [@default false]
  variable_regex : string; [@default ""]
  dash_to_underscore : bool; [@default false]
  basis_shim : string list; [@default []]
      (** List of basis libraries to include during conversion. *)
  input_file : source; [@default StdIn]
      (** Path to the input SML source file. *)
  output_file : target; [@default Silent]
      (** Optional path to the output OCaml file. If [None], output is written
          to stdout. *)
} [@@deriving make]

type arg = t -> t
 
let get : type a . a flag -> t -> a = fun flag cfg -> match flag with
  | Convert_names -> cfg.convert_names
  | Convert_comments -> cfg.convert_comments
  | Add_line_numbers -> cfg.add_line_numbers
  | Convert_keywords -> cfg.convert_keywords
  | Rename_types -> cfg.rename_types
  | Make_make_functor -> cfg.make_make_functor
  | Rename_constructors -> cfg.rename_constructors
  | Guess_pattern -> cfg.guess_pattern
  | Deref_pattern -> cfg.deref_pattern
  | Curry_expressions -> cfg.curry_expressions
  | Curry_types -> cfg.curry_types
  | Tuple_select -> cfg.tuple_select
  | Toplevel_names -> cfg.toplevel_names
  | Verbosity -> cfg.verbosity
  | Concat_output -> cfg.concat_output
  | Force -> cfg.force
  | Quiet -> cfg.quiet
  | Guess_var -> cfg.guess_var
  | Debug -> cfg.debug
  | Check_ocaml -> cfg.check_ocaml
  | Variable_regex -> cfg.variable_regex
  | Dash_to_underscore -> cfg.dash_to_underscore
  | Basis_shim -> cfg.basis_shim
  | Input_file -> cfg.input_file
  | Output_file -> cfg.output_file
let set : type a . a flag -> a -> arg = fun flag value -> match flag with
  | Convert_names -> fun cfg -> { cfg with convert_names = value }
  | Convert_comments -> fun cfg -> { cfg with convert_comments = value }
  | Add_line_numbers -> fun cfg -> { cfg with add_line_numbers = value }
  | Convert_keywords -> fun cfg -> { cfg with convert_keywords = value }
  | Rename_types -> fun cfg -> { cfg with rename_types = value }
  | Make_make_functor -> fun cfg -> { cfg with make_make_functor = value }
  | Rename_constructors -> fun cfg -> { cfg with rename_constructors = value }
  | Guess_pattern -> fun cfg -> { cfg with guess_pattern = value }
  | Deref_pattern -> fun cfg -> { cfg with deref_pattern = value }
  | Curry_expressions -> fun cfg -> { cfg with curry_expressions = value }
  | Curry_types -> fun cfg -> { cfg with curry_types = value }
  | Tuple_select -> fun cfg -> { cfg with tuple_select = value }
  | Toplevel_names -> fun cfg -> { cfg with toplevel_names = value }
  | Verbosity -> fun cfg -> { cfg with verbosity = value }
  | Concat_output -> fun cfg -> { cfg with concat_output = value }
  | Force -> fun cfg -> { cfg with force = value }
  | Quiet -> fun cfg -> { cfg with quiet = value }
  | Guess_var -> fun cfg -> { cfg with guess_var = value }
  | Debug -> fun cfg -> { cfg with debug = value }
  | Check_ocaml -> fun cfg -> { cfg with check_ocaml = value }
  | Variable_regex -> fun cfg -> { cfg with variable_regex = value }
  | Dash_to_underscore -> fun cfg -> { cfg with dash_to_underscore = value }
  | Basis_shim -> fun cfg -> { cfg with basis_shim = value }
  | Input_file -> fun cfg -> { cfg with input_file = value }
  | Output_file -> fun cfg -> { cfg with output_file = value }
let create (args : arg list) : t = List.fold_left (fun cfg f -> f cfg) (make ()) args

module type CONFIG = sig
  val config : t
end