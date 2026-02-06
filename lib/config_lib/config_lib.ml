include Config_lib_

type t = {
  convert_names : convert_flag; [@default Enable]
      (** Control name conflict resolution and transformation.
          When enabled, applies [@sml.bad_name] attributes to names that conflict
          with OCaml keywords or conventions. *)
  convert_keywords : convert_flag; [@default Enable]
      (** Handle SML identifiers that conflict with OCaml keywords by appending
          underscores (e.g., method → method_). *)
  rename_types : convert_flag; [@default Enable]
      (** Transform type names to follow OCaml conventions (lowercase). *)
  make_make_functor : convert_flag; [@default Enable]
      (** Rename functors to follow OCaml's Make pattern convention. *)
  guess_pattern : convert_flag; [@default Enable]
      (** Use heuristics to classify pattern heads as constructors vs variables. *)
  curry_expressions : convert_flag; [@default Enable]
      (** Convert tuple-argument functions to curried form. *)
  curry_types : convert_flag; [@default Enable]
      (** Convert tuple-argument function TYPES to curried form.
          Transforms types like (int * string) -> result to int -> string -> result. *)
  toplevel_names : convert_flag; [@default Enable]
      (** Map SML Basis library names to OCaml equivalents
          (e.g., SOME → Some, NONE → None). *)
  verbosity : int; [@default 0]
      (** Logging verbosity level (0-3). Higher values produce more debug output. *)
  concat_output : bool; [@default true]
      (** Concatenate multiple input files into a single output file. *)
  force : bool; [@default false]
      (** Overwrite existing output files without prompting. *)
  quiet : bool; [@default false]
      (** Suppress all output except errors. *)
  guess_var : string option; [@default None]
      (** Regex pattern to identify variables that should be prefixed with __. *)
  debug : string list; [@default []]
      (** List of debug categories to enable (e.g., ["parser"; "backend"]). *)
  check_ocaml : bool; [@default false]
      (** Validate generated OCaml code using the OCaml compiler. *)
  dash_to_underscore : bool; [@default false]
      (** Convert dashes to underscores in filenames (e.g., foo-bar.ml → foo_bar.ml). *)
  input_file : source; [@default StdIn]
      (** Input source specification (file paths or stdin). *)
  output_file : target; [@default Silent]
      (** Output target specification (file path, stdout, or silent). *)
    remove_constructor_manifest : bool; [@default true]
      (** If true, do not generate a constructor manifest file. *)
}
[@@deriving make]

type arg = t -> t

let get : type a. a flag -> t -> a =
 fun flag cfg ->
  match flag with
  | Convert_names -> cfg.convert_names
  | Convert_keywords -> cfg.convert_keywords
  | Rename_types -> cfg.rename_types
  | Make_make_functor -> cfg.make_make_functor
  | Guess_pattern -> cfg.guess_pattern
  | Curry_expressions -> cfg.curry_expressions
  | Curry_types -> cfg.curry_types
  | Toplevel_names -> cfg.toplevel_names
  | Verbosity -> cfg.verbosity
  | Concat_output -> cfg.concat_output
  | Force -> cfg.force
  | Quiet -> cfg.quiet
  | Guess_var -> cfg.guess_var
  | Debug -> cfg.debug
  | Check_ocaml -> cfg.check_ocaml
  | Dash_to_underscore -> cfg.dash_to_underscore
  | Input_file -> cfg.input_file
  | Output_file -> cfg.output_file
  | Remove_constructor_manifest -> cfg.remove_constructor_manifest

let set : type a. a flag -> a -> arg =
 fun flag value ->
  match flag with
  | Convert_names -> fun cfg -> { cfg with convert_names = value }
  | Convert_keywords -> fun cfg -> { cfg with convert_keywords = value }
  | Rename_types -> fun cfg -> { cfg with rename_types = value }
  | Make_make_functor -> fun cfg -> { cfg with make_make_functor = value }
  | Guess_pattern -> fun cfg -> { cfg with guess_pattern = value }
  | Curry_expressions -> fun cfg -> { cfg with curry_expressions = value }
  | Curry_types -> fun cfg -> { cfg with curry_types = value }
  | Toplevel_names -> fun cfg -> { cfg with toplevel_names = value }
  | Verbosity -> fun cfg -> { cfg with verbosity = value }
  | Concat_output -> fun cfg -> { cfg with concat_output = value }
  | Force -> fun cfg -> { cfg with force = value }
  | Quiet -> fun cfg -> { cfg with quiet = value }
  | Guess_var -> fun cfg -> { cfg with guess_var = value }
  | Debug -> fun cfg -> { cfg with debug = value }
  | Check_ocaml -> fun cfg -> { cfg with check_ocaml = value }
  | Dash_to_underscore -> fun cfg -> { cfg with dash_to_underscore = value }
  | Input_file -> fun cfg -> { cfg with input_file = value }
  | Output_file -> fun cfg -> { cfg with output_file = value }
| Remove_constructor_manifest -> fun cfg -> { cfg with remove_constructor_manifest = value }
let create (args : arg list) : t =
  List.fold_left (fun cfg f -> f cfg) (make ()) args

module type CONFIG = sig
  val config : t
end
