open Cmdliner
open Cmdliner.Term.Syntax
include Cmd_common_options
include Process

module type Command_S = sig
  val run_cmd : int Cmd.t
end

module Convert_File : Command_S = struct
  let output : string option Term.t =
    let doc = {|
  Output path for converted files
  |} in
    Arg.(
      value
      & opt (some string) None
      & info [ "o"; "output" ] ~doc ~docv:"OUTPUT")

  let input : string list Term.t =
    let doc =
      {|
  Input path(s) to Standard ML source file(s).
  Multiple files can be specified by separating them with spaces.
  It is *highly reccomended* to provide these in the order `%.sig %.fun %.sml`, as this will ensure that names are properly ordered. 
  |}
    in
    Arg.(non_empty (pos_all string [] & info [] ~doc ~docv:"INPUT"))

  let run_cmd : int Cmd.t =
    let doc =
      {|
  Convert a (group of) Standard ML source file(s) to OCaml. 
  Works by firstly processing the SML files into an internal representation, applying a series of transformations, and then pretty-printing the result as OCaml code.
  This is the currently only conversion offered here
  |}
    in
    Cmd.v (Cmd.info "file" ~doc ~docs:"SML Converter")
    @@ let+ output = output
       and+ input = input
       and+ common_options = common_options in
       Toplevel.convert_file ~options:common_options
         ?output_file:(Option.map Toplevel.string_to_path output)
         ~input_files:(List.map Toplevel.string_to_path input)
end

module Group_Convert : Command_S = struct
  let output_dir : string Term.t =
    let doc = {|
  Output directory for converted OCaml files.
  |} in
    Arg.(
      required
      & opt (some string) None
      & info [ "output" ] ~doc ~docv:"OUTPUT_DIR")

  let input_dir : string Term.t =
    let doc =
      {|
  Input directory containing Standard ML source files.
  All SML source files in this directory (and its subdirectories) will be processed.
  |}
    in
    Arg.(
      required
      & opt (some string) None
      & info [ "input" ] ~doc ~docv:"INPUT_DIR")

  let run_cmd : int Cmd.t =
    let doc =
      {|
  Convert a group of Standard ML source files in a directory to OCaml.
  This command processes all SML source files within the specified input directory, converting them to OCaml and saving the results in the specified output directory.
  |}
    in
    Cmd.v (Cmd.info "group" ~doc ~docs:"SML Converter")
    @@ let+ output_dir = output_dir
       and+ input_dir = input_dir
       and+ common_options = common_options in
       Toplevel.convert_group ~options:common_options
         ~output_dir:(Toplevel.string_to_path output_dir)
         ~input_dir:(Toplevel.string_to_path input_dir)
end

let cmd_convert_file : int Cmd.t = Convert_File.run_cmd
let cmd_convert_group : int Cmd.t = Group_Convert.run_cmd
