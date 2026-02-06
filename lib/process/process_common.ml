(** Exception raised when generated OCaml code has syntax errors. *)
exception Output_syntax_error of string

(** Result type for syntax validation. *)
type check_result = Good | Bad of Syntaxerr.error | Err of exn

(** Convert a syntax error to a formatted string message. *)
let print_error ~(err : Syntaxerr.error) : string =
  let buffer = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buffer in
  Location.report_exception fmt (Syntaxerr.Error err);
  Format.pp_print_flush fmt ();
  Buffer.contents buffer

(** Display error context with line numbers around the error location.
    Shows [context] lines before and after the error position.

    @param context Number of context lines to show (default: 4)
    @param file_content Source file split into lines
    @param p0 Start position of error
    @param p1 End position of error
    @return Formatted string with line numbers and error marker *)
let display_error_context_lines ?(context = 4) (file_content : string list)
    (p0 : Lexing.position) (p1 : Lexing.position) =
  let start_line = max 1 (p0.pos_lnum - context) in
  let max_line = List.length file_content - 1 in
  let end_line = min max_line (p1.pos_lnum + context) in
  let contents =
    List.filteri (fun i _ -> i >= start_line && i <= end_line) file_content
  in
  let contents0 =
    List.mapi
      (fun i line ->
        let line_no = i + start_line + 1 in
        if line_no = p0.pos_lnum then Printf.sprintf ">> %4d | %s" line_no line
        else Printf.sprintf "   %4d | %s" line_no line)
      contents
  in
  String.concat "\n" contents0

(** Display error context by reading from a file.
    Convenience wrapper around {!display_error_context_lines}.

    @param context Number of context lines (default: 4)
    @param filename Path to source file
    @param p0 Start position
    @param p1 End position
    @return Formatted error context *)
let display_error_context ?(context = 4) (filename : string)
    (p0 : Lexing.position) (p1 : Lexing.position) =
  let path' = Filename.current_dir_name ^ "/" ^ filename in
  let file_content =
    try Bos.OS.File.read_lines (Fpath.v path') |> Result.get_ok with _ -> []
  in
  display_error_context_lines ~context file_content p0 p1

(** Display error context from a string instead of a file.
    Used for showing errors in generated code that hasn't been written to disk.

    @param context Number of context lines (default: 4)
    @param _filename Filename (for API compatibility, not used)
    @param content Source code as string
    @param p0 Start position
    @param p1 End position
    @return Formatted error context *)
let display_error_context_from_string ?(context = 4) (_filename : string)
    (content : string) (p0 : Lexing.position) (p1 : Lexing.position) =
  let file_content = String.split_on_char '\n' content in
  display_error_context_lines ~context file_content p0 p1

(** Extract output file path from configuration for error messages.
    Returns a descriptive string if not writing to a file. *)
let get_output_file_path (config : Common.t) : string =
  match Common.get Output_file config with
  | Common.FileOut path -> path
  | Common.StdOut -> "standard output"
  | Common.Silent -> "silent output"

(** Validate OCaml syntax by parsing the generated code.

    Uses the OCaml compiler's parser to detect syntax errors in generated code.
    If errors are found, logs detailed error messages with source context.

    @param config Conversion configuration
    @param output_file Path for error reporting (defaults from config)
    @param input Generated OCaml source code
    @return Validation result *)
let check_output ~(config : Common.t)
    ?(output_file = get_output_file_path config) (input : string) : check_result
    =
  let module Log = Common.Make (struct
    let config = config
    let group = "process_common"
  end) in
  try
    Log.log ~level:Low ~kind:Neutral ~msg:"Checking output syntax..." ();
    let () = Printexc.record_backtrace true in
    let lexbuf = Lexing.from_string ~with_positions:true input in
    Lexing.set_filename lexbuf output_file;

    let () = Printexc.record_backtrace false in
    let p = Ppxlib.Parse.use_file lexbuf in
    Log.log ~level:Medium ~kind:Positive ~msg:"Output syntax is valid OCaml." ();
    (* List.iter (Pprintast.toplevel_phrase Format.err_formatter) p; *)
    Good
  with
  | Syntaxerr.Error e ->
      let source_files =
        match Common.get Input_file config with
        | Common.File path -> String.concat " , " path
        | Common.StdIn -> "standard input"
      in
      let loc = Syntaxerr.location_of_error e in
      (* Show context from the generated output, using the actual output_file path *)
      let file_content =
        display_error_context_from_string output_file input loc.loc_start
          loc.loc_end
      in
      let msg =
        Printf.sprintf "Source: %s\nGenerated: %s\n%s\n\n%s" source_files
          output_file (print_error ~err:e) file_content
      in
      Log.log ~level:High ~kind:Warning ~msg ();
      Bad e
  | Lexer.Error (_e, warn) ->
      let source_files =
        match Common.get Input_file config with
        | Common.File path -> String.concat " , " path
        | Common.StdIn -> "standard input"
      in
      let msg =
        Printf.sprintf "Source: %s\nGenerated: %s\nLexer error: %s" source_files
          output_file
          (Location.print_loc Format.str_formatter warn;
           Format.flush_str_formatter ())
      in
      Log.log ~level:High ~kind:Warning ~msg ();
      Bad (Syntaxerr.Other warn)
  | e ->
      let msg = "Unknown error during syntax check of output OCaml code." in
      (* Log.log ~level:High ~kind:Negative ~msg:msg (); *)
      Err e
