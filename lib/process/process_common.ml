exception Output_syntax_error of string

type check_result = Good | Bad of Syntaxerr.error | Err of exn

let print_error ~(err : Syntaxerr.error) : string =
  let buffer = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buffer in
  Location.report_exception fmt (Syntaxerr.Error err);
  Format.pp_print_flush fmt ();
  Buffer.contents buffer

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

let display_error_context ?(context = 4) (filename : string)
    (p0 : Lexing.position) (p1 : Lexing.position) =
  let path' = Filename.current_dir_name ^ "/" ^ filename in
  let file_content =
    try Bos.OS.File.read_lines (Fpath.v path') |> Result.get_ok with _ -> []
  in
  display_error_context_lines ~context file_content p0 p1

let display_error_context_from_string ?(context = 4) (_filename : string)
    (content : string) (p0 : Lexing.position) (p1 : Lexing.position) =
  let file_content = String.split_on_char '\n' content in
  display_error_context_lines ~context file_content p0 p1

let get_output_file (config : Common.options) : string =
  match Common.get_output_file config with
  | Common.FileOut path -> path
  | Common.StdOut -> "standard output"
  | Common.Silent -> "silent output"

let check_output ~(config : Common.options)
    ?(output_file = get_output_file config) (input : string) : check_result =
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
        match Common.get_input_file config with
        | Common.File path -> String.concat " , " path
        | Common.StdIn -> "standard input"
      in
      let loc = Syntaxerr.location_of_error e in
      (* Show context from the generated output, using the actual output_file path *)
      let file_content =
        display_error_context_from_string output_file input loc.loc_start loc.loc_end
      in
      let msg = Printf.sprintf "Source: %s\nGenerated: %s\n%s\n\n%s"
        source_files
        output_file
        (print_error ~err:e)
        file_content
      in
      Log.log ~level:High ~kind:Warning ~msg ();
      Bad e
  | Lexer.Error (_e, warn) ->
      let source_files =
        match Common.get_input_file config with
        | Common.File path -> String.concat " , " path
        | Common.StdIn -> "standard input"
      in
      let msg = Printf.sprintf "Source: %s\nGenerated: %s\nLexer error: %s"
        source_files
        output_file
        (Location.print_loc Format.str_formatter warn; Format.flush_str_formatter ())
      in
      Log.log ~level:High ~kind:Warning ~msg ();
      Bad (Syntaxerr.Other warn)
  | e ->
      let msg = "Unknown error during syntax check of output OCaml code." in
      (* Log.log ~level:High ~kind:Negative ~msg:msg (); *)
      Err e
