exception Output_syntax_error of string
type check_result = Good | Bad of Syntaxerr.error | Err of exn
let print_error ~(err : Syntaxerr.error) : string =
  let buffer = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buffer in
  Location.report_exception fmt (Syntaxerr.Error err);
  Format.pp_print_flush fmt ();
  Buffer.contents buffer

let check_output ~(config : Common.options) (input : string) : check_result =
  let module Log = Common.Make (struct
    let config = config
    let group = "process_common"
  end) in
  try
    Log.log ~level:Low ~kind:Neutral ~msg:"Checking output syntax..." ();
    let () = Printexc.record_backtrace true in
    let lexbuf = Lexing.from_string ~with_positions:true input in
    let p = Parse.use_file lexbuf in
    let () = Printexc.record_backtrace false in
    Log.log ~level:Medium ~kind:Positive ~msg:"Output syntax is valid OCaml." ();
    (* List.iter (Pprintast.toplevel_phrase Format.err_formatter) p; *)
    Good
  with 
    Syntaxerr.Error e ->
    let file = match Common.get_input_file config with
      | Common.File path -> String.concat " , " path ^ " "
      | Common.StdIn -> "standard input" in
    let msg = file ^ print_error ~err:e in
    Log.log ~level:High ~kind:Warning ~msg:msg
      ();
      Bad ( e)
    | Lexer.Error (e, warn) -> 
      let file = match Common.get_input_file config with
        | Common.File path -> String.concat " , " path ^ " "
        | Common.StdIn -> "standard input" in
        let msg = "Error in file " ^ file in
      Log.log ~level:High ~kind:Warning ~msg:msg ();
      Bad ( Syntaxerr.Other warn)
    | e -> 
      let msg = "Unknown error during syntax check of output OCaml code." in
      (* Log.log ~level:High ~kind:Negative ~msg:msg (); *)
      Err e
    
