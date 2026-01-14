exception Output_syntax_error of string

let check_output ~(config : Common.options) (input : string) : int =
  let module Log = Common.Make (struct
    let config = config
    let group = "process_common"
  end) in
  try
    Log.log ~level:Low ~kind:Neutral ~msg:"Checking output syntax..." ();
    let lexbuf = Lexing.from_string input in
    let p = Parse.use_file lexbuf in
    Log.log ~level:Medium ~kind:Positive ~msg:"Output syntax is valid OCaml." ();
    (* List.iter (Pprintast.toplevel_phrase Format.err_formatter) p; *)
    0
  with e ->
    Log.log ~level:Medium ~kind:Negative
      ~msg:(Printf.sprintf "Output syntax check failed.")
      ();
    1
