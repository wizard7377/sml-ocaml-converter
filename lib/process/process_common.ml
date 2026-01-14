exception Output_syntax_error of string
let check_output ~(config:Common.options) (input:string) : int =
  try 
    Common.log ~cfg:config ~level:Low ~kind:Neutral ~msg:"Checking output syntax..." ();
    let lexbuf = Lexing.from_string input in
    let p = Parse.use_file lexbuf in
    Common.log ~cfg:config ~level:Medium ~kind:Positive ~msg:"Output syntax is valid OCaml." ();
    (* List.iter (Pprintast.toplevel_phrase Format.err_formatter) p; *)
    0 
    with 
    e ->
        Common.log ~cfg:config ~level:Medium ~kind:Negative
        ~msg:(Printf.sprintf "Output syntax check failed.") ();
        1
