type check_result = Good | Bad of Syntaxerr.error | Err of exn

val check_output :
  config:Common.t -> ?output_file:string -> string -> check_result
