


let main input_file = let 
    res = Sml_ocaml_converter.sml_to_ocaml input_file 
in print_endline res
let () = main (Sys.argv.(1))
