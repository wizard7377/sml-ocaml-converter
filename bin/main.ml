
open Sml_ocaml_converter

let rec convert (file : string list) = List.iter process_file file
and process_file (file : string) : unit = print_endline ("processing " ^ file) ; let 
    file_contents = Common.get_file file in 
    let ast = sml_to_ocaml file_contents in
    print_endline ast 





let verbose = ref false

let input_files : string list ref = ref []

let output_file = ref ""

let anon_fun filename =
  input_files := filename :: !input_files

let speclist =
  [("-verbose", Arg.Set verbose, "Output debug information");
   ("-o", Arg.Set_string output_file, "Set output file name")]


let main () = 
    Arg.parse speclist anon_fun "Usage: sml_ocaml_converter [options] file1.sml file2.sml ..." ;
    convert !input_files
let () = main ()
