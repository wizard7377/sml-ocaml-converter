open Cmdliner 
open Cmdliner.Term.Syntax
open Cmdliner.Term
type common_options = {
    verbose : int option ;
    conversions : Common.do_convert ;
}

let convert_flag_option : Common.convert_flag Arg.conv = Arg.enum [("do", Common.Do_convert); ("dont", Common.Dont_convert); ("debug", Common.Debug_convert)]
let process_convert_flag (default_value:Common.convert_flag) (flags:string list) (desc:Arg.info) : Common.convert_flag Term.t =  

    Arg.(value & opt convert_flag_option default_value desc)
let verb : int Term.t =
  let doc = "Verbosity level" in
  Arg.(value & opt int 0 & info ["v"; "verbose"] ~doc)

let mkConvert_flags patterns constructors functions uncurry_types uncurry_values : Common.do_convert = {
    pattern_names = patterns ;
    constructor_names_values = constructors ;
    function_names = functions ;
    uncurry_types = uncurry_types ;
    uncurry_values = uncurry_values ;
}
let convert_flags : Common.do_convert Term.t = 
  let process_names_doc : Arg.info = Arg.info ["process-names"] ~doc:"Conversion options for names" in
  let process_patterns : Common.convert_flag Term.t = 
    process_convert_flag Common.Dont_convert [] (Arg.info ["patterns"] ~doc:"Convert pattern names")
  in let 
  process_constructors : Common.convert_flag Term.t = 
    process_convert_flag Common.Dont_convert [] (Arg.info ["constructors"] ~doc:"Convert constructor and value names")
  in let
  process_functions : Common.convert_flag Term.t = 
    process_convert_flag Common.Dont_convert [] (Arg.info ["functions"] ~doc:"Convert function names")
  in let
  process_uncurry_types : Common.convert_flag Term.t =
    process_convert_flag Common.Dont_convert [] (Arg.info ["uncurry-types"] ~doc:"Uncurry type expressions")
  in let
  process_uncurry_values : Common.convert_flag Term.t =
    process_convert_flag Common.Dont_convert [] (Arg.info ["uncurry-values"] ~doc:"Uncurry value expressions")
  in
    let+ patterns = process_patterns
    and+ constructors = process_constructors
    and+ functions = process_functions
    and+ uncurry_types = process_uncurry_types
    and+ uncurry_values = process_uncurry_values
    in
      mkConvert_flags patterns constructors functions uncurry_types uncurry_values

let common_options : common_options Cmdliner.Term.t =  
  let+ v = verb 
  and+ c = convert_flags in
    {
      verbose = Some v ;
      conversions = c ;
    }
  