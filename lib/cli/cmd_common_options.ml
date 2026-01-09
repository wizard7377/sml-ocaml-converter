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

let mkConvert_flags names : Common.do_convert = {
    convert_names = names ;
}



let convert_flags : Common.do_convert Term.t = 
  let process_names_doc : Arg.info = Arg.info ["process-names"] ~doc:"Conversion options for names" in
  let process_names : Common.convert_flag Term.t = 
    process_convert_flag Common.Dont_convert [] (Arg.info ["names"] ~doc:"Convert names")
  in
    let+ names = process_names 
    in
      mkConvert_flags names

let common_options : common_options Cmdliner.Term.t =  
  let+ v = verb 
  and+ c = convert_flags in
    {
      verbose = Some v ;
      conversions = c ;
    }
  