

 
type common_options = {
    verbose : int option ;
    conversions : Common.do_convert ;
}

val common_options : common_options Cmdliner.Term.t