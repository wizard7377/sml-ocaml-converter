

open Common


exception DirectoryExists 


class process_dir 
  : string -> options -> object
       method set_dir : string -> unit
       method setup_dir : string -> unit 
       method copy_other_files : string -> unit
       method process_sml : string -> unit
  end
