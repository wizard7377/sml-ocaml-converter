

open Common

exception DirectoryExists 


class process_dir (dir : string) (options : options) = object
     val mutable dir : string = dir
     val mutable cfg : options = options
     method set_dir (d : string) : unit = dir <- d
     method setup_dir (d : string) : unit = 
       if Sys.file_exists d then 
         raise DirectoryExists
       else 
          Bos.OS.Path.move (Fpath.v dir) (Fpath.v d) |> ignore ;
          dir <- d 
         
     method copy_other_files (dest : string) : unit = assert false
     method process_sml (dest : string) : unit = assert false
end
