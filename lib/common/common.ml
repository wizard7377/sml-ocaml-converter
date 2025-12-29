open Ez_file
let get_file filename = FileString.read_file filename
let write_file filename content = () (* TODO *)

type ident = Name of string | Symbol of string 