type context =
  | TypeVar
  | TypeConst
  | Value
  | Structure
  | Signature 
  | PatternHead
  | PatternTail
  | Empty

module type PROCESS_NAMES = sig
  module Config : Common.CONFIG
  type t
  (** The type of collections of processed names.
      This should include both names that should and {e should not} be changed
          *) 

  val init : unit -> t

  val add_name: string -> string -> t -> t
  val get_name : t -> string -> string option
  val process_name : ?ctx:context -> t -> string -> string * t
end

module MapString = Map.Make(struct
    type t = string
    let compare = String.compare
end)

let is_lowercase (s : string) : bool =
    match s with
    | "" -> false
    | _ -> 
        let c = String.get s 0 in
        Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z'
let is_uppercase (s : string) : bool =
    match s with
    | "" -> false
    | _ -> 
        let c = String.get s 0 in
        Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z'
let ocaml_reserved_names = ["and";"as";"assert";"asr";"begin";"class ";"constraint";"do";"done";"downto";"else";"end ";"exception";"external";"false";"for";"fun";"function ";"functor";"if";"in";"include";"inherit";"initializer ";"land";"lazy";"let";"lor";"lsl";"lsr ";"lxor";"match";"method";"mod";"module";"mutable ";"new";"nonrec";"object";"of";"open";"or ";"private";"rec";"sig";"struct";"then";"to ";"true";"try";"type";"val";"virtual";"when ";"while";"with" ]
module Make (C : Common.CONFIG) : PROCESS_NAMES = struct
    module Config = C
    type t = string MapString.t
    
    let init () : t = MapString.empty
    let add_name (original : string) (new_name : string) (names : t) : t = 
        MapString.add original new_name names
    let get_name (names : t) (original : string) : string option = 
        MapString.find_opt original names
    let process_name ?(ctx=Empty) (names : t) (original : string) = match get_name names original with
        | Some new_name -> (new_name, names)
        | None -> match ctx with
                  | TypeVar when is_uppercase original ->
                        let new_name = String.uncapitalize_ascii original in
                        let names' = add_name original new_name names in
                        (new_name, names')
                  | TypeConst when is_uppercase original ->
                        let new_name = String.uncapitalize_ascii original  in
                        let names' = add_name original new_name names in
                        (new_name, names') 
                  | Structure when is_lowercase original ->
                        let new_name = String.capitalize_ascii original in
                        let names' = add_name original new_name names in
                        (new_name, names')
                  | Signature when is_lowercase original ->
                        let new_name = String.capitalize_ascii original in
                        let names' = add_name original new_name names in
                        (new_name, names')
                  | PatternHead when is_lowercase original ->
                     let new_name = String.capitalize_ascii original in
                     let names' = add_name original new_name names in
                        (new_name, names')
                  | _ -> (original, names)
                     
            
end 
