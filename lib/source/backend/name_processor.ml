(** Centralized name processing for SML to OCaml conversion.
    
    This module provides a unified API for all name transformation needs,
    wrapping {!Process_names} with convenience functions for common operations
    used throughout the backend.
    
    Key features:
    - Context-aware name transformation (Value, Type, Constructor, etc.)
    - Longident and string conversion helpers
    - SML operator prefix handling (op +, op *, etc.)
    - Integration with {!Idx_utils} for AST identifier handling *)

module type CONFIG = sig
  val config : Common.t
  val context : Context.t
end

(** Functor to create a name processor with specific configuration *)
module Make (Config : CONFIG) = struct
  open Process_names

  (* 
  eqtype unit 	General
eqtype int 	Int
eqtype word 	Word
type real 	Real
eqtype char 	Char
eqtype string 	String
type substring 	Substring
type exn 	General
eqtype 'a array 	Array
eqtype 'a vector 	Vector
eqtype 'a ref 	primitive
datatype bool = false | true 	primitive
datatype 'a option = NONE | SOME of 'a 	Option
datatype order = LESS | EQUAL | GREATER 	General
datatype 'a list = nil | :: of ('a * 'a list) 	primitive
val ! : 'a ref -> 'a 	General.!
val := : 'a ref * 'a -> unit 	General.:=
val @ : ('a list * 'a list) -> 'a list 	List.@
val ^ : string * string -> string 	String.^
val app : ('a -> unit) -> 'a list -> unit 	List.app
val before : 'a * unit -> 'a 	General.before
val ceil : real -> int 	Real.ceil
val chr : int -> char 	Char.chr
val concat : string list -> string 	String.concat
val exnMessage : exn -> string 	General.exnMessage
val exnName : exn -> string 	General.exnName
val explode : string -> char list 	String.explode
val floor : real -> int 	Real.floor
val foldl : ('a*'b->'b)-> 'b -> 'a list -> 'b 	List.foldl
val foldr : ('a*'b->'b)-> 'b -> 'a list -> 'b 	List.foldr
val getOpt : ('a option * 'a) -> 'a 	Option.getOpt
val hd : 'a list -> 'a 	List.hd
val ignore : 'a -> unit 	General.ignore
val implode : char list -> string 	String.implode
val isSome : 'a option -> bool 	Option.isSome
val length : 'a list -> int 	List.length
val map : ('a -> 'b) -> 'a list -> 'b list 	List.map
val not : bool -> bool 	Bool.not
val null : 'a list -> bool 	List.null
val o : ('a->'b) * ('c->'a) -> 'c->'b 	General.o
val ord : char -> int 	Char.ord
val print : string -> unit 	TextIO.print
val real : int -> real 	Real.fromInt
val ref : 'a -> 'a ref 	primitive
val rev : 'a list -> 'a list 	List.rev
val round : real -> int 	Real.round
val size : string -> int 	String.size
val str : char -> string 	String.str
val substring : string * int * int -> string 	String.substring
val tl : 'a list -> 'a list 	List.tl
val trunc : real -> int 	Real.trunc
val use : string -> unit 	implementation dependent
val valOf : 'a option -> 'a 	Option.valOf
val vector : 'a list -> 'a vector 	Vector.fromList

  *)
  let process_special (name : string list) : string list option = 
    if not @@ Common.engaged @@ Common.get Toplevel_names Config.config then 
      None 
    else 
      match name with 
      | [ "true" ] -> Some [ "true" ]
      | [ "false" ] -> Some [ "false" ]
      | [ "nil" ] -> Some [ "[]" ]
      | [ "::" ] -> Some [ "::" ]
      | [ "SOME" ] -> Some [ "Some" ]
      | [ "NONE" ] -> Some [ "None" ]
      | [ "ref" ] -> Some [ "ref" ]
      | [ "hd" ] -> Some [ "List" ; "hd" ]
      | _ -> None 
  let namer : process_names =
    new process_names (ref Config.config) (ref Config.context)

  let in_core_lang (ctx : context) : bool = match ctx with
    | ModuleValue | ModuleType | Functor -> false
    | _ -> true

  (** Process a name and return both the result and whether it was changed *)
  let process_name ~(ctx : context) (name : string list) : Ppxlib.Longident.t * bool =
    let name = if List.exists (fun s -> String.ends_with s ~suffix:"_") name then List.map (fun s -> s ^ "__") name else name in
    begin match process_special name with
      | Some res -> Option.get (Longident.unflatten res), true
      | None ->
    match name with
    | [ name ] when in_core_lang ctx ->
        (match Common.get Guess_var Config.config with
        | Some regex ->

            (* Check if the variable name matches the regex pattern *)

            if Re.Str.string_match (Re.Str.regexp ("^" ^ regex ^ "$")) name 0 then
              let newname = String.uncapitalize_ascii name ^ "_" in
              (Longident.Lident newname, true)
            else
              namer#process_name ~ctx ~name:[name]
        | None -> namer#process_name ~ctx ~name:[name])
    | _ -> namer#process_name ~ctx ~name
            end

  (** Convert name parts to a string (last component only) *)
  let to_string ~(ctx : context) (name_parts : string list) : string =
    Ppxlib.Longident.last_exn (process_name ~ctx name_parts |> fst)

  (** Convert name parts to a Longident.t with context-aware processing *)
  let to_longident ~(ctx : context) (name_parts : string list) : Ppxlib.Longident.t =
    let (res, _changed) = process_name ~ctx name_parts in
    res

  (** Convert an AST idx to a Longident.t with context-aware processing *)
  let idx_to_longident ~(ctx : context) (idx : Ast.idx) : Ppxlib.Longident.t =
    to_longident ~ctx (Idx_utils.idx_to_name idx)

  (** Convert an AST idx to a string (last component only) *)
  let idx_to_string ~(ctx : context) (idx : Ast.idx) : string =
    to_string ~ctx (Idx_utils.idx_to_name idx)

  (** Process SML with_op wrapper (handles op prefix for operators).
      The op keyword in SML removes infix status: [op +] is prefix [+]. *)
  let process_with_op ~(ctx : context) (wo : Ast.with_op) : string =
    match wo with
    | WithOp id -> idx_to_string ~ctx id.value
    | WithoutOp id -> idx_to_string ~ctx id.value

  (** Check if a name is valid for the given context (e.g., constructors uppercase) *)
  let is_valid ~(ctx : context) (name : string list) : bool =
    namer#is_good ~ctx ~name
  (** Push a new naming context scope *)
  let push_context () : note = namer#push_context ()

  (** Pop a naming context scope *)
  let pop_context (n : note) : unit = namer#pop_context n

  (** Add a name mapping in the current scope *)
  let add_name ?(global = false) ~from ~res () : unit =
    namer#add_name ~global ~from ~res ()

  (** Get the processed name for a given input *)
  let get_name (from : string) : string = namer#get_name from

  let matches_pattern (name : string) : bool =
    match Common.get Guess_var Config.config with
    | Some regex ->
        Re.Str.string_match (Re.Str.regexp ("^" ^ regex ^ "$")) name 0
    | None -> false  
end

(** Re-export context type and constructors for convenience *)
type context = Process_names.context
let context_value = Process_names.Value
let context_type = Process_names.Type
let context_constructor = Process_names.Constructor
let context_operator = Process_names.Operator
let context_label = Process_names.Label
let context_module_value = Process_names.ModuleValue
let context_module_type = Process_names.ModuleType
let context_functor = Process_names.Functor
let context_pattern_head = Process_names.PatternHead
let context_pattern_tail = Process_names.PatternTail
let context_empty = Process_names.Empty
