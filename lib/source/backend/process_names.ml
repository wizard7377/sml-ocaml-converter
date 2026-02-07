(** Name processing for SML to OCaml conversion.
    
    This module provides contextual name transformation, handling:
    - Keyword escaping (SML identifiers that are OCaml keywords)
    - Capitalization adjustments (constructors, types, values)
    - Scoped name tracking for let bindings and modules
    - SML basis library constructor mapping (SOME -> Some, etc.)
    
    Uses {!Backend_utils} for low-level capitalization functions. *)

include Ast

type context = ..
type context += PatternHead
type context += PatternTail
type context += Value
type context += Type
type context += ModuleValue
type context += Functor
type context += ModuleType
type context += Label
type context += Constructor
type context += Operator
type context += Empty

let show_context (ctx : context) : string =
  match ctx with
  | PatternHead -> "PatternHead"
  | PatternTail -> "PatternTail"
  | Value -> "Value"
  | Type -> "Type"
  | ModuleValue -> "ModuleValue"
  | Functor -> "Functor"
  | ModuleType -> "ModuleType"
  | Label -> "Label"
  | Constructor -> "Constructor"
  | Operator -> "Operator"
  | Empty -> "Empty"
  | _ -> "Unknown"

open! Ppxlib
module StringMap = Map.Make(String)

(* Re-export capitalization utilities for convenience *)
let process_lowercase = Backend_utils.process_lowercase
let process_uppercase = Backend_utils.process_uppercase
let process_caps = Backend_utils.process_caps
let is_lowercase = Backend_utils.is_variable_identifier

type is_constructor = YesItIs of int | NoItsNot
type note = int
type scope = string StringMap.t Stack.t

let get_scope_level (input:string) (name:string option) (scope:string StringMap.t) : string option = 
  match name with
  | Some n -> Some n 
  | None -> StringMap.find_opt input scope

let last (lst:'a list) : 'a =
  assert (List.length lst > 0);
  List.nth lst (List.length lst - 1)

let rec map_last (f : 'a -> 'a) (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | [ x ] -> [ f x ]
  | first :: rest -> first :: map_last f rest

let rec get_in_scope (scope:scope) (name:string) : string option =
  Stack.fold (get_scope_level name) None scope

module Log = Common.Make (struct 
    let config = Common.create []
    let group = "process_names"
  end)
class process_names (config : Common.t ref) (store : Context.t ref) =
  object (self)
    val store : Context.t ref = store
    val config : Common.t ref = config
    val mutable current_depth : int = 0 
    val mutable context_stack : string StringMap.t Stack.t = Stack.create () 
    val mutable global_map : string StringMap.t = StringMap.empty
    method private guess_matches (n : string) : bool =
      match Common.get Guess_var !config with
      | Some pattern ->
          let regex = Re.Str.regexp pattern in
          Re.Str.string_match regex n 0
      | None -> false
    method push_context () : note =
      let depth = current_depth + 1 in
      current_depth <- depth ;
      Stack.push StringMap.empty context_stack;
      depth
    method pop_context (n : note) : unit =
      assert (n == current_depth) ;
      assert (not (Stack.is_empty context_stack)) ;
      ignore (Stack.pop context_stack) ;
      current_depth <- current_depth - 1 
    method add_name ?(global=false) ~(from : string) ~(res : string) () : unit =
      if global then begin 
      assert (not (Stack.is_empty context_stack)) ;
      let current_map = Stack.pop context_stack in
      let updated_map = StringMap.add from res current_map in
      Stack.push updated_map context_stack
      end else begin
        global_map <- StringMap.add from res global_map ; 
        ()
      end
    method get_name (from : string) : string =
      match get_in_scope context_stack from with
      | Some name -> name
      | None -> from
    method private split_name (s : string list) : string list * string = 
      let rec aux parts =
        match parts with
        | [] -> ([], "")
        | [ last ] -> ([], last)
        | first :: rest ->
            let (init, last) = aux rest in
            (first :: init, last)
      in
      aux s
    method private is_operator (s : string) : bool =
      Backend_utils.is_operator_name s
    (** Check if a string is an operator (non-alphanumeric identifier) *)

    method private build_longident (parts : string list) : Longident.t =
      match parts with
      | [] -> failwith "empty name"
      | [ x ] -> Longident.Lident x
      | first :: rest ->
          List.fold_left
            (fun acc part -> Longident.Ldot (acc, part))
            (Longident.Lident first) rest
    (** Build a Longident from name parts without using Longident.parse
      (which incorrectly wraps operators in parentheses).
      ["A"; "B"; "C"] becomes Ldot(Ldot(Lident "A", "B"), "C") *)
    method is_good ?(ctx : context = Empty) ~(name : string list) : bool =
      assert (List.length name > 0);
      assert (not @@ String.starts_with "(" @@ List.nth name 0);
      assert (not @@ String.ends_with ")" @@ List.nth name (List.length name - 1));
      let res = match ctx with
      | Type -> (
          let rec check_parts parts =
            match parts with
            | [] -> true
            | [ last ] -> last = process_lowercase last
            | _ :: rest -> check_parts rest
          in
          check_parts name
        )
      | Constructor -> (
          let rec check_parts parts =
            match parts with
            | [] -> true
            | [ last ] -> last = process_uppercase last || last = process_caps last
            | _ :: rest -> check_parts rest
          in
          check_parts name
        )
      | _ -> true 

      in
      res
    method private process_op (input : string) : string =
      let is_op = String.starts_with "op" input && (if String.length input > 2 then
          let c = String.get input 2 in
          not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
        else false) in
      let res0 = if is_op then String.sub input 2 (String.length input - 2) else input in 
      let res1 = match res0 with
        | "~" -> "~-"
        | _ -> res0 in
      res1

    method process_name ?(ctx : context = Empty)  ~(name : string list) =
      assert (List.length name > 0);
      assert (not @@ String.starts_with "(" @@ List.nth name 0);
      assert (not @@ String.ends_with ")" @@ List.nth name (List.length name - 1));
      let sanitize_part (s : string) : string =
        let buf = Buffer.create (String.length s) in
        String.iter
          (fun c ->
            match c with
            | '`' -> Buffer.add_string buf "_bq"
            | '\'' -> Buffer.add_string buf "_prime"
            | _ -> Buffer.add_char buf c)
          s;
        Buffer.contents buf
      in
      let sanitized = List.map sanitize_part name in
      let without_op = map_last (fun s -> self#process_op s) sanitized in 
      let name'' = map_last (fun s -> self#get_name s) without_op in
      let name' = match name'' with
        | [ name''' ] -> if StringMap.mem name''' global_map then [ StringMap.find name''' global_map ] else [ name''' ]
        | name''' -> name''' in
      let (res, b) =
        (match ctx with
        | Type when Common.is_flag_enabled (Common.get Rename_types !config) -> (
            let rec process_parts parts =
              match parts with
              | [] -> []
              | [ last ] -> if is_lowercase last then  [last] else [ String.uncapitalize_ascii last ^ "_" ]
              | first :: rest -> first :: process_parts rest
            in
            let new_name = process_parts name' in
            (new_name, true)
          )
        | Functor when Common.is_flag_enabled (Common.get Make_make_functor !config) -> (
            let rec process_parts parts =
              match parts with
              | [] -> []
              | [ last ] -> [ "Make_" ^ last ]
              | first :: rest -> first :: process_parts rest
            in
            let new_name = process_parts name' in
            (new_name, true)
          )
        | Functor -> (name', false)
        | PatternHead when Common.is_flag_enabled (Common.get Guess_pattern !config) -> let res = map_last process_uppercase name' in (res, name' <> res)
        | PatternTail when Common.is_flag_enabled (Common.get Convert_names !config) -> begin match name' with
            | [ last ] -> let res = process_lowercase last in ( [ res ], last <> res)
            | _ -> (name', false)
          end
        | Value when Common.is_flag_enabled (Common.get Convert_names !config) -> begin match name' with
            | [ last ] -> let res = process_lowercase last in ( [ res ], last <> res)
            | _ -> (name', false)
          end
        | Constructor when Common.is_flag_enabled (Common.get Convert_names !config) ->
            (* Map SML basis constructors to OCaml equivalents *)
            let mapped_name = match name' with
              | ["SOME"] -> ["Some"]
              | ["NONE"] -> ["None"]
              | ["true"] -> ["true"]
              | ["false"] -> ["false"]
              | ["nil"] -> ["[]"]
              | ["LESS"] -> ["Less"]
              | ["EQUAL"] -> ["Equal"]
              | ["GREATER"] -> ["Greater"]
              | _ -> name'
            in
            let res = map_last process_uppercase mapped_name in
            (res, name' <> res)
            | Operator -> begin 
              let name0 = match name' with
                | [ "~" ] -> [ "~- "]
                | _ -> name' in
              let res = name0 in
              (res, name' <> res)
            end
        | _ -> (name', false)
        )
          in 
      let (scope, basename) = self#split_name res in
      let (res0, res1) = (if Ppxlib.Keyword.is_keyword basename && Common.is_flag_enabled (Common.get Convert_keywords !config) then
        let new_basename = basename ^ "_" in
        let full_name = scope @ [ new_basename ] in
        (self#build_longident full_name, b)
      else
        (self#build_longident res, b)) 
      in 
      if ((last name) != (Ppxlib.Longident.last_exn res0)) then Log.log_with  ~cfg:!config ~level:Low ~kind:Neutral ~msg:(Printf.sprintf "From %s, Processed name: %s in context %s" (String.concat "." name) (Ppxlib.Longident.name res0) (show_context ctx)) ();
      (res0, res1)
    end

(** {1 Name Processor Functor}

    Unified API wrapping process_names with convenience functions. *)

module type CONFIG = sig
  val config : Common.t
  val context : Context.t
end

module Make (Config : CONFIG) = struct
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

  let build_longident_from_list (parts : string list) : Ppxlib.Longident.t =
    match parts with
    | [] -> failwith "empty name"
    | [ x ] -> Ppxlib.Longident.Lident x
    | first :: rest ->
        List.fold_left
          (fun acc part -> Ppxlib.Longident.Ldot (acc, part))
          (Ppxlib.Longident.Lident first) rest

  let process_name ~(ctx : context) (name : string list) : Ppxlib.Longident.t * bool =
    let name = if List.exists (fun s -> String.ends_with s ~suffix:"_") name then List.map (fun s -> s ^ "__") name else name in
    begin match process_special name with
      | Some res -> build_longident_from_list res, true
      | None ->
    match name with
    | [ name ] when in_core_lang ctx ->
        (match Common.get Guess_var Config.config with
        | Some regex ->
            if Re.Str.string_match (Re.Str.regexp ("^" ^ regex ^ "$")) name 0 then
              let newname = String.uncapitalize_ascii name ^ "_" in
              (Longident.Lident newname, true)
            else
              namer#process_name ~ctx ~name:[name]
        | None -> namer#process_name ~ctx ~name:[name])
    | _ -> namer#process_name ~ctx ~name
            end

  let to_string ~(ctx : context) (name_parts : string list) : string =
    Ppxlib.Longident.last_exn (process_name ~ctx name_parts |> fst)

  let to_longident ~(ctx : context) (name_parts : string list) : Ppxlib.Longident.t =
    let (res, _changed) = process_name ~ctx name_parts in
    res

  let idx_to_longident ~(ctx : context) (idx : idx) : Ppxlib.Longident.t =
    to_longident ~ctx (Backend_utils.idx_to_name idx)

  let idx_to_string ~(ctx : context) (idx : idx) : string =
    to_string ~ctx (Backend_utils.idx_to_name idx)

  let process_with_op ~(ctx : context) (wo : with_op) : string =
    match wo with
    | WithOp id -> idx_to_string ~ctx id.value
    | WithoutOp id -> idx_to_string ~ctx id.value

  let is_valid ~(ctx : context) (name : string list) : bool =
    namer#is_good ~ctx ~name

  let push_context () : note = namer#push_context ()
  let pop_context (n : note) : unit = namer#pop_context n

  let add_name ?(global = false) ~from ~res () : unit =
    namer#add_name ~global ~from ~res ()

  let get_name (from : string) : string = namer#get_name from

  let matches_pattern (name : string) : bool =
    match Common.get Guess_var Config.config with
    | Some regex ->
        Re.Str.string_match (Re.Str.regexp ("^" ^ regex ^ "$")) name 0
    | None -> false  
end

(* Context aliases for convenience *)
let context_value = Value
let context_type = Type
let context_constructor = Constructor
let context_operator = Operator
let context_label = Label
let context_module_value = ModuleValue
let context_module_type = ModuleType
let context_functor = Functor
let context_pattern_head = PatternHead
let context_pattern_tail = PatternTail
let context_empty = Empty
