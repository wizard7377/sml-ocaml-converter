(** Name processing for SML to OCaml conversion.
    
    This module provides contextual name transformation, handling:
    - Keyword escaping (SML identifiers that are OCaml keywords)
    - Capitalization adjustments (constructors, types, values)
    - Scoped name tracking for let bindings and modules
    - SML basis library constructor mapping (SOME -> Some, etc.)
    
    Uses {!Capital_utils} for low-level capitalization functions. *)

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
let process_lowercase = Capital_utils.process_lowercase
let process_uppercase = Capital_utils.process_uppercase
let process_caps = Capital_utils.process_caps
let is_lowercase = Capital_utils.is_variable_identifier

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
    let config = Common.make ()
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
      Capital_utils.is_operator_name s
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


