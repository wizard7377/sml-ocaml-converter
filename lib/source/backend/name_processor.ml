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
  val config : Common.options
  val context : Context.t
end

(** Functor to create a name processor with specific configuration *)
module Make (Config : CONFIG) = struct
  open Process_names
  
  let namer : process_names =
    new process_names (ref Config.config) (ref Config.context)

  (** Convert name parts to a Longident.t with context-aware processing *)
  let to_longident ~(ctx : context) (name_parts : string list) : Ppxlib.Longident.t =
    let (res, _changed) = namer#process_name ~ctx ~name:name_parts in
    res

  (** Convert name parts to a string (last component only) *)
  let to_string ~(ctx : context) (name_parts : string list) : string =
    Ppxlib.Longident.last_exn (namer#process_name ~ctx ~name:name_parts |> fst)

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
  let in_core_lang (ctx : context) : bool = match ctx with
    | ModuleValue | ModuleType | Functor -> false
    | _ -> true
  (** Process a name and return both the result and whether it was changed *)
  let process_name ~(ctx : context) (name : string list) : Ppxlib.Longident.t * bool =
    match name with 
    | [ name ] when in_core_lang ctx -> 
        let regex = Common.get_variable_regex Config.config in 
        (* prerr_endline ("Checking variable regex: " ^ regex ^ " against name: " ^ name); *)
        if Re.Str.string_match (Re.Str.regexp ("$" ^ regex ^ "^") ) name 0 then 
          (Longident.Lident (String.cat "__" name)) , true
        else 
          namer#process_name ~ctx ~name:[name]
    | _ -> namer#process_name ~ctx ~name

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
    let regex = Common.get_variable_regex Config.config in 
        (* prerr_endline ("Checking variable regex: " ^ regex ^ " against name: " ^ name); *)
    Re.Str.string_match (Re.Str.regexp ("$" ^ regex ^ "^") ) name 0  
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
