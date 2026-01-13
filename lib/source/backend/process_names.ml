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

open! Ppxlib

type is_constructor = YesItIs of int | NoItsNot

let process_lowercase (s : string) : string = String.uncapitalize_ascii s
let process_uppercase (s : string) : string = String.capitalize_ascii s
let process_caps (s : string) : string = String.uppercase_ascii s
let is_lowercase (s : string) : bool = try let fst_char = String.get s 0 in (fst_char >= 'a' && fst_char <= 'z') || fst_char = '_' with Invalid_argument _ -> false
class process_names (config : Common.options ref) (store : Context.t ref) =
  object (self)
    val store : Context.t ref = store
    val config : Common.options ref = config
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
      String.length s > 0
      &&
      let c = String.get s 0 in
      not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')
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
    method process_name ?(ctx : context = Empty) ~(name : string list) =
      assert (List.length name > 0);
      assert (not @@ String.starts_with "(" @@ List.nth name 0);
      assert (not @@ String.ends_with ")" @@ List.nth name (List.length name - 1));

      let (res, b) = 
        match ctx with
        | Type when Common.get_rename_types !config -> (
            let rec process_parts parts =
              match parts with
              | [] -> []
              | [ last ] -> if is_lowercase last then  [last] else [ "__" ^ last ]
              | first :: rest -> first :: process_parts rest
            in
            let new_name = process_parts name in
            (new_name, true)
          )
        | Functor when Common.get_make_make_functor !config -> (
            let rec process_parts parts =
              match parts with
              | [] -> []
              | [ last ] -> [ "Make_" ^ last ]
              | first :: rest -> first :: process_parts rest
            in
            let new_name = process_parts name in
            (new_name, true)
          )
        | _ -> (name, false)
          in 
      let (scope, basename) = self#split_name res in
      if Ppxlib.Keyword.is_keyword basename && Common.get_convert_keywords !config then
        let new_basename = basename ^ "__" in
        let full_name = scope @ [ new_basename ] in
        (self#build_longident full_name, b)
      else
        (self#build_longident res, b)
  end
