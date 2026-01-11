type context = ..
type context += PatternHead
type context += PatternTail
type context += Value
type context += Type
type context += ModuleValue
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

class process_names (config : Common.options ref) (store : Context.t ref) =
  object (self)
    val store : Context.t ref = store
    val config : Common.options ref = config

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

    method process_name ?(ctx : context = Empty) ~(name : string list) =
      assert (List.length name > 0);
      assert (not @@ String.starts_with "(" @@ List.nth name 0);
      assert (not @@ String.ends_with ")" @@ List.nth name (List.length name - 1));

      if Common.get_convert_names (Common.get_conversions !config) then
        assert false
      else self#build_longident name
  end
