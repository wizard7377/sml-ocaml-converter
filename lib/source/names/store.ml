(** Implementation of the name store.

    See {!Store} for the public interface documentation. *)

include Name

type name_context =
  | Type
  | Constructor
  | Structure
  | Signature
  | Value

type action =
  | Noted        (** Note that this has this context *)
  | Exact of string (** Change to this exact name *)
  | Change of (Name.t -> Name.t) (** Change to this name *)
  | NoChange     (** Do not change this name *)

type entry = {
  name : Name.t;
  action : action;
  context : name_context;
}

(** Internal representation: a simple list of entries.
    For larger stores, a map keyed by name would be more efficient. *)
type t = {
  entries : entry list;
}

let create x = { entries = x }

let entries t = t.entries

(** Default predicate that accepts everything. *)
let simple_true _ = true

let select t ?(name = simple_true) ?(path = simple_true) ?(package = simple_true) ?(context = simple_true) =
  let parse_name n =
    let (pkg, pth, nm) = Name.parse_name n in
    (pkg, pth, nm)
  in
  let filtered_entries = List.filter (fun e ->
    let (pkg, pth, nm) = parse_name e.name in
    name nm && path pth && package pkg && context e.context
  ) t.entries in
  { entries = filtered_entries }

let combine t1 t2 =
  { entries = t1.entries @ t2.entries }

let get_name (store : t) (name : Name.t) : entry list =
  List.filter (fun e -> e.name = name) store.entries
