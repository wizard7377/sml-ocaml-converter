(** Implementation of the name store.

    See {!Store} for the public interface documentation. *)

include Name

type name_context =
  | Type
  | Constructor
  | Structure
  | Signature
  | Value
  | Variable 

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

let select query t = List.partition query t.entries |> fun (matching, non_matching) -> ({ entries = matching }, { entries = non_matching })

let combine t1 t2 =
  { entries = t1.entries @ t2.entries }

let get_name (store : t) (name : string) : entry list = let 
  query : entry -> bool = (fun e -> (let (_, _, root) = Name.parse_name e.name in root = name)) in 
  let (matching, _) = select query store in
  matching.entries
