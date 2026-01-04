(** Name store for tracking identifier contexts and transformations.

    This module provides a store for collecting and querying {!Name.t} values
    along with their syntactic context (type, constructor, value, etc.) and
    any associated transformation actions. The store is used during the
    conversion process to track all identifiers and determine how they should
    be renamed when translating from SML to OCaml.

    {2 Typical Usage}

    {[
      (* Create entries for discovered names *)
      let entries = [
        { name = my_name; action = Noted; context = Value };
        { name = type_name; action = Exact "t"; context = Type };
      ] in
      let store = Store.create entries in

      (* Query the store *)
      let types_only = Store.select store ~context:(fun c -> c = Type) in
      let entries_for_name = Store.get_name store my_name
    ]}
*)

(** The syntactic context in which a name appears.

    This is important because SML and OCaml have different naming conventions:
    - Types are lowercase in both languages
    - Constructors must be uppercase in OCaml
    - Structures (modules) must be uppercase in OCaml
    - Values should be lowercase in OCaml *)
type name_context =
  | Type        (** A type name (e.g., [int], [my_type]) *)
  | Constructor (** A data constructor (e.g., [Some], [NONE]) *)
  | Structure   (** A module/structure name (e.g., [List], [MyModule]) *)
  | Signature   (** A signature name (e.g., [ORD], [MONO_ARRAY]) *)
  | Value       (** A value binding (e.g., [x], [my_function]) *)

(** The action to take when processing this name during conversion.

    Actions control how names are transformed from SML conventions to OCaml. *)
type action =
  | Noted
    (** Simply note that this name exists with this context.
        No transformation is applied; used for tracking purposes. *)
  | Exact of string
    (** Replace with this exact string.
        Useful for hardcoded renamings (e.g., [NONE] → [None]). *)
  | Change of (Name.t -> Name.t)
    (** Apply a transformation function to compute the new name.
        Useful for systematic renamings like case conversion. *)
  | NoChange
    (** Explicitly preserve the original name unchanged.
        Different from [Noted] in that it's an active decision. *)

(** An entry in the name store, associating a name with its context and action. *)
type entry = {
  name : Name.t;           (** The qualified name *)
  action : action;         (** What transformation to apply *)
  context : name_context;  (** The syntactic context *)
}

(** The abstract type of a name store. *)
type t

(** [create entries] creates a new store from a list of entries.

    @param entries The list of name entries to store
    @return A new store containing all entries *)
val create : entry list -> t

(** [entries t] returns all entries in the store.

    @param t The store to query
    @return The list of all entries *)
val entries : t -> entry list

(** [select t ?name ?path ?package ?context] filters the store by predicates.

    All predicates default to accepting everything. When multiple predicates
    are provided, an entry must satisfy {i all} of them (logical AND).

    @param t The store to filter
    @param name Predicate on the root name string
    @param path Predicate on the module path
    @param package Predicate on the package specifier
    @param context Predicate on the syntactic context
    @return A new store containing only matching entries *)
val select :
  t ->
  ?name:(Name.name -> bool) ->
  ?path:(Name.path -> bool) ->
  ?package:(Name.package -> bool) ->
  ?context:(name_context -> bool) ->
  t

(** [combine t1 t2] merges two stores by concatenating their entries.

    @param t1 The first store
    @param t2 The second store
    @return A new store containing entries from both stores *)
val combine : t -> t -> t

(** [get_name t name] retrieves all entries for a specific qualified name.

    A name may appear multiple times with different contexts (e.g., a type
    and a value with the same name).

    @param t The store to search
    @param name The qualified name to look up
    @return A list of all entries matching the name *)
val get_name : t -> Name.t -> entry list
