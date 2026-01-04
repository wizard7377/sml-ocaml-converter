(** Top-level module for the names library.

    This module re-exports the name handling submodules for convenient access.
    Import this module to get access to all name-related functionality:

    {[
      open Names

      (* Now you can use: *)
      let n = Name.make_name ~package:Global ~path:[] ~root:"foo" in
      let store = Store.create [{ name = n; action = Noted; context = Value }]
    ]}

    {2 Module Overview}

    - {!module:Name} - Core type for qualified names with package/path/root
    - {!module:Store} - Collection of names with contexts and transformation actions
*)

(** Qualified name representation.
    See {!Name} for full documentation. *)
module Name = Name

(** Name store for tracking and querying names.
    See {!Store} for full documentation. *)
module Store = Store
