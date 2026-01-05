(** Name resolution and transformation processing.

    {1 Synopsis}

    This module provides the name processing pipeline used during SML-to-OCaml
    conversion. It handles name resolution, scope management, and application
    of transformation rules from the name store.

    {1 Overview}

    The {!process_names} class provides high-level operations for:
    - Resolving unqualified names in context
    - Managing local bindings and scope
    - Opening and including modules
    - Converting qualified names to OCaml identifier lists

    {1 Name Resolution}

    Name resolution considers:
    - The current package context
    - Local bindings introduced by [let] and patterns
    - Opened modules
    - The global name store

    Names are resolved in order of precedence:
    1. Local bindings
    2. Names from opened modules
    3. Global store

    {1 Usage Example}

    {[
      let store = (* ... populated name store ... *) in
      let processor = new process_names Name.Global store in

      (* Process a simple name reference *)
      let resolved = processor#process_name ([], "map") in

      (* Process with local binding *)
      let result = processor#local_bind ~ctx:[Value]
        (Name.make_name ~package:Global ~path:[] ~root:"x")
        Store.NoChange
        (fun x_name ->
          (* x_name is bound in this scope *)
          processor#process_name ([], "x")
        )
      in

      (* Convert to OCaml identifier format *)
      let ocaml_id = processor#name_to_idx resolved
    ]}

    {1 Exceptions} *)

(** Raised when name resolution fails to find a binding.

    This indicates that a name was used but not defined or imported.
    The exception carries the unresolved name for error reporting. *)
exception Unbound_name of Names.Name.t

(** Raised when name resolution finds multiple candidates.

    This indicates ambiguity in name resolution, typically when a name
    is imported from multiple opened modules. The exception carries
    the ambiguous name and the list of candidate names. *)
exception Ambiguous_name of Names.Name.t * Names.Name.t list

(** Name processing class.

    This class provides stateful name resolution and transformation
    operations. It maintains:
    - Current package context
    - Opened modules
    - Local bindings
    - The global name store

    {3 Constructor Parameters}

    The constructor takes two parameters:
    @param package The package context for resolution (Global, Private, etc.)
    @param store The name store containing all discovered identifiers *)
class process_names : Names.Name.package -> Names.Store.t -> object

  (** {1 Module Operations} *)

  (** [open_module name] makes a module's contents available unqualified.

      After opening a module, its exported names can be referenced
      without qualification. Multiple modules can be opened, with
      later openings taking precedence.

      Example:
      {[
        processor#open_module (make_name ~package:Global ~path:[] ~root:"List")
      ]}

      @param name The fully-qualified module name to open
      @return The opened module name (for tracking) *)
  method open_module : Names.Name.t -> Names.Name.t

  (** [include_module name] includes a module's contents into the current scope.

      Similar to {!open_module}, but typically used in signature/structure
      contexts to merge module contents.

      Example: [processor#include_module sig_name]

      @param name The fully-qualified module name to include
      @return The included module name *)
  method include_module : Names.Name.t -> Names.Name.t

  (** {1 Scoped Bindings} *)

  (** [local_bind ~ctx name action f] creates a local binding for a name.

      The binding is active only within the execution of [f]. This is
      used for:
      - Let-bound variables
      - Pattern bindings
      - Function parameters

      The method is polymorphic in the return type to allow arbitrary
      computations in the binding scope.

      Example:
      {[
        processor#local_bind
          (make_name ~package:Local "f" ~path:[] ~root:"x")
          NoChange
          (fun x ->
            (* x is bound here *)
            process_expression body
          )
      ]}

      @param ctx Optional list of contexts where this binding is valid
                 (e.g., [[Value]] for a value binding)
      @param name The name to bind locally
      @param action The transformation action for this binding
      @param f Continuation function receiving the bound name
      @return The result of applying [f] to the bound name *)
  method local_bind : 'a . ?ctx:Names.Store.name_context list -> Names.Name.t -> Names.Store.action -> (Names.Name.t -> 'a) -> 'a

  (** {1 Name Resolution} *)

  (** [process_name ~ctx (path, name)] resolves a name reference.

      This is the primary name resolution method. It:
      - Searches local bindings first
      - Then searches opened modules
      - Finally searches the global store
      - Applies any transformation actions
      - Raises {!Unbound_name} if not found
      - Raises {!Ambiguous_name} if multiple candidates exist

      Example: [processor#process_name ~ctx:[Value] (["List"], "map")]

      @param ctx Optional list of expected contexts for disambiguation
                 (e.g., [[Constructor]] when matching in a pattern)
      @param path Module path prefix (e.g., [["List"]] for [List.map])
      @param name Root identifier name (e.g., ["map"])
      @return The resolved and transformed qualified name
      @raise Unbound_name if the name cannot be resolved
      @raise Ambiguous_name if multiple candidates are found *)
  method process_name : ?ctx:Names.Store.name_context list -> Names.Name.path * Names.Name.name -> Names.Name.t

  (** {1 Name Conversion} *)

  (** [name_to_idx name] converts a qualified name to OCaml identifier format.

      Produces a list of strings suitable for building OCaml long
      identifiers (Longident.t). For example:
      - [Foo.Bar.baz] → [["Foo"; "Bar"; "baz"]]
      - [map] → [["map"]]

      @param name The qualified name to convert
      @return List of identifier components for OCaml *)
  method name_to_idx : Names.Name.t -> string list

end
