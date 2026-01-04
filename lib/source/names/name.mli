(** Qualified name representation for SML identifiers.

    This module provides the core type for representing fully-qualified names
    in the SML-to-OCaml converter. Names consist of three components:
    - A {b package} indicating visibility/scope
    - A {b path} representing the module hierarchy
    - A {b root} which is the actual identifier name

    {2 Example}

    A name like [Foo.Bar.baz] in a private package would be represented as:
    {[
      make_name ~package:(Private "mylib") ~path:["Foo"; "Bar"] ~root:"baz"
    ]}
*)

(** The package specifier indicates where a name originates and its visibility.

    - [Global] - A built-in or pervasive identifier
    - [Private s] - An internal name within package [s] (not exported)
    - [Public s] - An exported name from package [s]
    - [Local s] - A locally-scoped name within [s] (e.g., let-bound) *)
type package = Global | Private of string | Public of string | Local of string

(** The module path leading to the name, represented as a list of module names.
    For example, the path for [Foo.Bar.baz] would be [["Foo"; "Bar"]]. *)
type path = string list

(** The base identifier name (the rightmost component of a qualified name). *)
type name = string

(** The abstract type representing a fully-qualified name.
    Use {!make_name} to construct and {!parse_name} to destructure. *)
type t

(** [make_name ~package ~path ~root] constructs a qualified name.

    @param package The package/visibility specifier
    @param path The module path leading to the identifier
    @param root The base identifier name
    @return A new qualified name value *)
val make_name : package:package -> path:path -> root:string -> t

(** [parse_name t] destructures a qualified name into its components.

    @param t The qualified name to parse
    @return A tuple of [(package, path, name)] *)
val parse_name : t -> (package * path * name)

(** [map_name ~f t] applies a transformation function to the root name.

    This is useful for name mangling operations like adding prefixes/suffixes
    or converting between naming conventions.

    @param f The transformation function to apply to the root name
    @param t The qualified name to transform
    @return A new qualified name with the transformed root *)
val map_name : f:(name -> name) -> t -> t

(** [name_to_string ?qualified t] converts a name to its string representation.

    @param qualified If [true], includes the full module path (e.g., ["Foo.Bar.baz"]).
                     If [false] (default), returns only the root name (e.g., ["baz"]).
    @param t The qualified name to convert
    @return The string representation of the name *)
val name_to_string : ?qualified:bool -> t -> string
