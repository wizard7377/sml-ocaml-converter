(** Name transformation for SML to OCaml constructor conversion.

    SML allows constructor names to start with lowercase letters, while OCaml
    requires them to start with uppercase. This module provides transformation
    functions to convert SML constructor names to valid OCaml names while
    avoiding conflicts with existing identifiers.

    Transformation rules:
    - Lowercase constructors: append "_" and capitalize (ok -> Ok_, some -> Some_)
    - Already uppercase: preserve as-is (Foo -> Foo, Bar -> Bar)
    - All caps: convert to proper case (SOME -> Some, NONE -> None)
    - Trailing underscore: add extra underscore to avoid collision (B_ -> B__)

    Examples:
    {[
      transform_constructor "ok"     = "Ok_"
      transform_constructor "some"   = "Some_"
      transform_constructor "Foo"    = "Foo"
      transform_constructor "SOME"   = "Some"
      transform_constructor "B_"     = "B__"
    ]}
*)

val transform_constructor : string -> string
(** Transform a constructor name to valid OCaml format.

    See module documentation for detailed transformation rules and examples. *)

val transform_to_lowercase : string -> string
(** Transform a variable name to lowercase:
    - SOME -> some_
    - Foo -> foo_
    - bar -> bar (no change) *)
