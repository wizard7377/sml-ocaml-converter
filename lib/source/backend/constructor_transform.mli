(** Name transformation for SML to OCaml constructor conversion *)

val transform_constructor : string -> string
(** Transform a constructor name to valid OCaml format:
    - Lowercase: a -> A_, some -> Some_
    - Uppercase with trailing _: B_ -> B__
    - Already valid: Foo -> Foo, SOME -> Some *)

val transform_to_lowercase : string -> string
(** Transform a variable name to lowercase:
    - SOME -> some_
    - Foo -> foo_
    - bar -> bar (no change) *)
