(* Test: mixed precedence levels *)
(* Expected: (1 + (2 * 3)) = 4 *)

val test1 = 1 + 2 * 3 = 7
val test2 = 2 * 3 + 1 = 7
val test3 = 1 < 2 + 3
