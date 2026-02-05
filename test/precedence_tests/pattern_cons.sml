(* Test: cons pattern with right-associativity *)

fun length [] = 0
  | length (x :: xs) = 1 + length xs
