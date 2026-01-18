(* Top level comment *)
fun f x =
  (* Function body comment *)
  x + 1

(* Between declarations *)
val y = (* inline comment *) 42

(* Multiple comments test *)
val z = if (* condition comment *) true
        then (* then comment *) 1
        else (* else comment *) 2
