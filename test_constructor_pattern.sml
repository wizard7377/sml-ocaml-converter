datatype tree = Leaf | Node of int * tree * tree

fun sum t =
  case t of
    Leaf => 0
  | Node(x, l, r) => x + sum l + sum r
