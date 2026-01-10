(* Test file for comment preservation *)

(* Simple value declaration with comment *)
val x = 42

(* Function with inline comment *)
fun add x y = (* returns sum *) x + y

(* Multi-line comment before function
   This function demonstrates
   comment preservation *)
fun multiply a b = a * b

(* Datatype with comments *)
datatype tree =
    Empty (* leaf node *)
  | Node of int * tree * tree (* branch with value and children *)

(* Pattern matching with comments *)
fun size t =
  case t of
    Empty => (* base case *) 0
  | Node (v, left, right) =>
      (* recursive case *)
      1 + size left + size right

(* Let expression with comments *)
val result =
  let
    (* local binding *)
    val temp = 10
  in
    (* body *)
    temp * 2
  end

(* Record with field comments *)
val point = {
  x = (* x coordinate *) 5,
  y = (* y coordinate *) 10
}

(* Exception with comment *)
exception (* custom error *) InvalidInput of string

(* Nested comments are not standard SML but single level *)
fun test n =
  if n > 0 then (* positive *) n
  else (* non-positive *) 0
