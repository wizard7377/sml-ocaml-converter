(* Test: multiplication has higher precedence than addition *)
(* Expected: 1 + (2 * 3) = 7, not (1 + 2) * 3 = 9 *)

structure Test = struct
  val result = 1 + 2 * 3
end

