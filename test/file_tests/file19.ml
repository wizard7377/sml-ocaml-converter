include Test_common
module TestFixity : TEST_FILES = struct
  let test_name = {|Fixity - Nested Datatypes and Functions|}
  let input = {sml|
structure Fixity :> FIXITY =
struct
  datatype associativity = Left | Right | None

  datatype precedence = Strength of int

  val maxPrecInt = 9999
  val maxPrec = Strength(maxPrecInt)
  val minPrecInt = 0
  val minPrec = Strength(minPrecInt)

  fun less (Strength(p), Strength(q)) = (p < q)
  fun leq (Strength(p), Strength(q)) = (p <= q)
  fun compare (Strength(p), Strength(q)) = Int.compare (p, q)

  fun inc (Strength(p)) = Strength(p+1)
  fun dec (Strength(p)) = Strength(p-1)

  datatype fixity =
      Nonfix
    | Infix of precedence * associativity
    | Prefix of precedence
    | Postfix of precedence

  fun prec (Infix(p,_)) = p
    | prec (Prefix(p)) = p
    | prec (Postfix(p)) = p
    | prec (Nonfix) = inc (maxPrec)

end
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestFixity)
