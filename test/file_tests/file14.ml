include Test_common
module TestWeaken : TEST_FILES = struct
  let test_name = {|Weaken - Functor with Local Declarations|}
  let input = {sml|
functor Weaken (structure Whnf : WHNF) : WEAKEN =
struct

  local
    structure I = IntSyn

    fun strengthenExp (U, s) = Whnf.normalize (Whnf.cloInv (U, s), I.id)

    fun strengthenDec (I.Dec (name, V), s) = I.Dec (name, strengthenExp (V, s))

    fun strengthenCtx (I.Null, s) = (I.Null, s)
      | strengthenCtx (I.Decl (G, D), s) =
        let
          val (G', s') = strengthenCtx (G, s)
        in
          (I.Decl (G', strengthenDec (D, s')), I.dot1 s')
        end

    fun strengthenSub (s, t) = Whnf.compInv (s, t)

    fun strengthenSpine (I.Nil, t) = I.Nil
      | strengthenSpine (I.App (U, S), t) = I.App (strengthenExp (U, t), strengthenSpine (S, t))

  in
    val strengthenExp = strengthenExp
    val strengthenSpine = strengthenSpine
    val strengthenDec = strengthenDec
    val strengthenCtx = strengthenCtx
    val strengthenSub = strengthenSub
  end
end
  |sml}
  let expected_output = None
end

module TestCase = Test_common.Make(TestWeaken)
