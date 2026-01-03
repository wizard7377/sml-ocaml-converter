include Test_common
module TestFiles : TEST_FILES = struct
  let test_name = {|TEST 1|}
  let input = {sml| 

    structure I = IntSyn

    fun cidFromHead (I.Const c) = c
      | cidFromHead (I.Def c) = c


    val indexArray : (IntSyn.Head Queue.queue) Array.array =
        Array.array (Global.maxCid + 1, Queue.empty)


    fun reset () = Array.modify (fn _ => Queue.empty) indexArray


    fun update (a, c) =
        Array.update (indexArray, a,
                      Queue.insert (c, Array.sub (indexArray, a)))


    fun install fromCS (H as I.Const c) =
        (case (fromCS, I.sgnLookup (c))
           of (_, I.ConDec (_, _, _, _, A, I.Type)) => update (cidFromHead (I.targetHead A), H)
            | (I.Clause, I.ConDef (_, _, _, _, A, I.Type, _)) => (update (cidFromHead (I.targetHead A), I.Def(c)))
            | _ => ())

    fun remove (a, cid) =
        (case Queue.deleteEnd (Array.sub (indexArray, a))
           of NONE => ()
            | SOME (c as I.Const cid', queue') =>
                if cid = cid' then Array.update (indexArray, a, queue')
                else ())

    fun uninstall cid =
        (case I.sgnLookup cid
           of I.ConDec (_, _, _, _, A, I.Type) => remove (cidFromHead (I.targetHead A), cid)
            | I.ConDef (_, _, _, _, A, I.Type, _) => remove (cidFromHead (I.targetHead A), cid)
            | _ => ())

    fun resetFrom mark =
        let
          val (limit, _) = I.sgnSize ()
          fun iter i = if i < mark then ()
                       else (uninstall i;
                             Array.update (indexArray, i, Queue.empty))
        in
          iter (limit - 1)
        end

    fun lookup a =
        let fun lk (l, NONE) = l
              | lk (l, SOME(q')) =
                (Array.update (indexArray, a, q'); l)
        in
          lk (Queue.toList (Array.sub (indexArray, a)))
        end

  

    val reset = reset
    val resetFrom = resetFrom
    val install = install
    val lookup = lookup



  |sml}
  let expected_output = "" (* TODO *)
end

module TestCase = Test_common.Make(TestFiles) 