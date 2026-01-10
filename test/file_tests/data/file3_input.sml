signature MODESYN =
sig

  datatype Mode = Plus | Star | Minus | Minus1
  datatype ModeSpine = Mnil | Mapp of Marg * ModeSpine
  and Marg = Marg of Mode * string option

  val modeEqual : Mode * Mode -> bool
  val modeToString : Mode -> string
end;


structure ModeSyn :> MODESYN =
struct

  exception Error of string

  datatype Mode = Plus | Star | Minus | Minus1
  datatype ModeSpine = Mnil | Mapp of Marg * ModeSpine
  and  Marg = Marg of Mode * string option


  fun modeEqual (Plus, Plus) = true
    | modeEqual (Star, Star) = true
    | modeEqual (Minus, Minus) = true
    | modeEqual (Minus1, Minus1) = true
    | modeEqual (_, _) = false

  fun modeToString Plus = "input (+)"
    | modeToString Star = "unrestricted (*)"
    | modeToString Minus = "output (-)"
    | modeToString Minus1 = "unique output (-1)"

end;
