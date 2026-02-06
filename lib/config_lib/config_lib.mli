include module type of Config_lib_

type t
type arg
type 'a flag = 'a Config_lib_.flag

val get : 'a flag -> t -> 'a
val set : 'a flag -> 'a -> arg
val create : arg list -> t

module type CONFIG = sig
  val config : t
end
