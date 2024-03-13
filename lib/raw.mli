open Types

type 'a s = private
  | Nil
  | Par of 'a s * 'a s
  | Fgt of 'a * 'a s
  | Lft of 'a s
  | Prm of perm * 'a s
  | Edg of 'a
  (* derived *)
  | Inj of inj * 'a s
  | Ser of 'a s list
  | Str of 'a * 'a s list
  | Dot of 'a * 'a s * 'a s
  | Cnv of 'a s

include ISEALGEBRA with type 'a u = 'a s

val nil': 'a u
val inj': inj -> 'a u -> 'a u
val edg': 'a -> 'a u

val flexible: (int -> 'a) -> 'a u -> 'a t
