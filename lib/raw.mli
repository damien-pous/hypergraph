open Common

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
  (* to fix an explicit arity *)
  | Fix of int * 'a s

include INITIAL_ALGEBRA with type 'a t = 'a s

val nil': 'a s
val inj': inj -> 'a s -> 'a s
val edg': 'a -> 'a s
val fix: int -> 'a s -> 'a s
