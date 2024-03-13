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

include INITIAL_ALGEBRA with type 'a t = 'a s

val nil': 'a t
val inj': inj -> 'a t -> 'a t
val edg': 'a -> 'a t

val fix: (int -> 'a) -> int -> 'a t -> 'a st
val flex: (int -> 'a) -> 'a t -> 'a st
val source: 'a seq -> 'a t -> 'a st
