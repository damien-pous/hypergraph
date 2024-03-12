open Common

(* terms, as in the paper *)
type 'a s = private
  | Nil of int
  | Par of 'a s * 'a s
  | Fgt of 'a * 'a s
  | Lft of 'a s
  | Prm of perm * 'a s
  | Edg of int * 'a

include INITIAL_ALGEBRA with type 'a t = 'a s

val width: 'a t -> int

val raw: 'a t -> 'a Raw.t
