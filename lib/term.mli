open Types

(* terms, as in the paper *)
type 'a s = private
  | Nil of int
  | Par of 'a s * 'a s
  | Fgt of 'a * 'a s
  | Lft of 'a s
  | Prm of perm * 'a s
  | Edg of int * 'a

include ISEALGEBRA'
        with type 'a u = 'a s
         and type 'a ru = 'a Raw.u
         and type 'a rt = 'a Raw.t
