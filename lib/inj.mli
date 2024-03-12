(* finite suport injections on strictly positive numbers*)
type t

(* [4;3] -> {1->4,2->3}*)
val of_list: int list -> t

(* size of the support *)
val dom: t -> int
(* maximal value of image *)
val cod: t -> int

(* is the given injection the identity *)
val is_id: int -> t -> bool

(* extend an injection into a permutation of the given size *)
(* [4,3]   4 -> [4;3;1;2],   2 *)
(* [2,3]   4 -> [2;3;1;4],   1 *)
(* [4,3]   5 -> [4;3;1;2;5], 3 *)
(* [1;3;2] 3 -> [1;3;2],     0 *)
val extend: t -> int -> Perm.t * int 

(* pretty printing *)
val pp: Format.formatter -> t -> unit
