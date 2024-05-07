(* increasing sequences of strictly positive numbers
   (or, increasing injections)
 *)
type t

val of_list: int list -> t

(* nth element of the sequence, starting from index 1 *)
val get: t -> int -> int

(* does an element belong to a sequence *)
val mem: int -> t -> bool

(* [index s i] returns the index of i in s; raises Not_founf if i does not appear in s *)
val index: t -> int -> int

(* [3;4] -> {1->3,2->4}*)
val to_inj: t -> Inj.t

(* length of sequence / size of the support *)
val size: t -> int
(* maximal value of the sequence *)
val max: t -> int

(* empty sequence *)
val empty: t

(* 'union' of two sequences *)
val merge: t -> t -> t

(* identity sequence *)
val id: int -> t

(* filtering elements from a sequence *)
val filter: (int -> bool) -> t -> t

(* assuming s is contained in t, [reindex s t] returns the sequence r such that s = t ° r
   e.g., reindex [2;6] [2;4;5;6] = [1;4] *)
val reindex: t -> t -> t

(* crop the last element, if any *)
val crop: t -> t

(* [map p s]
   applies p to the sequence,
   reorders it to make it increasing,
   and returns the corresponding permutation *)
val map: Perm.t -> t -> t * Perm.t

val pp: Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
