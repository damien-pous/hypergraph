(* finite suport permutations on strictly positive numbers *)
type t

(* [a;b;c] -> (abc) *)
val of_cycle: int list -> t
(* [a;b;c] -> [abc] *)
val of_list: int list -> t

(* f, n -> [f 1; ... ;f n] *)
val of_fun: int -> (int -> int) -> t

(* size of the support *)
val size: t -> int

(* f i -> f i *)
val apply: t -> int -> int

(* f, [x_1;...;x_n] -> [x_f1;...;x_fn]*)
val sapply: t -> 'a Seq.t -> 'a Seq.t
val lapply: t -> 'a list -> 'a list

(* extensional equality *)
val eq: t -> t -> bool

(* identity, composition, and inverse *)
val id: t
val comp: t -> t -> t           (* [comp p q] is q°p *)
val inv: t -> t

(* pretty printing *)
val pp: Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
