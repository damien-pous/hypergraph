(** sequences of values, with index starting at 1 *)

type 'a t

val get: 'a t -> int -> 'a
val init: int -> (int -> 'a) -> 'a t
val to_list: 'a t -> 'a list
val of_list: 'a list -> 'a t

val empty: 'a t
val snoc: 'a t -> 'a -> 'a t
val case: 'a t -> ('a t * 'a) option
val is_empty: 'a t -> bool
val mem: 'a -> 'a t -> bool

val iter: (int -> 'a -> unit) -> 'a t -> unit
val map: ('a -> 'b) -> 'a t -> 'b t
val omap: ('a -> 'b option) -> 'a t -> 'b t
val imap: (int -> 'a -> 'b) -> 'a t -> 'b t
val lmap: ('a -> 'b) -> 'a t -> 'b list
val fold: ('a -> 'c -> 'c) -> 'a t -> 'c -> 'c
val ofold2: ('c -> 'a -> 'b -> 'c option) -> 'c -> 'a t -> 'b t -> 'c option
val forall: ('a -> bool) -> 'a t -> bool

val size: 'a t -> int

val id: int -> int t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
