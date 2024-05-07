(** finite multisets of values *)

type 'a t

val empty: 'a t
val single: 'a -> 'a t
val union: 'a t -> 'a t -> 'a t
val add: 'a -> 'a t -> 'a t
val mem: 'a -> 'a t -> bool

val filter: ('a -> bool) -> 'a t -> 'a t

val remq: 'a -> 'a t -> 'a t
  
val case: 'a t -> ('a * 'a t) option
val is_empty: 'a t -> bool

val exists_split: ('a -> 'a t -> bool) -> 'a t -> bool

val size: 'a t -> int
val map: ('a -> 'b) -> 'a t -> 'b t
val omap: ('a -> 'b option) -> 'a t -> 'b t
val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
val lmap: ('a -> 'b) -> 'a t -> 'b list
val iter: ('a -> unit) -> 'a t -> unit
val fold: ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
val big: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
val exists: ('a -> bool) -> 'a t -> bool
val forall: ('a -> bool) -> 'a t -> bool
val find: ('a -> bool) -> 'a t -> 'a option

val iteri: (int -> 'a -> unit) -> 'a t -> unit
val index: 'a -> 'a t -> int
val nth: 'a t -> int -> 'a

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit [@@ocaml.toplevel_printer]
