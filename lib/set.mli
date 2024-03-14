(** finite (multi)sets of values *)

type 'a t

val empty: 'a t
val single: 'a -> 'a t
val union: 'a t -> 'a t -> 'a t
val add: 'a -> 'a t -> 'a t

val filter: ('a -> bool) -> 'a t -> 'a t

val remq: 'a -> 'a t -> 'a t
  
val case: 'a t -> ('a * 'a t) option
val is_empty: 'a t -> bool

val exists: ('a -> 'a t -> bool) -> 'a t -> bool

val size: 'a t -> int
val map: ('a -> 'b) -> 'a t -> 'b t
val partition: ('a -> bool) -> 'a t -> 'a t * 'a t
val lmap: ('a -> 'b) -> 'a t -> 'b list
val iter: ('a -> unit) -> 'a t -> unit
val fold: ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b

val iteri: (int -> 'a -> unit) -> 'a t -> unit
val index: 'a -> 'a t -> int
val nth: 'a t -> int -> 'a

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
