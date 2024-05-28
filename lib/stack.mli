(* not really stacks: lists with insertion capabilities at a designated position *)

type 'a t

val of_list: 'a list -> 'a t
val to_list: 'a t -> 'a list

val same: 'a t -> 'a t -> bool
  
val pos: 'a t -> int
val size: 'a t -> int
val count: ('a -> bool) -> 'a t -> int

val current: 'a t -> 'a
val move_left: 'a t -> 'a t
val move_right: 'a t -> 'a t

val pop: 'a t -> 'a t
val replace: 'a t -> 'a -> 'a t
val push_right: 'a t -> 'a -> 'a t
val push_here: 'a t -> 'a -> 'a t
