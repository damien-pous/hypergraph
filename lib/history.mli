type 'a t

val create: 'a -> 'a t
val present: 'a t -> 'a
val save: 'a t -> 'a -> unit
val clear: 'a t -> unit
val undo: 'a t -> 'a option
val redo: 'a t -> 'a option
