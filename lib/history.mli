type 'a t

val create: unit -> 'a t
val save: 'a t -> 'a -> unit
val reset: 'a t -> unit
val undo: 'a t -> 'a option
val redo: 'a t -> 'a option
