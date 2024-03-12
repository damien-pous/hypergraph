open Gg

type kv = string*string
type kvl = kv list

type t

val of_kvl: ?label:string -> kvl -> t
val for_ivertex: p2 -> t
val for_source: p2 -> int -> t
val for_edge: ?color:Color.t -> string -> t Seq.t -> t

val pos: t -> p2
val radius: t -> float
val scale: t -> float
val label: t -> string
val color: t -> Color.t

val user_pos: t -> p2 option
val user_shift: t -> p2

val set_pos: t -> p2 -> unit
val set_scale: t -> float -> unit
val set_label: t -> string -> unit
val set_color: t -> Color.t -> unit

val same_label: t -> t -> bool
val inside: p2 -> t -> bool

(* pretty printing (full infos or only edge labels) *)
val pp: ?full:bool -> Format.formatter -> t -> unit
