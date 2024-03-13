open Misc
open Gg

type kv
type kvl = kv list

val kv: string -> string -> kv

val print_mapper: (kvl, printable) mapper
val print_smapper: (kvl, printable) smapper

val draw_mapper: (kvl, drawable) mapper
val draw_smapper: (kvl, drawable) smapper

val drawable_ivertex: p2 -> drawable
val drawable_source: int -> p2 -> drawable
val drawable_edge: int -> string -> drawable
