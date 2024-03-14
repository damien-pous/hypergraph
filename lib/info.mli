open Types

type kv
type kvl = kv list

val kv: string -> string -> kv

val print_mapper: (kvl, printable) mapper
val draw_mapper: (kvl, drawable) mapper

val drawable_ivertex: p2 -> drawable
val drawable_source: int -> p2 -> drawable
val drawable_edge: int -> label -> drawable

val same_label_kvl: kvl -> kvl -> bool
val same_label: #printable -> #printable -> bool
