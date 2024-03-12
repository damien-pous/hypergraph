
val failwith: ('a, Format.formatter, unit, 'b) format4 -> 'a

val pp_print_sep:
  string -> Format.formatter -> unit -> unit

val pp_print_list:
  string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val big: ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a

val sqr: float -> float

