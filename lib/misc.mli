open Gg

type label = string             (* edge labels *)

val failwith: ('a, Format.formatter, unit, 'b) format4 -> 'a

val pp_print_sep:
  string -> Format.formatter -> unit -> unit

val pp_print_list:
  string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val big: ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a

val sqr: float -> float

type pp_mode = Full | Sparse

class type printable =
  object
    method text: string
    method pp: pp_mode -> Format.formatter -> unit
    method pp_empty: pp_mode -> bool
  end

val same_label: #printable -> #printable -> bool

class type drawable =
  object
    inherit printable
    method pos: p2
    method radius: float
    method color: color
    method move: p2 -> unit
    method scale: float -> unit
    method placed: bool (* was the element placed before  *)
  end

type ('a,'b) mapper = {fi: 'a -> 'b; fe: 'a -> 'b}
type ('a,'b) smapper = {fs: int -> 'a -> 'b; fo: ('a,'b) mapper}
