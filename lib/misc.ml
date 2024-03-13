type label = string

let failwith fmt =
  Format.kasprintf failwith fmt

let pp_print_sep sep f () =
  Format.pp_print_string f sep

let pp_print_list sep =
  Format.pp_print_list ~pp_sep:(pp_print_sep sep)

let rec big b z = function
  | [] -> z
  | [x] -> x
  | x::q -> b x (big b z q)

let sqr x = x *. x

type pp_mode = Full | Sparse

class type printable =
  object
    method text: string
    method pp: pp_mode -> Format.formatter -> unit
    method pp_empty: pp_mode -> bool
  end

let same_label x y = x#text = y#text

class type drawable =
  object
    inherit printable
    method pos: Gg.p2
    method radius: float
    method color: Gg.color
    method move: Gg.p2 -> unit
    method scale: float -> unit
    method placed: bool
  end

type ('a,'b) mapper = {fi: 'a -> 'b; fe: 'a -> 'b}
type ('a,'b) smapper = {fs: int -> 'a -> 'b; fo: ('a,'b) mapper}
