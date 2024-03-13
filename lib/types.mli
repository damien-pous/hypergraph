type label = string         (* edge labels *)
type 'a seq = 'a Seq.t      (* sequences (with index starting at 1) *)
type 'a set = 'a Set.t      (* (multi) sets *)
type perm = Perm.t          (* finite support permutations *)
type inj = Inj.t            (* finite support injections *)
type iseq = ISeq.t          (* increasing sequences *)

type p2 = Gg.p2                 (* 2D points *)
type v2 = Gg.v2                 (* 2D vectors *)
type color = Gg.color           (* colors *)
type font = Vg.font             (* fonts *)
type image = Vg.image           (* images *)
type path = Vg.path             (* paths *)

type formatter = Format.formatter
type pp_mode = Full | Sparse

class type printable =
  object
    method label: string
    method pp: pp_mode -> formatter -> unit
    method pp_empty: pp_mode -> bool
  end

val same_label: #printable -> #printable -> bool

class type drawable =
  object
    inherit printable
    method pos: Gg.p2
    method radius: float
    method color: Gg.color
    method move: Gg.p2 -> unit
    method scale: float -> unit
    method placed: bool (* was the element placed before? *)
  end

module type BASE = sig
  type 'a t
  type ('a,'b) m
  val arity: 'a t -> int
  val isize: 'a t -> int
  val esize: 'a t -> int
  val width: 'a t -> int
  val map: ('a,'b) m -> 'a t -> 'b t
end

type ('a,'b) umapper = {fi: 'a -> 'b; fe: 'a -> 'b}
module type ALGEBRA = sig
  include BASE with type ('a,'b) m = ('a,'b) umapper
  val nil: int -> 'a t
  val par: 'a t -> 'a t -> 'a t
  val fgt: 'a -> 'a t-> 'a t
  val lft: 'a t -> 'a t
  val prm: perm -> 'a t -> 'a t
  val edg: int -> 'a -> 'a t
end

module type EALGEBRA = sig
  include ALGEBRA
  val bigpar: int -> 'a t list -> 'a t
  val inj: int -> inj -> 'a t -> 'a t
  val ser: 'a t list -> 'a t
  val str: 'a -> 'a t list -> 'a t
  val dot: 'a -> 'a t -> 'a t -> 'a t
  val cnv: 'a t -> 'a t
  val size: 'a t -> int
end

module type IALGEBRA = sig
  include ALGEBRA
  module I(M: EALGEBRA): sig val eval: 'a t -> 'a M.t end
end

module type IEALGEBRA = sig
  include EALGEBRA
  module I(M: EALGEBRA): sig val eval: 'a t -> 'a M.t end  
  val pp: pp_mode -> formatter -> #printable t -> unit
  type 'a r
  val raw: 'a t -> 'a r
end

type ('a,'b) mapper = {fs: int -> 'a -> 'b; fu: ('a,'b) umapper}
module type SEALGEBRA = sig
  type 'a u 
  type 'a ru
  type 'a rt
  module U: IEALGEBRA with type 'a t = 'a u and type 'a r = 'a ru
  include BASE
          with type 'a t = 'a seq * 'a u
           and type ('a,'b) m = ('a,'b) mapper
  val source: 'a seq -> 'a u -> 'a t
  val size: 'a t -> int
  val pp: pp_mode -> formatter -> #printable t -> unit  
  val raw: 'a t -> 'a rt
end
module type ISEALGEBRA = sig
  include SEALGEBRA
  module SI(M: SEALGEBRA): sig val eval: 'a t -> 'a M.t end
end
