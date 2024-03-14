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

type line = { point: p2; dir: v2 }          (* directed line *)
type circle = { center: p2; radius: float } (* circles *)

type formatter = Format.formatter
type pp_mode = Full | Sparse

(* 'functions' used to map decorations in terms or graphs *)
type ('a,'b) mapper =
  { fs: int -> 'a -> 'b;        (* sources; first argument is the arity *)
    fi: 'a -> 'b;               (* inner vertices *)
    fe: int -> 'a -> 'b }       (* edges; first argument is the edge arity *)

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
    method circle: circle
    method color: Gg.color
    method move: Gg.p2 -> unit
    method scale: float -> unit
    method placed: bool (* was the element placed before? *)
  end

class type picture =
  object
    method clear: unit
    method get: image
    method blend: image -> unit
    method path: ?color:color -> path -> unit
    method surface: ?color:color -> path -> unit
    method circle: ?color:color -> circle -> unit
    method disc: ?color:color -> circle -> unit
    method point: ?color:color -> p2 -> unit
    method segment: ?color:color -> p2 -> p2 -> unit 
    method line: ?color:color -> line -> unit 
    method text: p2 -> string -> unit 
  end

module type BASE = sig
  type 'a t
  val arity: 'a t -> int
  val isize: 'a t -> int
  val esize: 'a t -> int
  val width: 'a t -> int
  val map: ('a,'b) mapper -> 'a t -> 'b t
end

(* the signature of operations from the paper *)
module type ALGEBRA = sig
  include BASE
  val nil: int -> 'a t
  val par: 'a t -> 'a t -> 'a t
  val fgt: 'a -> 'a t-> 'a t
  val lft: 'a t -> 'a t
  val prm: perm -> 'a t -> 'a t
  val edg: int -> 'a -> 'a t
end

(* derived syntactic operations *)
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

(* initial algebras *)
module type IALGEBRA = sig
  include ALGEBRA
  module I(M: EALGEBRA): sig val eval: 'a t -> 'a M.t end
end

(* initial extended algebras *)
module type IEALGEBRA = sig
  include EALGEBRA
  module I(M: EALGEBRA): sig val eval: 'a t -> 'a M.t end  
  val pp: pp_mode -> formatter -> #printable t -> unit
end

(* extension with source decorations *)
module type SEALGEBRA = sig
  type 'a u 
  module U: IEALGEBRA with type 'a t = 'a u
  include BASE
  val source: 'a seq -> 'a u -> 'a t
end

(* initial sourced extended algebras *)
module type ISEALGEBRA = sig
  include SEALGEBRA
  module SI(M: SEALGEBRA): sig val eval: 'a t -> 'a M.t end  
  val pp: pp_mode -> formatter -> #printable t -> unit
end

(* additional derived operations *)
module type IEALGEBRA' = sig
  include IEALGEBRA
  val forget: int -> 'a -> 'a t -> 'a t
  type 'a r
  val raw: 'a t -> 'a r
end

(* additional derived sourced operations *)
module type ISEALGEBRA' = sig
  type 'a u 
  type 'a ru
  module U: IEALGEBRA' with type 'a t = 'a u and type 'a r = 'a ru
  include BASE with type 'a t = 'a seq * 'a u
  val source: 'a seq -> 'a u -> 'a t
  val size: 'a t -> int
  val nil: unit -> 'a t
  val prm: perm -> 'a t -> 'a t
  val lft: 'a -> 'a t -> 'a t
  val fgt: 'a t -> 'a t
  val forget: int -> 'a t -> 'a t
  module SI(M: SEALGEBRA): sig val eval: 'a t -> 'a M.t end
  val pp: pp_mode -> formatter -> #printable t -> unit  
  type 'a rt
  val raw: 'a t -> 'a rt
end
