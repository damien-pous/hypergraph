type color = Gg.Color.t            (* colors *)
type font = Vg.font             (* fonts *)
type p2 = Gg.p2                 (* points *)
type v2 = Gg.v2                 (* vectors *)
type path = Vg.path             (* paths *)
type image = Vg.image           (* images  *)

type 'a seq = 'a Seq.t          (* sequences (with index starting at 1) *)
type 'a set = 'a Set.t          (* (multi) sets *)
type label = string             (* edge labels *)
type perm = Perm.t              (* finite support permutations *)
type inj = Inj.t                (* finite support injections *)
type iseq = ISeq.t              (* increasing sequences *)
type info = Info.t              (* edge/vertex information *)

(* algebras giving the operations described in the paper *)
module type ALGEBRA = sig
  type 'a t
  val nil: int -> 'a t
  val par: 'a t -> 'a t -> 'a t
  val fgt: 'a -> 'a t-> 'a t
  val lft: 'a t -> 'a t
  val prm: perm -> 'a t -> 'a t
  val edg: int -> 'a -> 'a t

  val arity: 'a t -> int
  val isize: 'a t -> int        (* number of inner vertices) *)
  val esize: 'a t -> int        (* number of edges *)
  
  val map:
    fi:('a -> 'b) -> (* inner vertices *)
    fe:('a -> 'b) -> (* edges *)
    'a t -> 'b t
  
  (* pretty printing (with full infos or only edge labels) *)
  val pp_: ?full:bool -> Format.formatter -> info t -> unit
end

(* derived operations *)
module type EALGEBRA = sig
  include ALGEBRA
  (* n-ary parallel composition *)
  val bigpar: int -> 'a t list -> 'a t
  (* permuted lift:
     e.g.,  {2,4}a is the graph with four sources and an edge from 2 to 4 *)
  val inj: int -> inj -> 'a t -> 'a t
  (* series composition (without forgetting the last source)
     takes k arguments of arity k, and returns an element of arity k+1 *)
  val ser: 'a t list -> 'a t
  (* star composition 
     takes k arguments of arity 2, and returns an element of arity k *)
  val str: 'a -> 'a t list -> 'a t
  (* arity 2 sequential composition *)
  val dot: 'a -> 'a t -> 'a t -> 'a t
  (* arity 2 converse *)
  val cnv: 'a t -> 'a t

  (* arity + isize + esize *)
  val size: 'a t -> int

  (* pretty printing (with full infos or only edge labels) *)
  val pp: Format.formatter -> info t -> unit
  val pp_full: Format.formatter -> info t -> unit
end

(* derived operations are derivable... *)
module Extend(M: ALGEBRA): EALGEBRA with type 'a t = 'a M.t

(* initial algebras *)
module type INITIAL_ALGEBRA = sig
  include EALGEBRA
  module INIT(M: EALGEBRA): sig
    val eval: 'a t -> 'a M.t
  end
end
