open Misc

type 'a seq = 'a Seq.t          (* sequences (with index starting at 1) *)
type 'a set = 'a Set.t          (* (multi) sets *)
type perm = Perm.t              (* finite support permutations *)
type inj = Inj.t                (* finite support injections *)
type iseq = ISeq.t              (* increasing sequences *)
type kvl = Info.kvl             (* edge/vertex information *)

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
  
  val map: ('a,'b) mapper -> 'a t -> 'b t
  
  (* pretty printing *)
  val pp: pp_mode -> Format.formatter -> #printable t -> unit
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

  (* sourced elements *)
  type 'a st = 'a seq * 'a t
  val ssize: 'a st -> int
  val spp: pp_mode -> Format.formatter -> #printable st -> unit
  val smap: ('a,'b) smapper -> 'a st -> 'b st
end

val spp:
  (pp_mode -> Format.formatter -> 'b -> unit) ->
  arity:('b -> int) ->
  pp_mode -> Format.formatter -> #printable seq * 'b -> unit
  

(* derived operations are derivable... *)
module Extend(M: ALGEBRA): EALGEBRA with type 'a t = 'a M.t

(* initial algebras *)
module type INITIAL_ALGEBRA = sig
  include EALGEBRA
  module INIT(M: EALGEBRA): sig
    val eval: 'a t -> 'a M.t
    val seval: 'a st -> 'a M.st
  end
end
