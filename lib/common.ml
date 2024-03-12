open Gg
open Misc

type 'a seq = 'a Seq.t
type 'a set = 'a Set.t
type label = string
type perm = Perm.t
type inj = Inj.t
type iseq = ISeq.t
type color = Color.t
type font = Vg.font
type p2 = Gg.p2
type v2 = Gg.v2
type path = Vg.path
type image = Vg.image
type info = Info.t

module type ALGEBRA = sig
  type 'a t

  val nil: int -> 'a t
  val par: 'a t -> 'a t -> 'a t
  val fgt: 'a -> 'a t-> 'a t
  val lft: 'a t -> 'a t
  val prm: perm -> 'a t -> 'a t
  val edg: int -> 'a -> 'a t
  
  val arity: 'a t -> int
  val isize: 'a t -> int
  val esize: 'a t -> int

  val map:
    fi:('a -> 'b) -> (* inner vertices *)
    fe:('a -> 'b) -> (* edges *)
    'a t -> 'b t
  
  val pp_: ?full:bool -> Format.formatter -> info t -> unit  
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
  val pp: Format.formatter -> info t -> unit
  val pp_full: Format.formatter -> info t -> unit
end

module Extend(M: ALGEBRA) = struct  
  include M
  let bigpar n = big par (nil n)
  let cnv u =
    let k = arity u in
    if k<2 then failwith "invalid converse arity";
    prm (Perm.of_cycle[k-1;k]) u
  let inj k i u =
    let p,n = Inj.extend i k in
    let rec l = function
      | 0 -> u
      | n -> lft (l(n-1))
    in
    prm p (l n)
  let ser l =
    let k = List.length l+1 in
    bigpar k (List.mapi (fun i u -> prm (Perm.of_cycle[i+1;k]) (lft u)) l)
  let dot i u v = fgt i (ser [v;u])  
  let str i l =
    let k = List.length l+1 in
    fgt i (bigpar k (List.mapi (fun i u -> inj k (Inj.of_list[i+1;k]) u) l))  
  let size u = arity u + isize u + esize u
  let pp = pp_ ~full:false
  let pp_full = pp_ ~full:true
end

module type INITIAL_ALGEBRA = sig
  include EALGEBRA
  module INIT(M: EALGEBRA): sig
    val eval: 'a t -> 'a M.t
  end
end
