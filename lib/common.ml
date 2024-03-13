open Misc

type 'a seq = 'a Seq.t
type 'a set = 'a Set.t
type perm = Perm.t
type inj = Inj.t
type iseq = ISeq.t
type kvl = Info.kvl

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
  val map: ('a,'b) mapper -> 'a t -> 'b t  
  val pp: pp_mode -> Format.formatter -> #printable t -> unit  
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
  type 'a st = 'a seq * 'a t
  val ssize: 'a st -> int
  val spp: pp_mode -> Format.formatter -> #printable st -> unit
  val smap: ('a,'b) smapper -> 'a st -> 'b st
end

let spp pp ~arity mode f (s,u) =
  if mode=Sparse || Seq.forall (fun s -> s#pp_empty mode) s then
    let k = Seq.size s in
    if arity u = k then pp mode f u
    else Format.fprintf f "#%i %a" k (pp mode) u
  else
    let ppx f x = x#pp mode f in
    Format.fprintf f "#%a %a" (pp_print_list "," ppx) (Seq.to_list s) (pp mode) u

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
  type 'a st = 'a seq * 'a t
  let ssize (_,u) = size u
  let spp mode f x = spp pp ~arity mode f x
  let smap f (s,u) = (Seq.imap f.fs s, map f.fo u)
end

module type INITIAL_ALGEBRA = sig
  include EALGEBRA
  module INIT(M: EALGEBRA): sig
    val eval: 'a t -> 'a M.t
    val seval: 'a st -> 'a M.st
  end
end
