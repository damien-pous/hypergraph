open Types

include ISEALGEBRA

module Flexible: sig
  type 'a t
  val nil: unit -> 'a t
  val par: 'a t -> 'a t -> 'a t
  val fgt: 'a -> 'a t-> 'a t
  val lft: 'a t -> 'a t
  val prm: perm -> 'a t -> 'a t
  val edg: 'a -> 'a t
  val inj: inj -> 'a t -> 'a t
  val ser: 'a t list -> 'a t
  val str: 'a -> 'a t list -> 'a t
  val dot: 'a -> 'a t -> 'a t -> 'a t
  val cnv: 'a t -> 'a t
end

val fixed: 'a seq -> 'a Flexible.t -> 'a t
val flexible: (int -> 'a) -> 'a Flexible.t -> 'a t
