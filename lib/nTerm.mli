open Common

(* normalised terms *)
type 'a s =
  (* arity, (multi)set of located reduced components
     e.g., (3, {([2;3],x); ([2],y)}) denotes the graph of arity three
     with arity 2 full prime graph x between sources 2 and 3,
     and arity 1 full prime graph y at source 2 *)
  private int * (iseq * 'a fpt) set

(* full prime terms *)
and 'a fpt = private
  | Fgt of 'a * 'a s       (* invariant: in Fgt(x,u) all components of u touch the last source *)
  | Edg of int * perm * 'a (* permuted edge *)

include INITIAL_ALGEBRA with type 'a t = 'a s

val width: 'a t -> int

val raw: 'a t -> 'a Raw.t
val sraw: 'a st -> 'a Raw.st
