open Types

(* normalised terms *)
type 'a s = private
  (* arity, (multi)set of located reduced components
     e.g., (3, {([2;3],x); ([2],y)}) denotes the graph of arity three
     with arity 2 full prime graph x between sources 2 and 3,
     and arity 1 full prime graph y at source 2 *)
  int * (iseq * 'a fpt) mset

(* full prime terms *)
and 'a fpt = private
  | Fgt of 'a * 'a s       (* invariant: in Fgt(x,u) all components of u touch the last source *)
  | Edg of int * perm * 'a (* permuted edge *)

include ISEALGEBRA'
        with type 'a u = 'a s
         and type 'a ru = 'a Term.u
         and type 'a rt = 'a Term.t
