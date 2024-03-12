open Common

(** drawable dynamic graphs *)
class graph: 
  object
    inherit [info] Graph.dyn
    method draw: Vg.image
  end

