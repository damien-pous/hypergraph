open Types

(* debug area *)
val set_debug: picture -> unit
val unset_debug: unit -> unit

(* directed lines *)
val line: p2 -> p2 -> line
val line': p2 -> v2 -> line

(* circles *)
val circle: p2 -> float -> circle

(* random point in [-s;s]x[-s;s] *)
val random2: float -> p2

(* distance between two points *)
val dist: p2 -> p2 -> float

(* is a point inside a drawable element *)
val inside: p2 -> drawable -> bool

(* barycenter *)
val center: p2 list -> p2

(* center of incircle *)
val mid3: p2 -> p2 -> p2 -> p2

(* directed angle between two vectors *)
val angle: v2 -> v2 -> float

(* intersection of two lines *)
val line_inter: line -> line -> p2

(* (directed) distance between a line and a point *)
val line_dist : line -> p2 -> float

(* orieantations *)
type lr = L | R
val swap: lr -> lr

(* side of a point w.r.t. a line *)
val side: line -> p2 -> lr
  
(* [tangent_point x c o] computes the tangents of [c] going through [x],
   and returns the tangent point on side [o] of [xc] *)
val tangent_point: p2 -> circle -> lr -> p2 * v2

(* [bisect_point x y c] returns the point of [c] bisecting cone [xcy]  *)
val bisect_point: p2 -> p2 -> circle -> p2 * v2

val start: p2 -> path

val curve: circle -> ?lx:bool -> ?ly:bool -> p2 -> p2 -> path
val edge1: circle -> p2 -> path
val edge2: circle -> p2 -> p2 -> path
val edge3: circle -> p2 -> p2 -> p2 -> path

val edge: circle -> p2 list -> path
