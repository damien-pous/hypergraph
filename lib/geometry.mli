open Types

(* debug area *)
val set_debug: canvas -> unit
val unset_debug: unit -> unit

(* directed lines *)
val line: point -> point -> line
val line': point -> vector -> line

(* circles *)
val circle: point -> float -> circle

(* random point in [-s;s]x[-s;s] *)
val random2: float -> point

(* distance between two points *)
val dist: point -> point -> float

(* is a point inside a drawable element *)
val inside: point -> positionned -> bool

(* barycenter *)
val center: point list -> point

(* center of incircle *)
val mid3: point -> point -> point -> point

(* directed angle between two vectors *)
val angle: vector -> vector -> float

(* intersection of two lines *)
val line_inter: line -> line -> point

(* (directed) distance between a line and a point *)
val line_dist : line -> point -> float

(* orieantations *)
type lr = L | R
val swap: lr -> lr

(* side of a point w.r.t. a line *)
val side: line -> point -> lr
  
(* [tangent_point x c o] computes the tangents of [c] going through [x],
   and returns the tangent point on side [o] of [xc] *)
val tangent_point: point -> circle -> lr -> point * vector

(* [bisect_point x y c] returns the point of [c] bisecting cone [xcy]  *)
val bisect_point: point -> point -> circle -> point * vector

val start: point -> path

val curve: circle -> ?lx:bool -> ?ly:bool -> point -> point -> path
val edge1: circle -> point -> path
val edge2: circle -> point -> point -> path
val edge3: circle -> point -> point -> point -> path

val edge: circle -> point list -> path
