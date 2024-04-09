open Gg
open Vg

(* number of points in an inch: 72(.27) *)
val inch: float
(* number of points in a mm: inch / 25.4 *)
val mm: float

(* below: always in points *)
val fontsize : float
val font : font

val iradius : float
val sradius : float
val eradius : int -> float
val pradius : float
val linewidth : float

val gray : color
val color : string -> color
val color' : ?color:color -> string -> color

val anchor_color: color
