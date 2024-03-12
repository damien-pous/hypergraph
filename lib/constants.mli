open Gg

val iradius : float
val sradius : float
val eradius : float
val linewidth : float
val fontsize : float
val font : Vg.font
val gray : Color.t
val color : string -> Color.t
val color' : ?color:Color.t -> string -> Color.t
val render_labels_with_cairo : bool
