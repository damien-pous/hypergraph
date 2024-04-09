open Gg

let inch = 72.27
let mm = inch /. 25.4

(* in points *)
let fontsize = 11.
let font = Vg.Font.{name="Latin Modern Roman"; slant=`Italic; weight=`W100; size=fontsize }

(* in inches *)
let linewidth = 0.5
let pradius = 2.0
let iradius = fontsize *. 0.4
let sradius = fontsize *. 0.5
let eradius0 = fontsize *. 1.0
let eradius = function 3 -> 3. *. eradius0 | _ -> eradius0

let gray = Color.gray 0.5
let xcolor = function
  | "red"    -> Color.v 0.8 0.0 0.0 1.
  | "green"  -> Color.v 0.0 0.8 0.0 1.
  | "blue"   -> Color.v 0.0 0.0 0.8 1.
  | "yellow" -> Color.v 0.8 0.8 0.0 1.
  | "lblue"  -> Color.v 0.4 0.8 0.8 1.
  | "orange" -> Color.v 1.0 0.4 0.0 1.
  | "violet" -> Color.v 0.4 0.0 0.4 1.
  | "gray"   -> gray
  | _        -> gray

let color' ?color label =
  match color with
  | Some c -> c 
  | None -> xcolor
              (match label with
               | "a" -> "yellow"
               | "b" -> "orange"
               | "c" -> "red"
               | "d" -> "violet"
               | "e" -> "green"
               | _   -> "gray")
let color = xcolor

let anchor_color = color "violet"
