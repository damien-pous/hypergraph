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
  | "red"    -> Color.v 0.8 0.1 0.1 1.
  | "green"  -> Color.v 0.1 0.8 0.1 1.
  | "blue"   -> Color.v 0.1 0.1 0.8 1.
  | "yellow" -> Color.v 0.8 0.8 0.0 1.
  | "lblue"  -> Color.v 0.4 0.8 0.8 1.
  | "orange" -> Color.v 1.0 0.4 0.0 1.
  | "violet" -> Color.v 0.4 0.1 0.4 1.
  | "turquoise" -> Color.v 0.0 0.4 0.4 1.
  | "rose"   -> Color.v 1.0 0.4 1.0 1.
  | "purple" -> Color.v 0.8 0.0 0.4 1.
  | "brown"  -> Color.v 0.7 0.3 0.0 1.
  | "cacadoie" -> Color.v 0.3 0.6 0.0 1.
  | "black"  -> Color.black
  | "gray"   -> gray
  | _        -> gray

let color' ?color label =
  match color with
  | Some c -> c 
  | None -> xcolor
              (if label = "" then ""
               else match label.[0] with
               | 'a' -> "yellow"
               | 'b' -> "orange"
               | 'c' -> "red"
               | 'd' -> "violet"
               | 'e' -> "green"
               | 'f' -> "lblue"
               | 'g' -> "blue"
               | 'h' -> "turquoise"
               | 'i' -> "purple"
               | 'j' -> "rose"
               | 'k' -> "cacadoie"
               | _   -> "gray")
let color = xcolor
