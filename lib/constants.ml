open Gg

let iradius = 0.02
let sradius = 0.03
let eradius = function 3 -> 0.2 | _ -> 0.08
let linewidth = 0.001
let fontsize = 0.07
let font = Vg.Font.{name="Latin Modern Roman"; slant=`Italic; weight=`W100; size=fontsize }

let gray = Color.gray 0.5
let xcolor = function
  | "red"    -> Color.v 0.8 0.0 0.0 1.
  | "green"  -> Color.v 0.0 0.8 0.0 1.
  | "blue"   -> Color.v 0.0 0.0 0.8 1.
  | "yellow" -> Color.v 0.8 0.8 0.0 1.
  | "orange" -> Color.v 0.4 0.8 0.8 1.
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

(* should we render labels with vg or cairo ? *)
let render_labels_with_cairo = false
