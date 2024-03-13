open Hypergraphs
open Conversions

let s = "a|b|0|(214)(c|d)|lfe"
(* let s = "(23)la|(13)lb|lc" *)
(* let s = "f<pos=(.5,1)>((23)la|(13)lb)|c<pos=(0,-.3),color=yellow>" *)
(* let s = "(3)a<pos=(0,0),color=orange>" *)
(* let s = "(2)a<pos=(1,-2.3),color=orange>" *)
(* let s = "{43}a<color=orange>" *)
(* let s = "{43}a<color=orange>|b.c" *)
(* let s = "la'" *)
(* let s = "f{1}(a|b)" *)
let l = Lexing.from_string s
let t = Parser.main Lexer.token l
let t = Raw.smap Info.print_smapper t
let _ = Format.printf "%a@." (Raw.spp Full) t
let g = S.graph_of_raw t
let _ = Format.printf "%a@." (Graph.spp Full) g
(* let _ = Graph.export_dot "test.png" g *)
(* let g = [(0.,0.);(10.,0.);(5.,sqrt 75.)],g *)
(* let _ = Printf.printf "%a\n%!" Graph.pp_tikz g *)
(* let _ = Graph.export_tikz "test.svg" g *)
