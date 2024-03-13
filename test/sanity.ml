open Hypergraphs
open Conversions.S

let from_string s =
  try
    let l = Lexing.from_string s in
    let t = Parser.main Lexer.token l in
    Raw.smap Info.print_smapper t
  with e -> Format.eprintf "error parsing  %s@." s; raise e

let to_string = Format.kasprintf (fun s -> s) "%a" (Raw.spp Full)

let iso = Graph.siso Misc.same_label
let gpp = Graph.spp Sparse
let rpp = Raw.spp Sparse
(* let rpp_full = Raw.spp Full *)

let test s =
  let t = from_string s in
  let s' = to_string t in
  try
    let t' = from_string s' in
    (* if t<>t' then ( *)
    (*   Format.eprintf "Warning: reparsing mismatch\n%s\n%a\n%s\n%a@." s rpp_full t s' rpp_full t'; *)
      let _ =
    iso (graph_of_raw t) (graph_of_raw t') ||
      (Format.eprintf "Error: reparsing mismatch\n%s\n%a\n%s\n%a@." s rpp t s' rpp t'; failwith "iso") in
    ()
    (* ) *)
    ;
    (* Format.eprintf "t = %a@." Raw.pp t; *)
    (* Format.eprintf "t' = %a@." Term.pp (term_of_raw t); *)
    (* Format.eprintf "t'' = %a@." NTerm.pp (nterm_of_raw t); *)
    let g = graph_of_raw t in
    let g' = graph_of_term (term_of_raw t) in
    let g'' = graph_of_nterm (nterm_of_raw t) in
    let _ = iso g g' || (Format.eprintf "Sanity failed iso:\ng = \n%ag'= \n%a@." gpp g gpp g'; failwith "iso") in
    let _ = iso g' g'' || (Format.eprintf "Sanity failed iso:\ng' = \n%ag''= \n%a@." gpp g' gpp g''; failwith "iso") in
    let _ = iso g g'' || (Format.eprintf "Sanity failed iso:\ng = \n%ag''= \n%a@." gpp g gpp g''; failwith "iso") in
    ()
  with e -> Format.eprintf "init input was %s@." s; raise e

let test_iso s s' =
  let t = from_string s in
  let t' = from_string s' in
  let g = graph_of_raw t in
  let g' = graph_of_raw t' in
  iso g g' || (Format.eprintf "Sanity failed iso:\nt = %a\nt'= %a@." rpp t rpp t'; failwith "iso")
  
let _ = test "a|(b|c)"
let _ = test "(a|b)|c"
let _ = test "a.(b.c)"
let _ = test "(a.b).c"
let _ = test "(12)(12)a"
let _ = test "a|b|0|(214)(c|d)|lfe"
let _ = test "(23)la|(13)lb|lc"
let _ = test "a.b"
let _ = test "f((23)la|(13)lb)"
let _ = test "f((23)la|(13)lb)|c<color=yellow>"
let _ = test "a<pos=0.5,-0.3>"
let _ = test "f<pos=0.5,1>((23)la|(13)lb)|c<pos=0,-.3;color=yellow>"
let _ = test "#3 a<pos=0,1;color=orange>"
let _ = test "#2 a<pos=1,-2.3;color=orange>"
let _ = test "{43}a<color=orange>"
let _ = test "a<color=orange>|b.c"
let _ = test "la'"
let _ = test "{1}a|{2}b|{3}c"
let _ = test "f{132}a"
let _ = test "f{213}a"
let _ = test "{234}d"
let _ = test "f{234}d"
let _ = test "f(134)ld"
let _ = test "f{324}d"
let _ = test "f({123}a | {12}b | {324}d)"
let _ = test "#3 lb"
let _ = test "#<color=orange>,<color=yellow> lb"

let _ = test_iso "a|(b|c)" "(a|b)|c"
let _ = test_iso "#3 a|(b|c)" "#3 (a|b)|c"
let _ = test_iso "ffa" "ffa'"
let _ = test_iso "#4 ffa" "#4 ffa'"
let _ = test_iso "lla" "(lla)'"
let _ = test_iso "#4 lla" "#4 (lla)'"
let _ = test_iso "fa | b" "f(a | lb)"
let _ = test_iso "#4 fa | b" "#4 f(a | lb)"

let _ = test_iso "(134)ld" "{324}d"
let _ = test_iso "f(134)ld" "f{324}d"
