open Hypergraphs
open Conversions

let pdf = ref true
let svg = ref true

let last l = List.hd (List.rev l)

let read f =
  let i = open_in (f^".hg") in
  let l = Lexing.from_channel i in
  let l = Parser.file Lexer.token l in
  close_in i;
  List.map (Raw.map Info.kvl_to_positionned) l

let write f l =
  let o = open_out (f^".hg") in
  let f = Format.formatter_of_out_channel o in
  let rec pp = function
    | [] -> ()
    | [t] -> Format.fprintf f "%a@." (Raw.pp Full) t
    | t::q -> Format.fprintf f "%a\n~ " (Raw.pp Sparse) t; pp q
  in
  pp l;
  close_out o

let check_ ?(warn=false) f l =
  let rec check i = function
    | [] | [_] -> ()
    | g::(h:: _ as q) ->
       if not (Graph.iso Info.same_label g h) then
         (Format.eprintf "error: graph number %i is not isomorphic to the previous ones\n" i; exit 1);
       check (i+1) q
  in
  if warn && List.length l <= 1 then Format.eprintf "warning: nothing to check in %s\n" f;
  check 2 (List.map graph_of_raw l)

let check f = check_ ~warn:true f (read f)

let export_ f t =
  let g = graph_of_raw t in
  let view = Graph.bbox g in
  let image = Graph.draw g in
  if !pdf then Picture.pdf image view( f^".pdf");
  if !svg then Picture.svg image view( f^".svg");
  ()

let export f =
  let l = read f in
  check_ f l;
  export_ f (last l)

let place f =
  let l = read f in
  check_ f l;
  let t = last l in
  let g = graph_of_raw t in
  let _ = Place.sources_on_circle g in
  let _ = Place.graphviz g in
  let l = l@[t] in
  write f l;
  export_ f t

let _ =
  Arg.(parse
         [ "-nopdf", Clear svg, "\tno PDF output";
           "-nosvg", Clear pdf, "\tno SVG output";
           "-check", String check, "f\tcheck file f.hg (no output)";
           "-place", String place, "f\tplace file f.hg (before output)" ]
         export
         "hg [-nopdf, -nosvg, -check file, -place file, file]*")
