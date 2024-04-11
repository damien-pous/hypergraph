open Conversions

type term = Types.positionned Term.t

type t = term list

let single x = [x]

let first = List.hd

let last l = List.hd (List.rev l)

let compatible l x = Graph.iso Info.same_label (graph_of_term (first l)) (graph_of_term x)

let append l x = l@[x]

let check l =
  let g = graph_of_term (first l) in
  List.iteri (fun i x ->
      if not (Graph.iso Info.same_label_kvl g (graph_of_term x)) then
        Format.kasprintf failwith "error: graph number %i is not isomorphic to the previous ones" (i+1)
    ) (List.tl l) 

let read f =
  let i = open_in (f^".hg") in
  let l = Lexing.from_channel i in
  let l = Parser.file Lexer.token l in
  close_in i;
  check l;
  List.map (Term.map Info.kvl_to_positionned) l

let write f l =
  let o = open_out (f^".hg") in
  let f = Format.formatter_of_out_channel o in
  let rec pp = function
    | [] -> ()
    | [t] -> Format.fprintf f "%a@." (Term.pp Full) t
    | t::q -> Format.fprintf f "%a\n~ " (Term.pp Sparse) t; pp q
  in
  pp l;
  close_out o

let export_term f t =
  let g = graph_of_term t in
  let view = Graph.bbox g in
  let image = Graph.draw g in
  Picture.pdf image view( f^".pdf");
  Picture.svg image view( f^".svg");
  ()
    
let export f l = export_term f (last l)

let exists f = Sys.file_exists (f ^ ".hg")
