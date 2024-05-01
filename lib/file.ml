open Hypergraphs
open Gg
open Vg

let pdf image view file =
  (* export via cairo (could not find how to get the right fonts with vg) *)
  let size = Box2.size view in
  let w,h = V2.x size, V2.y size in
  let i = Cairo.PDF.create file ~w ~h in
  let cr = Cairo.create i in
  let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
  ignore (Vgr.render vgr (`Image (size, view, image)));
  ignore (Vgr.render vgr `End);
  Cairo.Surface.finish i

let svg image view file =
  (* export via vg (also possible via cairo as above) *)
  let size = Box2.size view in
  let o = open_out_bin file in
  (* let title = "some nice graph" in *)
  (* let description = "some description" in *)
  let xmp = Vgr.xmp (* ~title ~description *) () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_svg.target ~xmp ()) (`Channel o) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  close_out o


type term = Types.positionned Term.t

type t = term list

let single x = [x]

let first = List.hd

let last l = List.hd (List.rev l)

let compatible l x = Graph.iso Info.same_label (Graph.of_term (first l)) (Graph.of_term x)

let append l x = l@[x]

let check l =
  let g = Graph.of_term (first l) in
  List.iteri (fun i x ->
      if not (Graph.iso Info.same_label_kvl g (Graph.of_term x)) then
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
  let g = Graph.of_term t in
  let view = Graph.bbox g in
  let image = Graph.draw g in
  pdf image view( f^".pdf");
  svg image view( f^".svg");
  ()
    
let export f l = export_term f (last l)

let exists f = Sys.file_exists (f ^ ".hg")
  
