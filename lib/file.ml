open Gg
open Vg

let multi_pdf l file =  
  (* export to pdf via cairo (could not find how to get the right fonts with vg) *)
  match l with
  | [] -> failwith "cannot export an empty list to PDF"
  | (image,view)::q ->
     let size = Box2.size view in
     let w,h = V2.x size, V2.y size in
     let i = Cairo.PDF.create file ~w ~h in
     let cr = Cairo.create i in
     let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
     ignore (Vgr.render vgr (`Image (size, view, image)));
     List.iter (fun (image,view) ->
         Cairo.show_page cr;
         let w,h = V2.x size, V2.y size in
         Cairo.PDF.set_size i ~w ~h;
         ignore (Vgr.render vgr (`Image (size, view, image)))
       ) q;
     ignore (Vgr.render vgr `End);
     Cairo.Surface.finish i

let pdf image view file = multi_pdf [image,view] file

let multi_svg l file =  
  (* export to svg via cairo *)
  match l with
  | [] -> failwith "cannot export an empty list to SVG"
  | (image,view)::q ->
     let size = Box2.size view in
     let w,h = V2.x size, V2.y size in
     let i = Cairo.SVG.create file ~w ~h in
     let cr = Cairo.create i in
     let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
     ignore (Vgr.render vgr (`Image (size, view, image)));
     List.iter (fun (image,view) ->
         Cairo.show_page cr;
         ignore (Vgr.render vgr (`Image (size, view, image)))
       ) q;
     ignore (Vgr.render vgr `End);
     Cairo.Surface.finish i

let svg image view file = multi_svg [image,view] file

let svg_via_vg image view file =
  (* export to svg via vg *)
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

let read f =
  let i = open_in (f^".hg") in
  let l = Lexing.from_channel i in
  let l = Parser.file Lexer.token l in
  close_in i;
  List.map (Term.map Info.kvl_to_positionned) l

let write f l =
  let o = open_out (f^".hg") in
  let f = Format.formatter_of_out_channel o in
  Format.fprintf f "%a" (Misc.pp_print_list " ;\n" (Term.pp Full)) l;
  close_out o
    
let export f l =
  let l = List.map (fun t ->
              let g = Graph.of_term t in
              Graph.draw g, Graph.bbox g
            ) l
  in
  multi_pdf l (f^".pdf");
  multi_svg l (f^".svg")

let exists f = Sys.file_exists (f ^ ".hg")
  
