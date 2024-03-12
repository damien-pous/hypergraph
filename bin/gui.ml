open Hypergraphs
open GMain

open Gg
open Vg
open Conversions

(* sanity checks *)
(* open Sanity *)

let graph = new Drawable.graph
let active = ref `N
let mode = ref `Normal
let view_center = ref V2.zero
let zoom = ref 400.
let picture = ref I.void

let text = "a|lb|(23)lc|(13)l(f<pos=.8,.8>d)"

(* initial width/height*)
let width = 800
let height = 800

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:"HG" ()
let backing = ref (GDraw.pixmap ~width ~height ~window ())
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()
let da = GMisc.drawing_area ~width ~height ~packing:vbox#add ()
let entry = GEdit.entry ~text ~editable:true ~packing:(vbox#pack ~expand:false) ()
let label = GMisc.label ~selectable:true ~xalign:0.01 ~height:80 ~justify:`LEFT ~packing:(vbox#pack ~expand:false) ()

let da_size() =
  let w,h = !backing#size in
  float_of_int w, float_of_int h
let da_size'() =
  let w,h = da_size() in Size2.v w h

let view() =
  let s = da_size'() in
  Box2.v_mid !view_center (V2.(/) s !zoom)

let p2_of_pointer (x,y) =
  let w,h = da_size() in
  (* p is (x,y) in [0;1]x[0;1] *)
  let p = P2.v
            (float_of_int x /. w)
            (1. -. float_of_int y /. h)
  in
  let v = view() in
  V2.add (Box2.o v) (V2.mul p (Box2.size v))

let v2_of_pointer_shift dx dy =
  let w,h = da_size() in
  let p = V2.v (dx /. w) (-. dy /. h) in
  V2.mul p (V2.(/) (da_size'()) !zoom)

let pointer_of_p2 p =
  let w,h = da_size() in
  let v = view() in
  let p = V2.div (V2.sub p (Box2.o v)) (Box2.size v) in
  let x',y' = V2.x p, V2.y p in
  (w *. x', h *. (1.-.y'))

(* rendering the picture *)
let render () =
  let cr = Cairo_gtk.create !backing#pixmap in  
  let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
  let size = da_size'() in
  let view = view() in
  let drawing = I.blend !picture (I.const Color.white) in
  let _ = Vgr.render vgr (`Image (size, view, drawing)) in
  let _ = Vgr.render vgr (`End) in
  if Constants.render_labels_with_cairo then
    begin
      Cairo.set_source_rgb cr 0.0 0.0 0.0;
      Cairo.select_font_face cr "Latin Modern Roman" ~slant:Cairo.Italic;
      Cairo.set_font_size cr (Constants.fontsize *. !zoom);
      let text p s =
        let (x,y) = pointer_of_p2 p in
        let te = Cairo.text_extents cr s in
        Cairo.move_to cr
          (x -. te.width /. 2. -. te.x_bearing)
          (y -. te.height /. 2. -. te.y_bearing);
        Cairo.show_text cr s
      in
      graph#iter_infos (fun i -> text (Info.pos i) (Info.label i))
    end;
  da#misc#draw None

let export_pdf () =
  (* export via cairo (could not find how to get the right fonts with vg) *)
  let w,h = 100.,100. in
  let size = Size2.v w h (* mm *) in
  let view = view() in
  let image = !picture in
  let i = Cairo.PDF.create "test.pdf" ~w ~h in
  let cr = Cairo.create i in
  let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
  ignore (Vgr.render vgr (`Image (size, view, image)));
  ignore (Vgr.render vgr `End);
  Cairo.Surface.finish i;
  print_endline "exported to test.pdf"

let export_svg() =
  (* export via vg (also possible via cairo as above) *)
  let size = Size2.v 100. 100. (* mm *) in
  let view = view() in
  let image = !picture in
  let o = open_out_bin "test.svg" in
  let title = "some nice graph" in
  let description = entry#text in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_svg.target ~xmp ()) (`Channel o) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  close_out o;
  print_endline "exported to test.svg"

(* recomputing the picture and rendering it *)
let redraw() = picture := graph#draw; render()

let relabel msg =
  let g = snd graph#sgraph in
  let t = raw_of_graph g in
  let n = normalise (term_of_raw t) in
  Format.kasprintf label#set_label
    "%sExtracted term: %a\nNormalised term: %a" msg
    Raw.pp t NTerm.pp n;
  let same_label x y = Info.label x = Info.label y in
  let _ = Graph.iso same_label g (graph_of_raw t) ||
            (Format.eprintf "%a" Graph.pp g; failwith "Mismatch between graph and extracted term")
  in
  let _ = Graph.iso same_label g (graph_of_nterm n) ||
            failwith "Mismatch between graph and normalised term"
  in
  ()

let set_graph _ =
  try
    let l = Lexing.from_string entry#text in
    let r = Parser.main Lexer.token l in
    let g = Place.circle_random (graph_of_raw r) in
    let t = term_of_raw r in
    let n = nterm_of_raw r in
    Format.kasprintf label#set_label
      "Parsed term: %a\nPlain term: %a\nNormalised term: %a"
      Raw.pp r
      Term.pp t
      NTerm.pp n;
    graph#set g;
    active := `N;
    redraw()
  with e -> relabel "Parsing error\n"; raise e
  
let pointer() = p2_of_pointer da#misc#pointer

let catch() = graph#find (Info.inside (pointer()))

let ivertex() =
  let v = graph#add_ivertex (Info.for_ivertex (pointer())) in
  redraw(); relabel "";
  v

let button_press e =
  (match !mode with
   | `Normal ->
      active := (match catch() with
                   | `V x -> `V x | `E x -> `E x
                   | `N -> `C (!view_center, GdkEvent.Button.x e, GdkEvent.Button.y e))
  | `InsertEdge l ->
     match catch() with
     | `V (v,_) -> mode := `InsertEdge (Seq.snoc l v)
     | `E _ -> ()
     | `N ->
        let v = ivertex() in
        mode := `InsertEdge (Seq.snoc l (Inn v))); true
    
let button_release _ =
  active := `N; true

let motion_notify e =
  match !active with
  | `V (v,i) ->
     Info.set_pos i (pointer());
     Graph.iter_edges (fun e _ n -> if Seq.mem v n then Graph.Sourced.center_edge graph#sgraph e)
       graph#graph;
     redraw(); true
  | `E (_,i) -> Info.set_pos i (pointer()); redraw(); true
  | `C (vc0,x0,y0) ->
     view_center := V2.sub vc0
                      (v2_of_pointer_shift
                         (GdkEvent.Motion.x e -. x0)
                         (GdkEvent.Motion.y e -. y0));
     render(); true
  | `N -> false

let scroll e =
  (* TODO: update view_center so that we zoom around the pointer  *)
  match GdkEvent.Scroll.direction e with
  | `UP -> zoom := !zoom *. 1.1; render(); true
  | `DOWN -> zoom := !zoom /. 1.1; render(); true
  | _ -> false

let scale s =
  match catch() with
  | `V (_,i)
  | `E (_,i) -> Info.set_scale i (Info.scale i *. s); redraw()
  | `N -> ()

let lift() =
  graph#lift (Info.for_source (pointer()) (graph#arity+1)); redraw(); relabel ""

let remove() =
  match catch() with
  | `V (v,_) -> graph#rem_vertex v; redraw(); relabel ""
  | `E (e,_) -> graph#rem_edge e; redraw(); relabel ""
  | `N -> ()

let promote() =
  match catch() with
  | `V (Inn v,_) -> graph#promote v; redraw(); relabel ""
  | `V (Src _,_) -> print_endline "cannot promote a source"
  | `E _ -> print_endline "cannot promote an edge"
  | `N -> ()

let forget() =
  match catch() with
  | `V (Src i,_) -> graph#forget i; redraw(); relabel ""
  | `V (Inn _,_) -> print_endline "cannot forget an inner vertex (use r to remove it)"
  | `E _ -> print_endline "cannot forget an edge (use r to remove it)"
  | _ -> ()

let edge l s =
  ignore(graph#add_edge (Info.for_edge s (Seq.map graph#vinfo l)) l);
  redraw(); relabel ""

let center() =
  match catch() with
  | `E (e,_) -> Graph.Sourced.center_edge graph#sgraph e; redraw()
  | _ ->  view_center := pointer()
  
let key_press e =
  (match !mode with
   | `Normal ->
      (match GdkEvent.Key.string e with
       | "q" -> Main.quit()
       | "o" -> export_svg(); export_pdf()
       | "c" -> center()
       | "-" -> scale (1. /. 1.1)
       | "+" -> scale 1.1
       | "i" -> ignore(ivertex())
       | "l" -> lift()
       | "f" -> forget()
       | "p" -> promote()
       | "d" | "r" -> remove()
       | "e" -> mode := `InsertEdge Seq.empty
       | _ -> ())
   | `InsertEdge l ->
      (match GdkEvent.Key.string e with
       | "q" -> Main.quit()
       | ("a" | "b" | "c" | "d" | "e") as s -> mode := `Normal; edge l s
       | _ -> mode := `Normal; edge l "")
  ); true


let _ = GtkBase.Widget.add_events da#as_widget
          [ `KEY_PRESS;
            `BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE;
            `SCROLL
          ]
let _ = da#misc#set_can_focus true
let _ = da#event#connect#motion_notify ~callback:motion_notify
let _ = da#event#connect#scroll ~callback:scroll
let _ = da#event#connect#button_press ~callback:button_press
let _ = da#event#connect#button_release ~callback:button_release
let _ = entry#connect#changed ~callback:set_graph
let _ = window#connect#destroy ~callback:Main.quit
let _ = da#event#connect #key_press ~callback:key_press
let _ =
  da#event#connect#expose ~callback:(fun ev ->
      let area = GdkEvent.Expose.area ev in
      let x = Gdk.Rectangle.x area in
      let y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area in
      let height = Gdk.Rectangle.width area in
      let drawing =
        da#misc#realize ();
        new GDraw.drawable da#misc#window
      in
      drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
      false)
let _ =
  da#event#connect#configure ~callback:(fun ev ->
      let width = GdkEvent.Configure.width ev in
      let height = GdkEvent.Configure.height ev in
      let pixmap = GDraw.pixmap ~width ~height ~window () in
      backing := pixmap;
      render();
      false)
let _ =
  window#show ();
  set_graph ();
  Main.main ()
