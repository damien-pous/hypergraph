open Hypergraphs
open Misc
open Conversions

open GMain

(* initial width/height *)
let width = 400
let height = 400

let canvas = new Picture.basic_canvas
let graph = ref (Graph.nil ())
let active = ref `N
let mode = ref `Normal
let text = "a|lb|(23)lc|(13)l(fd)"

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:"HG" ()
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()
let da = GMisc.drawing_area ~width ~height ~packing:vbox#add ()
let arena = GArena.create ~width ~height ~window da canvas ()
let entry = GEdit.entry ~text ~editable:true ~packing:(vbox#pack ~expand:false) ()
let label = GMisc.label ~selectable:true ~xalign:0.01 ~height:80 ~justify:`LEFT ~packing:(vbox#pack ~expand:false) ()

let export_pdf () =
  let view = Graph.bbox !graph in
  Picture.pdf canvas#get view "test.pdf";
  print_endline "exported to test.pdf"

let export_svg () =
  let view = Graph.bbox !graph in
  Picture.svg canvas#get view "test.svg";
  print_endline "exported to test.svg"

(* recomputing the picture and rendering it *)
let redraw() =
  canvas#clear;
  Graph.draw_on canvas !graph;
  arena#refresh

let relabel msg =
  let g = !graph in
  let t = raw_of_graph g in
  let n = normalise (term_of_raw t) in
  Format.kasprintf label#set_label
    "%sExtracted term: %a\nNormalised term: %a" msg
    (Raw.pp Sparse) t (NTerm.pp Sparse) n;
  let _ = Graph.iso Info.same_label g (graph_of_raw t) ||
            (Format.eprintf "%a" (Graph.pp Sparse) g;
             failwith "Mismatch between graph and extracted term")
  in
  let _ = Graph.iso Info.same_label g (graph_of_nterm n) ||
            failwith "Mismatch between graph and normalised term"
  in
  ()

let set_graph _ =
  try
    let l = Lexing.from_string entry#text in
    let r = Parser.sterm Lexer.token l in
    let r = Raw.map Info.kvl_to_positionned r in
    let t = term_of_raw r in
    let n = nterm_of_raw r in
    Format.kasprintf label#set_label
      "Parsed term: %a\nPlain term: %a\nNormalised term: %a"
      (Raw.pp Sparse) r
      (Term.pp Sparse) t
      (NTerm.pp Sparse) n;
    let g = graph_of_raw r in
    Place.sources_on_circle g;
    Place.graphviz g;
    graph := g;
    active := `N;
    canvas#clear;
    Graph.draw_on canvas g;
    arena#ensure (Graph.bbox g)
  with e -> relabel "Parsing error\n"; raise e

let catch() = Graph.find (Geometry.inside arena#pointer) !graph

let ivertex() =
  let v = Info.positionned_ivertex arena#pointer in
  graph := Graph.add_ivertex v !graph;
  redraw();
  relabel "";
  v

let button_press ev =
  let state = GdkEvent.Button.state ev in
  not (Gdk.Convert.test_modifier `CONTROL state) &&
  match !mode, catch() with
   | `Normal, `V x -> active := `V x; true
   | `Normal, `E x -> active := `E x; true
   | `InsertEdge l, `V v -> mode := `InsertEdge (Seq.snoc l v); true
   | `InsertEdge l, `N ->
        let v = ivertex() in
        mode := `InsertEdge (Seq.snoc l (Inn v)); true
   | _ -> false
    
let button_release _ =
  active := `N; false

let motion_notify _ =
  match !active with
  | `V v ->
     (Graph.vinfo !graph v)#move arena#pointer;
     Graph.iter_edges'' (fun e _ n-> if Seq.mem v n then Place.center_edge !graph e) !graph;
     redraw();
     true
  | `E e ->
     (Graph.einfo e)#move arena#pointer;
     redraw();
     true
  | `N -> false

let scale s =
  match catch() with
  | `V v -> (Graph.vinfo !graph v)#scale s; redraw()
  | `E e -> (Graph.einfo e)#scale s; redraw()
  | `N -> ()

let lift() =
  graph := Graph.lft (Info.positionned_source (Graph.arity !graph+1) arena#pointer) !graph;
  redraw(); relabel ""

let remove() =
  match catch() with
  | `V v -> graph := Graph.rem_vertex v !graph; redraw(); relabel ""
  | `E e -> graph := Graph.rem_edge e !graph; redraw(); relabel ""
  | `N -> ()

let promote() =
  match catch() with
  | `V (Inn v) -> graph := Graph.promote v !graph; redraw(); relabel ""
  | `V (Src _) -> print_endline "cannot promote a source"
  | `E _ -> print_endline "cannot promote an edge"
  | `N -> ()

let forget() =
  match catch() with
  | `V (Src i) -> graph := Graph.forget i !graph; redraw(); relabel ""
  | `V (Inn _) -> print_endline "cannot forget an inner vertex (use r to remove it)"
  | `E _ -> print_endline "cannot forget an edge (use r to remove it)"
  | _ -> ()

let edge l s =
  let e = Info.positionned_edge (Seq.size l) s in
  let e,g = Graph.add_edge e l !graph in
  graph := g;
  Place.center_edge g e;
  redraw(); relabel ""

let center() =
  match catch() with
  | `E e -> Place.center_edge !graph e; redraw()
  | _ -> ()
  
let key_press e =
  (match !mode with
   | `Normal ->
      (match GdkEvent.Key.string e with
       | "q" -> Main.quit()
       | "o" -> export_pdf(); export_svg()
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
          [ `KEY_PRESS; `BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE ]
let _ = da#misc#set_can_focus true
let _ = da#event#connect#motion_notify ~callback:motion_notify
let _ = da#event#connect#button_press ~callback:button_press
let _ = da#event#connect#button_release ~callback:button_release
let _ = arena#enable_moves
let _ = da#event#connect #key_press ~callback:key_press
let _ = entry#connect#changed ~callback:set_graph
let _ = window#connect#destroy ~callback:Main.quit
let _ =
  set_graph ();
  window#show ();
  Main.main ()
