open Hypergraphs
open Hypergraphs_gtk
open Hypergraphs_place
open Hypergraphs_cairo

open GMain

(* initial width/height *)
let width = 400
let height = 400

let canvas = new Picture.basic_canvas
let graph = ref (Graph.nil ())
let active = ref `N
let mode = ref `Normal
let file = ref None
let text = "a|lb|(23)lc|(13)lfd"
let hist = History.create text

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:"HG" ()
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_factory = new GMenu.factory (factory#add_submenu "File") ~accel_group
let edit_factory = new GMenu.factory (factory#add_submenu "Edit") ~accel_group
let view_factory = new GMenu.factory (factory#add_submenu "View") ~accel_group
let term_factory = new GMenu.factory (factory#add_submenu "Term") ~accel_group

let da = GMisc.drawing_area ~width ~height ~packing:vbox#add ()
let arena = GArena.create ~width ~height ~window da canvas ()
let entry = GEdit.entry ~editable:true ~packing:(vbox#pack ~expand:false) ()
let label = GMisc.label ~selectable:true ~xalign:0.01 ~height:40 ~justify:`LEFT ~packing:(vbox#pack ~expand:false) ()

let dialog title action stock stock' filter =
  let dlg = GWindow.file_chooser_dialog
    ~action ~title
    ~parent:window
    ~position:`CENTER_ON_PARENT
    ~destroy_with_parent:true ()
  in
  dlg#add_button_stock `CANCEL `CANCEL;
  dlg#add_select_button_stock stock stock';
  dlg#add_filter filter;
  ignore(dlg#set_current_folder ".");
  fun k () ->
  if dlg#run() = stock' then (
    match dlg#filename with
    | Some f ->
       let f = Filename.chop_extension f in
       window#set_title (Filename.basename f);
       file := Some f;
       k f
    | None -> ()
  ); dlg#misc#hide()

let checkpoint() =
  Format.kasprintf (History.save hist) "%a" (Term.pp Full) (Graph.to_term !graph)

let term_of_string s =
  let l = Lexing.from_string s in
  let t = Parser.sterm Lexer.token l in
  Term.map Info.kvl_to_positionned t

let graph_of_string s =
  Graph.of_term (term_of_string s)

let current_term k =
  try Some (term_of_string entry#text)
  with e -> k e; None

let redraw() =
  canvas#clear;
  Graph.draw_on canvas ~iprops:true !graph;
  arena#refresh

let display_graph_infos g =
  let pp_graph_infos f =
    Format.fprintf f "Treewidth: %i\n" (Graph.width g);
    match MSet.size (Graph.components g) with
    | 0 -> Format.fprintf f "Empty"
    | 1 -> 
       Format.fprintf f
         (if Graph.is_full g then
            if Graph.is_atomic g then "Atomic"
            else if Graph.is_hard g then  "Hard"
            else "Full prime"
          else "Prime")
    | n ->
       if Graph.is_full g then Format.fprintf f "Full, ";
       Format.fprintf f "%i components" n
  in
  Format.kasprintf label#set_label "%t" pp_graph_infos

let set_graph g =
  graph := g;
  redraw();
  if (match current_term (fun _ -> ()) with
      | Some t -> not (Graph.iso Info.same_label g (Graph.of_term t))
      | None -> true)
  then
    let t = Graph.to_term g in
    assert (Graph.iso Info.same_label g (Graph.of_term t));
    Format.kasprintf entry#set_text "%a" (Term.pp Sparse) t;
    display_graph_infos g

let undo _ =
  match History.undo hist with
  | Some g -> set_graph (graph_of_string g)
  | None -> print_endline "no more undos"

let redo _ =
  match History.redo hist with
  | Some g -> set_graph (graph_of_string g)
  | None -> print_endline "no more redos"

let on_graph f =
  set_graph (f !graph);
  if !mode = `Normal then checkpoint()

let text_changed _ =
  active := `N;
  match current_term (fun _ -> label#set_label "Parsing error\n") with
  | Some r ->
     let g = Graph.of_term r in
     display_graph_infos g;
     if not (Graph.iso Info.same_label g !graph) then (
       Place.sources_on_circle g;
       Place.graphviz g;
       graph := g;
       active := `N;
       canvas#clear;
       Graph.draw_on canvas ~iprops:true g;
       arena#ensure (Graph.bbox g);
       checkpoint())
  | None -> ()

let load =
  dialog "Open graph file" `OPEN `OPEN `OPEN 
    (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ())
    (fun file ->      
      let l = File.read file in
      Format.kasprintf entry#set_text "%a" (Term.pp Sparse) (File.first l);
      set_graph (Graph.of_term (File.last l));
      checkpoint();
      History.clear hist)

let save_to f =
  match current_term (fun _ -> failwith "cannot save while there are parsing errors") with
  | Some t -> 
     let l =
       if File.exists f then (
         let old = File.read f in
         if not (File.compatible old t) then failwith ("current graph is incompatible with the one in "^f);
         old
       ) else File.single t
     in
     let t' = Graph.to_term !graph in
     let l = File.append l t' in
     File.write f l;
     File.export_term f t'
  | None -> ()

let save =
  let dlg =
    dialog "Save graph to"
      `SAVE `SAVE `SAVE 
      (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ())
  in
  fun () ->
  match !file with
  | Some file -> save_to file
  | None -> dlg save_to ()


let catch() = Graph.find (Geometry.inside arena#pointer) !graph

let ivertex() =
  let v = Info.positionned_ivertex arena#pointer in
  on_graph (Graph.add_ivertex v);
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
  (match !active with `V _ | `E _ -> checkpoint() | `N -> ());
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
  | `V v -> (Graph.vinfo !graph v)#scale s; checkpoint(); redraw()
  | `E e -> (Graph.einfo e)#scale s; checkpoint(); redraw()
  | `N -> ()

let lift() =
  on_graph (Graph.lft (Info.positionned_source (Graph.arity !graph+1) arena#pointer))

let remove() =
  match catch() with
  | `V v -> on_graph (Graph.rem_vertex v)
  | `E e -> on_graph (Graph.rem_edge e)
  | `N -> ()

let promote() =
  match catch() with
  | `V (Inn v) -> on_graph (Graph.promote v)
  | `V (Src _) -> print_endline "cannot promote a source"
  | `E _ -> print_endline "cannot promote an edge"
  | `N -> ()

let forget() =
  match catch() with
  | `V (Src i) -> on_graph (Graph.forget i)
  | `V (Inn _) -> print_endline "cannot forget an inner vertex (use r to remove it)"
  | `E _ -> print_endline "cannot forget an edge (use r to remove it)"
  | `N -> ()

let edge l s =
  let e = Info.positionned_edge (Seq.size l) s in
  let e,g = Graph.add_edge e l !graph in
  Place.center_edge g e;
  set_graph g;
  checkpoint()

let subst s =
  let h = graph_of_string s in
  match catch() with
  | `E e ->
     let c =
       let g = !graph in
       Geometry.center (Seq.lmap (fun i -> (Graph.vinfo g i)#pos) (Graph.neighbours e))
     in
     on_graph (fun g ->
         let g,es = Graph.subst_edge g e h in
         Graph.iter_ivertices (fun i -> i#move c) h;
         MSet.iter (Place.center_edge g) es;
         g)
  | `V _ -> print_endline "cannot substitute a vertex"
  | `N -> ()

let center() =
  match catch() with
  | `E e -> Place.center_edge !graph e; checkpoint(); redraw()
  | _ -> ()
  
let key_press e =
  (match !mode with
   | `Normal ->
      (match GdkEvent.Key.string e with
       | "c" -> center()
       | "-" -> scale (1. /. 1.1)
       | "+" -> scale 1.1
       | "i" -> ignore(ivertex())
       | "l" -> lift()
       | "f" -> forget()
       | "p" -> promote()
       | "d" | "r" -> remove()
       | "s" -> subst "*(-,-,-)"
       | "1" -> subst "{12}-|{13}-"
       | "2" -> subst "{21}-|{23}-"
       | "3" -> subst "{31}-|{32}-"
       | "e" -> mode := `InsertEdge Seq.empty
       | "h" -> print_endline 
                  "** keys **
c:     center edge
-/+:   shrink/enlarge element
i:     add inner vertex
l:     add new source (lift)
f:     forget source 
p:     promote inner vertex as source
d/r:   remove element
e:     insert edge (click on the sequence of neighbours, then press a,b,c,d,e,- to name the edge)
s:     substitute ternary edge with a star
1/2/3: substitute ternary edge with a V (angle at given neighbour)
h:     print this help message"
       | _ -> ())
   | `InsertEdge l ->
      (match GdkEvent.Key.string e with
       | ("a" | "b" | "c" | "d" | "e") as s -> mode := `Normal; edge l s
       | _ -> mode := `Normal; edge l "")
  ); true

let fullscreen =
  let fs = ref false in
  fun _ ->
  if !fs then window#unfullscreen() else window#fullscreen();
  fs := not !fs

let on_term f () =
   match current_term (fun _ -> failwith "cannot work on the term while there are parsing errors") with
   | Some t -> Format.kasprintf entry#set_text "%a" (Term.pp Sparse) (f t)
   | None -> ()                       

let _ = file_factory#add_item "Open" ~key:GdkKeysyms._O ~callback:load
let _ = file_factory#add_item "Save" ~key:GdkKeysyms._S ~callback:save
let _ = file_factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit
let _ = edit_factory#add_item "Undo" ~key:GdkKeysyms._Z ~callback:undo
let _ = edit_factory#add_item "Redo" ~key:GdkKeysyms._R ~callback:redo
let _ = view_factory#add_item "Fullscreen" ~key:GdkKeysyms._F ~callback:fullscreen
let _ = term_factory#add_item "Normalise" ~key:GdkKeysyms._N ~callback:(on_term NTerm.get)
let _ = term_factory#add_item "Desugar" ~key:GdkKeysyms._D ~callback:(on_term PTerm.get)

let _ = GtkBase.Widget.add_events da#as_widget
          [ `KEY_PRESS; `BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE ]
let _ = da#misc#set_can_focus true
let _ = da#event#connect#motion_notify ~callback:motion_notify
let _ = da#event#connect#button_press ~callback:button_press
let _ = da#event#connect#button_release ~callback:button_release
let _ = arena#enable_moves
let _ = da#event#connect #key_press ~callback:key_press
let _ = entry#connect#changed ~callback:text_changed
let _ = window#connect#destroy ~callback:Main.quit
let _ = window#add_accel_group accel_group
let _ = entry#set_text text; History.clear hist
let _ = window#show ()
let _ = Main.main ()
