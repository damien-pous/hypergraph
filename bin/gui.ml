open Hypergraphs

open GMain

(* initial width/height *)
let width = 400
let height = 400

let canvas = new Picture.basic_canvas
let graph = ref (Graph.nil ())
let active = ref `N
let mode = ref `Normal
let file = ref None
let text = "a|lb|(23)lc|(13)l(fd)"

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:"HG" ()
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_menu = factory#add_submenu "File"
let file_factory = new GMenu.factory file_menu ~accel_group

let da = GMisc.drawing_area ~width ~height ~packing:vbox#add ()
let arena = GArena.create ~width ~height ~window da canvas ()
let entry = GEdit.entry ~text ~editable:true ~packing:(vbox#pack ~expand:false) ()
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

let current_term k =
  try
    let l = Lexing.from_string entry#text in
    let t = Parser.sterm Lexer.token l in
    Some (Term.map Info.kvl_to_positionned t)
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
    | 1 -> (
      if Graph.is_full g then
        if Graph.is_hard g then Format.fprintf f "Hard"
        else Format.fprintf f "Full prime"
      else Format.fprintf f "Prime"
    )
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

let on_graph f = set_graph (f !graph)

let text_changed _ =
  match current_term (fun _ -> label#set_label "Parsing error\n") with
  | Some r ->
     let g = Graph.of_term r in
     display_graph_infos g;
     Place.sources_on_circle g;
     Place.graphviz g;
     graph := g;
     active := `N;
     canvas#clear;
     Graph.draw_on canvas ~iprops:true g;
     arena#ensure (Graph.bbox g)
  | None -> ()

let load =
  dialog "Open graph file" `OPEN `OPEN `OPEN 
    (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ())
    (fun file ->
      let l = File.read file in
      Format.kasprintf entry#set_text "%a" (Term.pp Sparse) (File.first l);
      set_graph (Graph.of_term (File.last l)))

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
  | _ -> ()

let edge l s =
  let e = Info.positionned_edge (Seq.size l) s in
  let e,g = Graph.add_edge e l !graph in
  Place.center_edge g e;
  set_graph g

let center() =
  match catch() with
  | `E e -> Place.center_edge !graph e; redraw()
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
       | "e" -> mode := `InsertEdge Seq.empty
       | _ -> ())
   | `InsertEdge l ->
      (match GdkEvent.Key.string e with
       | ("a" | "b" | "c" | "d" | "e") as s -> mode := `Normal; edge l s
       | _ -> mode := `Normal; edge l "")
  ); true

let _ = file_factory#add_item "Open" ~key:GdkKeysyms._O ~callback:load
let _ = file_factory#add_item "Save" ~key:GdkKeysyms._S ~callback:save
let _ = file_factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit

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
let _ = text_changed ()
let _ = window#show ()
let _ = Main.main ()
