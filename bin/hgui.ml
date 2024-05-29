open Hypergraphs
open Hypergraphs_gtk

open GMain

let term_of_string s =
  let l = Lexing.from_string s in
  let t = Parser.sterm Lexer.token l in
  Term.map Info.kvl_to_positionned t

let string_of_term =
  Format.asprintf "%a" (Term.pp Full) 

let graph_of_string s =
  Graph.of_term (term_of_string s)

let place_graph g =
  Place.sources_on_circle g;
  Place.graphviz g

let place_term t =
  let g = Graph.of_term t in
  place_graph g

(* let center_edges s = *)
(*   let g = graph_of_string s in *)
(*   Graph.iter_edges' (Place.center_edge g) g; *)
(*   let t = Graph.to_term g in *)
(*   Format.asprintf "%a" (Term.pp Full) t *)

let find_ivertex g l =
  match MSet.find (fun i -> i#label = l) (Graph.ivertices g) with
  | None ->
     Format.printf "%a@." (Graph.pp Full) g;
     failwith ("missing inner vertex "^l)
  | Some v -> v     

let is_minimal g x =
  let g = Graph.promote x g in
  MSet.forall
    (fun x -> not (Graph.is_separator g 1 3 [x] || Graph.is_separator g 2 3 [x]))
    (Graph.ivertices g)

let kind g =
  if not (Graph.is_hard g) then `Skip "the graph is not hard"
  else
    let x = find_ivertex g "x" in
    let y = find_ivertex g "y" in
    let z = find_ivertex g "z" in
    let t = find_ivertex g "t" in
    if not (is_minimal g x) then `Skip "x is not minimal" else
    if not (is_minimal g y) then `Skip "y is not minimal" else
    if not (is_minimal g z) then `Skip "z is not minimal" else
    if not (is_minimal g t) then `Skip "t is not minimal" else
    if Graph.is_separator g 1 2 [x;z] then `Skip "{x,z} is a separation pair" else
    if Graph.is_separator g 1 2 [x;t] then `Skip "{x,t} is a separation pair" else
    if Graph.is_separator g 1 2 [y;z] then `Skip "{y,z} is a separation pair" else
    if Graph.is_separator g 1 2 [y;t] then `Skip "{y,t} is a separation pair" else
    let test_seps f g k =
      if not (Graph.is_separator g 1 2 [x;y]) then f "{x,y}" "separation" else
      if not (Graph.is_separator g 1 2 [z;t]) then f "{z,t}" "separation" else
      if not (Graph.width_less_than 3 (List.fold_right Graph.promote [x;y] g)) then f "{x,y}" "forget" else
      if not (Graph.width_less_than 3 (List.fold_right Graph.promote [z;t] g)) then f "{z,t}" "forget" else
      k()
    in
    let g' = Graph.filter_edges (fun e -> (Graph.einfo e)#label <> "?") g in
    test_seps (fun a b -> `Skip (a^" cannot be a "^b^" pair")) g' (fun () ->
    test_seps (fun a b -> `Refine (a^" is not yet a "^b^" pair")) g (fun () ->
        `Axiom
    ))

(* initial window width/height *)
let width = 800
let height = 800

let canvas = new Picture.basic_canvas
let graph = ref (Graph.nil ())
let active = ref `N
let mode = ref `Normal
let file = ref
             (match Sys.argv with
              | [|_|] -> "default"
              | [|_;file|] when File.exists file -> file
              | _ -> Format.eprintf "usage: hgui [file]\n"; exit 1)
let hist = History.create (Stack.of_list[])

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:!file ()
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_factory = new GMenu.factory (factory#add_submenu "File") ~accel_group
let edit_factory = new GMenu.factory (factory#add_submenu "Edit") ~accel_group
let view_factory = new GMenu.factory (factory#add_submenu "View") ~accel_group
let term_factory = new GMenu.factory (factory#add_submenu "Term") ~accel_group

let da = GMisc.drawing_area ~width ~height ~packing:(vbox#pack ~expand:true) ()
let arena = GArena.create ~width ~height ~window da canvas ()
let entry = GEdit.entry ~text:"0" ~editable:true ~packing:(vbox#pack ~expand:false) ()
let label = GMisc.label ~selectable:true ~xalign:0.01 ~justify:`LEFT ~packing:(vbox#pack ~expand:true) ()

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
       file := f;
       k f
    | None -> ()
  ); dlg#misc#hide()

let checkpoint() =
  Format.kasprintf (fun s -> History.save hist (Stack.replace (History.present hist) s)) "%a"
    (Term.pp Full) (Graph.to_term !graph)

let redraw ?(rebox=false) () =
  canvas#clear;
  Graph.draw_on canvas ~iprops:true !graph;
  if rebox then arena#ensure (Graph.bbox !graph);
  arena#refresh

let display_graph_infos g =
  let s = History.present hist in
  let pp_graph_infos f =
    Format.fprintf f "Graph %i/%i\n" (Stack.pos s) (Stack.size s);    
    Format.fprintf f "Treewidth: %i\n" (Graph.width g);
    (match MSet.size (Graph.components g) with
    | 0 -> Format.fprintf f "Empty\n"
    | 1 -> 
       Format.fprintf f
         (if Graph.is_full g then
            if Graph.is_atomic g then "Atomic\n"
            else if Graph.is_hard g then  "Hard\n"
            else "Full prime\n"
          else "Prime\n")
    | n ->
       if Graph.is_full g then Format.fprintf f "Full, ";
       Format.fprintf f "%i components\n" n);
    match kind g with
    | `Skip msg -> Format.fprintf f "Can be skipped: %s" msg
    | `Refine msg -> Format.fprintf f "Should be refined: %s" msg
    | `Axiom -> Format.fprintf f "Axiom?"
  in
  Format.kasprintf label#set_label "%t" pp_graph_infos

let set_graph ?rebox g =
  (* print_endline "set_graph"; *)
  graph := g;
  redraw ?rebox ();
  display_graph_infos g;
  if (match term_of_string entry#text with
      | t -> not (Graph.iso Info.same_label g (Graph.of_term t))
      | exception _ -> true)
  then
    let t = Graph.to_term g in
    assert (Graph.iso Info.same_label g (Graph.of_term t));
    (* print_endline "set_graph.set_text"; *)
    Format.kasprintf entry#set_text "%a" (Term.pp Sparse) t

let undo _ =
  match History.undo hist with
  | Some s -> set_graph (graph_of_string (Stack.current s))
  | None -> print_endline "no more undos"

let redo _ =
  match History.redo hist with
  | Some s -> set_graph (graph_of_string (Stack.current s))
  | None -> print_endline "no more redos"

let on_graph f =
  set_graph (f !graph);
  if !mode = `Normal then checkpoint()

let text_changed _ =
  (* print_endline "text_changed"; *)
  active := `N;
  match term_of_string entry#text with
  | r ->
     let g = Graph.of_term r in
     if not (Graph.iso Info.same_label g !graph) then (
       (* print_endline "text_changed.really"; *)
       place_graph g;
       graph := g;
       active := `N;
       redraw ~rebox:true ();
       display_graph_infos g;
       checkpoint())
     else
       display_graph_infos !graph
  | exception (Failure s) -> label#set_label s
  | exception Parser.Error -> label#set_label "Parsing error"
  | exception e -> label#set_label (Printexc.to_string e)

let set_stack ?rebox stack =
  (* print_endline "set_stack"; *)
  History.save ~cmp:Stack.same hist stack;
  set_graph ?rebox (graph_of_string (Stack.current stack))

let load_from file =
  let l = File.read file in
  List.iter place_term l;
  let s = Stack.of_list (List.map string_of_term l) in
  set_stack ~rebox:true s;
  checkpoint();
  History.clear hist  

let save_to file =
  let s = Stack.to_list (History.present hist) in
  let s = List.map term_of_string s in
  File.write file s;
  File.export file s

let load =
  dialog "Open graph file" `OPEN `OPEN `OPEN 
    (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ()) load_from

let save() =
  save_to !file

let save_as =
  dialog "Save graphs as" `SAVE `SAVE `SAVE 
    (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ()) save_to

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

let center() =
  match catch() with
  | `E e -> Place.center_edge !graph e; checkpoint(); redraw()
  | _ -> ()

let subst e s =
  let h = graph_of_string s in
  let g,es = Graph.subst_edge !graph e h in
  Graph.iter_ivertices (fun i -> i#move (Graph.einfo e)#pos) h;
  MSet.iter (Place.center_edge g) es;
  Format.asprintf "%a" (Term.pp Full) (Graph.to_term g)

let split ~opt =
  match catch() with
  | `E e ->
     let x = Graph.einfo e in
     if x#label <> "?"
     then print_endline "this edge cannot be split"
     else (
       match Seq.size (Graph.neighbours e) with
       | 3 -> 
          let c = match x#get "color" with Some c -> c | None -> "gray" in
          let r = match x#get "radius" with Some r -> r | None -> string_of_float (Constants.eradius 3) in
          let split = match x#get "split" with Some s -> s | None -> "s123" in
          let sel c = not opt || String.contains split c in
          let s = History.present hist in
          let s =
            if sel '3' then
              Stack.push_right s (Format.kasprintf (subst e) "{31}?<color=%s>|{32}?<color=%s>" c c)
            else s in
          let s =
            if sel '2' then
              Stack.push_right s (Format.kasprintf (subst e) "{21}?<color=%s>|{23}?<color=%s>" c c)
            else s in
          let s =
            if sel '1' then
              Stack.push_right s (Format.kasprintf (subst e) "{12}?<color=%s>|{13}?<color=%s>" c c)
            else s in
          let s =
            if sel 's' then
              Stack.push_right s (Format.kasprintf (subst e) "*(-<color=%s>,-<color=%s>,-<color=%s>)" c c c)
            else s in
          let s = Stack.replace s (Format.kasprintf (subst e) "#3 -<color=%s;radius=%s>" c r) in
          set_stack s 
       | 2 -> 
          let c = match x#get "color" with Some c -> c | None -> "gray" in
          let s = History.present hist in
          let s = Stack.replace s (Format.kasprintf (subst e) "#2 -<color=%s>" c) in
          let s = Stack.push_here s (subst e "#2 0") in
          set_stack s 
       | _ -> print_endline "may only split edges of arity two and three")
  | `V _ -> print_endline "cannot split a vertex"
  | `N -> ()  

let left() = set_stack (Stack.move_left (History.present hist))
let right() = set_stack (Stack.move_right (History.present hist))

let discard ~force =
  if force || match kind !graph with `Skip _ -> true | _ -> false then
    let s = Stack.pop (History.present hist) in
    let s = if Stack.size s = 0 then
              (print_endline "discarded the last graph!"; Stack.of_list ["0"])
            else s
    in set_stack s
  else print_endline "no obvious reason to discard this case"

let duplicate() =
  let s = History.present hist in
  History.save hist (Stack.push_right s (Stack.current s))

let key_press e =
  (match !mode with
   | `Normal ->
      (match GdkEvent.Key.string e with
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
s:     split edge, generating subcases
o:     optimised split edge, generating less subcases according to the \"split\" specification
k:     discard current case (when justification is clear)
K:     discard current case (whatever the situation)
D:     duplicate current case
h:     print this help message"
       | "c" -> center()
       | "-" -> scale (1. /. 1.1)
       | "+" -> scale 1.1
       | "i" -> ignore(ivertex())
       | "l" -> lift()
       | "f" -> forget()
       | "p" -> promote()
       | "d" | "r" -> remove()
       | "e" -> mode := `InsertEdge Seq.empty
       | "s" -> split ~opt:false
       | "o" -> split ~opt:true
       | "k" -> discard ~force:false
       | "K" -> discard ~force:true
       | "D" -> duplicate()
       | _ when GdkEvent.Key.keyval e = GdkKeysyms._Left -> left()
       | _ when GdkEvent.Key.keyval e = GdkKeysyms._Right -> right()
       | "" -> ()
       | s -> Format.printf "skipping key %s@." s)
   | `InsertEdge l ->
      (match GdkEvent.Key.string e with
       | "-" | "" -> mode := `Normal; edge l ""
       | s -> mode := `Normal; edge l s)
  ); true

let fullscreen =
  let fs = ref false in
  fun _ ->
  if !fs then window#unfullscreen() else window#fullscreen();
  fs := not !fs

let on_term f () =
   match term_of_string entry#text with
   | t -> Format.kasprintf entry#set_text "%a" (Term.pp Sparse) (f t)
   | exception _ -> print_endline "current term is not valid"

let _ = file_factory#add_item "Open" ~key:GdkKeysyms._O ~callback:load
let _ = file_factory#add_item "Save" ~key:GdkKeysyms._S ~callback:save
let _ = file_factory#add_item "Save as" ~key:GdkKeysyms._E ~callback:save_as
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
let _ = load_from !file
let _ = window#show ()
let _ = Main.main ()
