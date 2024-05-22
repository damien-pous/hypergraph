open Hypergraphs
open Hypergraphs_gtk
open Hypergraphs_place
open Hypergraphs_cairo

open GMain

module Stack: sig
  type 'a t
  val create: 'a -> 'a t
  val current: 'a t -> 'a
  val left: 'a t -> 'a t option
  val right: 'a t -> 'a t option
  val pop: 'a t -> 'a t option
  val replace: 'a t -> 'a -> 'a t
  val push: 'a t -> 'a -> 'a t
  val push_here: 'a t -> 'a -> 'a t
  val pos: 'a t -> int
  val size: 'a t -> int
  (* val count: ('a -> bool) -> 'a t -> int *)
end = struct
  type 'a t = { left: 'a list; here: 'a; right: 'a list }
  let create here = { left=[]; here; right=[] }
  let current x = x.here
  let left x =
    match x.left with
    | [] -> None
    | here::left -> Some { left; here; right=x.here::x.right }
  let right x =
    match x.right with
    | [] -> None
    | here::right -> Some { left=x.here::x.left; here; right }
  let pop x =
    match x.left,x.right with
    | [],[] -> None
    | left,here::right | here::left,right -> Some { left; here; right }
  let replace x here = { x with here }
  let push x next  = { x with right = next::x.right }
  let push_here x here  = { x with here; right = x.here::x.right }
  let pos x = List.length x.left + 1
  let size x = List.length x.left + 1 + List.length x.right
  (* let count f x = List.length (List.filter f (x.left @ x.here :: x.right)) *)
end


let term_of_string s =
  let l = Lexing.from_string s in
  let t = Parser.sterm Lexer.token l in
  Term.map Info.kvl_to_positionned t

let graph_of_string s =
  Graph.of_term (term_of_string s)

let center_edges s =
  let g = graph_of_string s in
  Graph.iter_edges' (Place.center_edge g) g;
  let t = Graph.to_term g in
  Format.asprintf "%a" (Term.pp Full) t

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
    let g' = Graph.filter_edges (fun e -> (Graph.einfo e)#get "clique" = Some "true") g in
    test_seps (fun a b -> `Skip (a^" cannot be a "^b^" pair")) g' (fun () ->
    if not (is_minimal g x) then `Skip "x is not minimal" else
    if not (is_minimal g y) then `Skip "y is not minimal" else
    if not (is_minimal g z) then `Skip "z is not minimal" else
    if not (is_minimal g t) then `Skip "t is not minimal" else
    test_seps (fun a b -> `Refine (a^" is not yet a "^b^" pair")) g (fun () ->
        `Axiom
    ))

(* initial width/height *)
let width = 800
let height = 800

let canvas = new Picture.basic_canvas
let graph = ref (Graph.nil ())
let active = ref `N
let mode = ref `Normal
let file = ref None

let text = center_edges
             "#<pos=0,0>,<pos=400,0>               f<pos=100,100;label=x;radius=6>(f<pos=100,-100;label=y;radius=6>({234}f<pos=300,100;label=z;radius=6>({234}c | {14}-f<clique=true>) | {234}f<pos=300,-100;label=t;radius=6>({14}g<clique=true> | {234}b) | {134}a<radius=25>))"
let text2 = center_edges
              "#<pos=0,0>,<pos=400,0> f<pos=100,100;label=x;radius=6>(f<pos=100,-100;label=y;radius=6>({134}a<radius=22> | {234}f<pos=200,0>({134}f<pos=300,-100;label=t;radius=6>({134}-f<radius=22> | {234}d) | {124}f<pos=300,100;label=z;radius=6>({134}e<radius=22> | {234}c) | {234}b<radius=22>)))"
let hist =
  let s = Stack.create text in
  let s = Stack.push s text2 in
  History.create s

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
let label = GMisc.label ~selectable:true ~xalign:0.01 ~height:80 ~justify:`LEFT ~packing:(vbox#pack ~expand:false) ()

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
  Format.kasprintf (fun s -> History.save hist (Stack.replace (History.present hist) s)) "%a"
    (Term.pp Full) (Graph.to_term !graph)

let current_term k =
  try Some (term_of_string entry#text)
  with e -> k e; None

let redraw() =
  canvas#clear;
  Graph.draw_on canvas ~iprops:true !graph;
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
    | `Skip msg -> Format.fprintf f "Can be skipped: %s\n" msg
    | `Refine msg -> Format.fprintf f "Should be refined: %s\n" msg
    | `Axiom -> Format.fprintf f "Axiom? \n"
  in
  Format.kasprintf label#set_label "%t" pp_graph_infos

let set_graph g =
  graph := g;
  redraw();
  display_graph_infos g;
  if (match current_term (fun _ -> ()) with
      | Some t -> not (Graph.iso Info.same_label g (Graph.of_term t))
      | None -> true)
  then
    let t = Graph.to_term g in
    assert (Graph.iso Info.same_label g (Graph.of_term t));
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
  active := `N;
  match current_term (fun _ -> label#set_label "Parsing error\n") with
  | Some r ->
     let g = Graph.of_term r in
     if not (Graph.iso Info.same_label g !graph) then (
       display_graph_infos g;
       Place.sources_on_circle g;
       (* Place.graphviz g; *)
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

let set_stack stack =
  History.save hist stack;
  set_graph (graph_of_string (Stack.current stack))

let split() =
  match catch() with
  | `E e ->
     let x = Graph.einfo e in
     if x#get "clique" = Some "true"
     then print_endline "this edge was already split"
     else (
       x#set "clique" "true";
       match Seq.size (Graph.neighbours e) with
       | 3 -> 
          checkpoint();
          let l = Info.escape x#label in
          let s = History.present hist in
          let s = Stack.push s (Format.kasprintf (subst e) "{31}%s|{32}%s" l l) in
          let s = Stack.push s (Format.kasprintf (subst e) "{21}%s|{23}%s" l l) in
          let s = Stack.push s (Format.kasprintf (subst e) "{12}%s|{13}%s" l l) in
          let s = Stack.push s (Format.kasprintf (subst e) "*(%s<clique=true>,%s<clique=true>,%s<clique=true>)" l l l) in
          set_stack s 
       | 2 -> 
          checkpoint();
          let s = History.present hist in
          let s = Stack.push_here s (subst e "#2 0") in
          set_stack s 
       | _ -> print_endline "may only split edges of arity two and three")
  | `V _ -> print_endline "cannot split a vertex"
  | `N -> ()  

let left() =
  match Stack.left (History.present hist) with
  | Some s -> set_stack s
  | None -> print_endline "already on leftmost case"

let right() =
  match Stack.right (History.present hist) with
  | Some s -> set_stack s
  | None -> print_endline "already on rightmost case"

let discard force =
  if force || match kind !graph with `Skip _ -> true | _ -> false then
    match Stack.pop (History.present hist) with
    | Some s -> set_stack s
    | None -> print_endline "congrats: this was the last case!"
  else print_endline "no obvious reason to discard this case"

let duplicate() =
  let s = History.present hist in
  History.save hist (Stack.push s (Stack.current s))

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
       | "s" -> split()
       | "e" -> mode := `InsertEdge Seq.empty
       | "k" -> discard false
       | "K" -> discard true
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
