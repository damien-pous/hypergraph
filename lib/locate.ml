open Types

let term_of_string s =
  let l = Lexing.from_string s in
  let t = Parser.sterm Lexer.token l in
  Term.map Info.kvl_to_positionned t

let string_of_term =
  Format.asprintf "%a" (Term.pp Full)

let graph_of_string s =
  Graph.of_term (term_of_string s)

let place_term t =
  let g = Graph.of_term t in
  Place.automatic g

let find_ivertex g l =
  match MSet.find (fun i -> i#label = l) (Graph.ivertices g) with
  | None -> raise Not_found
  | Some v -> v     

let is_minimal g x =
  let g = Graph.promote x g in
  MSet.forall
    (fun x -> not (Graph.is_separator g 1 3 [x] || Graph.is_separator g 2 3 [x]))
    (Graph.ivertices g)

let kind g =
  match
    if Seq.size (Graph.sources g) <> 2 then raise Not_found;
    List.map (find_ivertex g) ["x";"y";"z";"t"]
  with
  | [x;y;z;t] ->
     if not (Graph.is_hard g) then `Skip "the graph is not hard"
     else if not (is_minimal g x) then `Skip "x is not minimal"
     else if not (is_minimal g y) then `Skip "y is not minimal"
     else if not (is_minimal g z) then `Skip "z is not minimal"
     else if not (is_minimal g t) then `Skip "t is not minimal"
     else if Graph.is_separator g 1 2 [x;z] then `Skip "{x,z} is a separation pair"
     else if Graph.is_separator g 1 2 [x;t] then `Skip "{x,t} is a separation pair"
     else if Graph.is_separator g 1 2 [y;z] then `Skip "{y,z} is a separation pair"
     else if Graph.is_separator g 1 2 [y;t] then `Skip "{y,t} is a separation pair"
     else
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
       `Other "Axiom?"
         ))
  | _ | exception _ -> `Other ""

let subst g e s =
  let h = graph_of_string s in
  let g,es = Graph.subst_edge g e h in
  Graph.iter_ivertices (fun i -> i#move (Graph.einfo e)#pos) h;
  MSet.iter (Place.center_edge g) es;
  Format.asprintf "%a" (Term.pp Full) (Graph.to_term g)

class virtual locate (arena: Types.arena) =
  object(self)
    
    method virtual entry: string
    method virtual set_entry: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual warning: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual info: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual help: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
        
    val hist = History.create (Stack.of_list[])
    val mutable graph = Graph.nil ()
    val mutable active = `N
    val mutable mode = `Normal

    method private checkpoint =
      Format.kasprintf (fun s -> History.save hist (Stack.replace (History.present hist) s)) "%a"
        (Term.pp Full) (Graph.to_term graph)

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      Graph.draw_on arena#canvas ~iprops:true graph;
      if rebox then arena#ensure (Graph.bbox graph);
      arena#refresh

    method private display_graph_infos =
      let s = History.present hist in
      let g = graph in
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
        | `Other msg -> Format.fprintf f "%s" msg
      in
      self#info "%t" pp_graph_infos
    
    method private set_graph ?rebox g =
      (* print_endline "set_graph"; *)
      graph <- g;
      self#redraw ?rebox ();
      self#display_graph_infos;
      if (match term_of_string self#entry with
          | t -> not (Graph.iso Info.same_label g (Graph.of_term t))
          | exception _ -> true)
      then
        let t = Graph.to_term g in
        assert (Graph.iso Info.same_label g (Graph.of_term t));
        (* print_endline "set_graph.set_text"; *)
        self#set_entry "%a" (Term.pp Sparse) t

    method undo () =
      match History.undo hist with
      | Some s -> self#set_graph (graph_of_string (Stack.current s))
      | None -> self#warning "no more undos"

    method redo () =
      match History.redo hist with
      | Some s -> self#set_graph (graph_of_string (Stack.current s))
      | None -> self#warning "no more redos"

    method private on_graph f =
      self#set_graph (f graph);
      if mode = `Normal then self#checkpoint

    method on_entry_changed =
      active <- `N;
      match term_of_string self#entry with
      | r ->
         let g = Graph.of_term r in
         if not (Graph.iso Info.same_label g graph) then (
           (* print_endline "text_changed.really"; *)
           Place.automatic g;
           graph <- g;
           active <- `N;
           self#redraw ~rebox:true ();
           self#display_graph_infos;
           self#checkpoint)
         else
           self#display_graph_infos
      | exception (Failure s) -> self#info "%s" s
      | exception Parser.Error -> self#info "%s" "Parsing error"
      | exception e -> self#info "%s" (Printexc.to_string e)

    method private set_stack ?rebox stack =
      (* print_endline "set_stack"; *)
      History.save ~cmp:Stack.same hist stack;
      self#set_graph ?rebox (graph_of_string (Stack.current stack))

    method private catch =
      Graph.find (Geometry.inside arena#pointer) graph

    method private ivertex =
      let v = Info.positionned_ivertex arena#pointer in
      self#on_graph (Graph.add_ivertex v);
      v

    method private scale s =
      match self#catch with
      | `V v -> (Graph.vinfo graph v)#scale s; self#checkpoint; self#redraw()
      | `E e -> (Graph.einfo e)#scale s; self#checkpoint; self#redraw()
      | `N -> Place.scale s graph; self#checkpoint; self#redraw()

    method private lift =
      self#on_graph (Graph.lft (Info.positionned_source (Graph.arity graph+1) arena#pointer))

    method private remove =
      match self#catch with
      | `V v -> self#on_graph (Graph.rem_vertex v)
      | `E e -> self#on_graph (Graph.rem_edge e)
      | `N -> ()

    method private promote =
      match self#catch with
      | `V (Inn v) -> self#on_graph (Graph.promote v)
      | `V (Src _) -> self#warning "cannot promote a source"
      | `E _ -> self#warning "cannot promote an edge"
      | `N -> ()

    method private forget =
      match self#catch with
      | `V (Src i) -> self#on_graph (Graph.forget i)
      | `V (Inn _) -> self#warning "cannot forget an inner vertex (use r to remove it)"
      | `E _ -> self#warning "cannot forget an edge (use r to remove it)"
      | `N -> ()

    method private edge l s =
      let e = Info.positionned_edge (Seq.size l) s in
      let e,g = Graph.add_edge e l graph in
      Place.center_edge g e;
      self#set_graph g;
      self#checkpoint

    method private center =
      match self#catch with
      | `E e -> Place.center_edge graph e; self#checkpoint; self#redraw()
      | _ -> Place.improve_placement 0.05 graph; self#checkpoint; self#redraw()

    method private block b =
      let f = if b then Place.fix else Place.unfix in
      match self#catch with
      | `E e -> f (Graph.einfo e); self#checkpoint
      | `V v -> f (Graph.vinfo graph v); self#checkpoint
      | _ -> ()

    method private split ~opt =
      match self#catch with
      | `E e ->
         let subst = subst graph e in 
         let x = Graph.einfo e in
         if x#label <> "?"
         then self#warning "this edge cannot be split"
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
                  Stack.push_right s (Format.kasprintf subst "{31}?<color=%s>|{32}?<color=%s>" c c)
                else s in
              let s =
                if sel '2' then
                  Stack.push_right s (Format.kasprintf subst "{21}?<color=%s>|{23}?<color=%s>" c c)
                else s in
              let s =
                if sel '1' then
                  Stack.push_right s (Format.kasprintf subst "{12}?<color=%s>|{13}?<color=%s>" c c)
                else s in
              let s =
                if sel 's' then
                  Stack.push_right s (Format.kasprintf subst "*(-<color=%s>,-<color=%s>,-<color=%s>)" c c c)
                else s in
              let s = Stack.replace s (Format.kasprintf subst "#3 -<color=%s;radius=%s>" c r) in
              self#set_stack s 
           | 2 -> 
              let c = match x#get "color" with Some c -> c | None -> "gray" in
              let s = History.present hist in
              let s = Stack.replace s (Format.kasprintf subst "#2 -<color=%s>" c) in
              let s = Stack.push_here s (subst "#2 0") in
              self#set_stack s 
           | _ -> self#warning "may only split edges of arity two and three")
      | `V _ -> self#warning "cannot split a vertex"
      | `N -> ()  

    method private left = self#set_stack (Stack.move_left (History.present hist))
    method private right = self#set_stack (Stack.move_right (History.present hist))

    method private discard ~force =
      if force || match kind graph with `Skip _ -> true | _ -> false then
        let s = Stack.pop (History.present hist) in
        let s = if Stack.size s = 0 then
                  (self#warning "discarded the last graph!"; Stack.of_list ["0"])
                else s
        in self#set_stack s
      else self#warning "no obvious reason to discard this case"

    method private duplicate =
      let s = History.present hist in
      History.save hist (Stack.push_right s (Stack.current s));
      self#display_graph_infos

    method on_button_press =
      match mode, self#catch with
      | `Normal, `V x -> active <- `V x
      | `Normal, `E x -> active <- `E x
      | `InsertEdge l, `V v -> mode <- `InsertEdge (Seq.snoc l v)
      | `InsertEdge l, `N ->
         mode <- `InsertEdge (Seq.snoc l (Inn self#ivertex))
      | _ -> ()
    
    method on_button_release =
      (match active with `V _ | `E _ -> self#checkpoint | `N -> ());
      active <- `N

    method on_motion =
      match active with
      | `V v ->
         (Graph.vinfo graph v)#move arena#pointer;
         Graph.iter_edges'' (fun e _ n-> if Seq.mem v n then Place.center_edge graph e) graph;
         self#redraw()
      | `E e ->
         (Graph.einfo e)#move arena#pointer;
         self#redraw()
      | `N -> ()

    method on_key_press s =
      match mode with
      | `Normal ->
         (match s with
          | "h" -> self#help 
                     "** keys **
c:     center edge / optimise placement
-/+:   shrink/enlarge element / whole graph
b/u:   block/unblock element for placement optimisations
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
          | "c" -> self#center
          | "b" -> self#block true
          | "u" -> self#block false
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "i" -> ignore self#ivertex
          | "l" -> self#lift
          | "f" -> self#forget
          | "p" -> self#promote
          | "d" | "r" -> self#remove
          | "e" -> mode <- `InsertEdge Seq.empty
          | "s" -> self#split ~opt:false
          | "o" -> self#split ~opt:true
          | "k" -> self#discard ~force:false
          | "K" -> self#discard ~force:true
          | "D" -> self#duplicate
          | "left" -> self#left
          | "right" -> self#right
          | "" -> ()
          | s -> self#warning "skipping key %s@." s)
      | `InsertEdge l ->
         match s with
         | "-" -> mode <- `Normal; self#edge l ""
         | s -> mode <- `Normal; self#edge l s

    method private on_term f =
      match term_of_string self#entry with
      | t -> self#set_entry "%a" (Term.pp Sparse) (f t)
      | exception _ -> self#warning "current term is not valid"

    method normalise_term() = self#on_term NTerm.get
    method desugar_term() = self#on_term PTerm.get

    method load_from file =
      let l = File.read file in
      List.iter place_term l;
      let s = Stack.of_list (List.map string_of_term l) in
      self#set_stack ~rebox:true s;
      self#checkpoint;
      History.clear hist  

    method save_to file =
      let s = Stack.to_list (History.present hist) in
      let s = List.map term_of_string s in
      File.write file s;
      File.export file s

  end
