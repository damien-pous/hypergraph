open Common
open Gg

module E = Id.Make(struct let prefix = "e" end)
type edge = E.t
type 'a emap = 'a E.map
let nomap f = E.map (fun (i,n) -> (i,Seq.omap f n))
let nmap f = E.map (fun (i,n) -> (i,Seq.map f n))
let nfilter f = nomap (fun v -> if f v then Some v else None)

module I = Id.Make(struct let prefix = "x" end)
type ivertex = I.t
type 'a imap = 'a I.map

type vertex = Src of int | Inn of ivertex
let src i = Src i
let inn i = Inn i

module M = struct
type 'a t = {
    arity: int;
    ivertices: 'a imap;
    edges: ('a * vertex seq) emap;
  }
let arity g = g.arity
let isize g = I.size g.ivertices
let esize g = E.size g.edges

let nil arity =
  { arity; ivertices = I.empty; edges = E.empty }

let par g h =
  assert (arity g = arity h);
  { g with
    ivertices = I.union g.ivertices h.ivertices;
    edges = E.union g.edges h.edges }

let lft g = { g with arity = g.arity + 1 }

let fgt i g =
  let k = arity g in
  assert (k > 0);
     let z = I.fresh() in
     let z' = Inn z in
     { arity = k-1;
       ivertices = I.add z i g.ivertices;
       edges = nmap 
                 (function
                  | Src i when i=k -> z'
                  | x -> x)
                 g.edges }

let src_map f = function
  | Src i -> Src (f i)
  | x -> x

let prm p g =
  { g with
    edges = nmap (src_map (Perm.apply p)) g.edges }

let edg arity einfo =
  let e = E.fresh() in
  { arity;
    ivertices = I.empty;
    edges = E.single e (einfo, Seq.init arity src) }

let iter_edges f g = E.iter (fun e (i,n) -> f e i n) g.edges
let iter_ivertices f g = I.iter f g.ivertices

let edges g = E.values g.edges
let neighbours g e = snd (E.get g.edges e)
let einfo g e = fst (E.get g.edges e)
let iinfo g x = I.get g.ivertices x

let map ~fi ~fe g =
  { arity = g.arity;
    ivertices = I.map fi g.ivertices;
    edges = E.map (fun (x,n) -> fe x, n) g.edges }

let map' fi fe g =
  let ivertices = I.map fi g.ivertices in
  let edges = E.map (fun (x,n) -> fe (I.get ivertices) x n, n) g.edges in
  { arity = g.arity; ivertices; edges }

let ppv f = function
  | Src i -> Format.fprintf f "%i" i
  | Inn i -> I.pp f i
let pp_gen ~ppi ~ppe f g =
  iter_ivertices (fun v x ->
      Format.fprintf f "%a %a\n" I.pp v ppi x) g;
  iter_edges (fun e x n ->
      Format.fprintf f "%a %a\n" E.pp e ppe x;
        Seq.iter (fun i v ->
          Format.fprintf f "%a -- %a [label=%i]\n" E.pp e ppv v i
        ) n
    ) g

let pp_ ?full = pp_gen ~ppi:(Info.pp ?full) ~ppe:(Info.pp ?full)
let pp_dot =
  let ppi f _ = Format.fprintf f "[shape=point]\n" in
  let ppe f _ = Format.fprintf f "[shape=circle]\n" in
  fun f g ->    
  Format.fprintf f "graph {\n";
  for i = 1 to arity g do
    Format.fprintf f "%i [shape=box]\n" i;
  done;
  Format.fprintf f "%a}\n" (pp_gen ~ppi ~ppe) g

end
include M
include Extend(M)

module INIT(M: EALGEBRA) = struct
  let eval g =
    let k = ref (arity g) in
    let t = Hashtbl.create 10 in
    let names = I.map (fun x -> incr k; Hashtbl.add t !k x; !k) g.ivertices in
    let k' = !k in
    let idx = function
      | Src i -> i
      | Inn i -> I.get names i
    in
    let edges = ref [] in
    iter_edges (fun _ e n ->
        edges := M.inj k' (Inj.of_list (List.map idx (Seq.to_list n)))
                   (M.edg (Seq.size n) e) :: !edges)
      g;
    let rec fgt i =
      if i=k' then M.bigpar k' !edges
      else M.fgt (Hashtbl.find t (i+1)) (fgt (i+1))
    in fgt (arity g)    
end

(* checking isomorphism
   naively for now: just try to match edges in all possible ways
 *)
let iso cmp g h =
  let rec extend1 acc r x y =
    match Set.case r with
    | None -> Some (Set.add (x,y) acc)
    | Some (x',y' as p, q) ->
       match x==x', y==y' with
       | true,true -> Some (Set.union acc r)
       | false,false -> extend1 (Set.add p acc) q x y
       | _,_ -> None
  in
  let extend1 r x y =
    match x,y with
    | Src i, Src j when i=j -> Some r
    | Inn x, Inn y -> extend1 Set.empty r x y
    | _ -> None
  in    
  let extend r (x,nx) (y,ny) =
    if cmp x y && Seq.size nx = Seq.size ny then
      Seq.ofold2 extend1 r nx ny
    else None
  in
  let rec iso h k r =
    (* Format.printf "iso: %a %a %a@." *)
    (*   (Set.pp (fun f (x,_) -> Info.ppe f x)) h *)
    (*   (Set.pp (fun f (y,_) -> Info.ppe f y)) k *)
    (*   (Set.pp (fun f (x,y) -> Format.fprintf f "%a--%a" I.pp x I.pp y)) r; *)
    match Set.case h with
    | None -> assert (Set.is_empty k); true
    | Some(x,h) -> Set.exists (fun y k ->
                       match extend r x y with
                       | Some r -> iso h k r
                       | None -> false
                     ) k
  in
  arity g = arity h &&
    isize g = isize h &&
      esize g = esize h &&
        iso (edges g) (edges h) Set.empty

type 'a graph = 'a t
module Sourced = struct
  type 'a t = 'a seq * 'a graph
  let map ~fs ~fi ~fe (s,g) =
    Seq.map fs s, map ~fi ~fe g
  let pp_gen ~pps ~ppi ~ppe f (s,g) =
    Seq.iter (fun i x -> Format.fprintf f "%i %a\n" i pps x) s;
    pp_gen ~ppi ~ppe f g
  let vpos (s,g)= function
    | Src i -> Info.pos (Seq.get s i)
    | Inn x -> Info.pos (iinfo g x)
  let center_edge (_,g as sg) e =
    let n = neighbours g e in
    match Seq.size n with
    | 0 -> ()
    | 1 -> let p = vpos sg (Seq.get n 1) in
           let x = einfo g e in
           let px = Info.pos x in
           let v = V2.unit (V2.sub px p) in
           Info.set_pos x (V2.add p (V2.smul (Info.radius x *. 2.) v))
    | _ -> 
       let c = Geometry.center (Seq.lmap (vpos sg) n) in
       let x = einfo g e in
       Info.set_pos x c
end
type 'a sgraph = 'a Sourced.t

class ['a] dyn = 
  object(self)
    val mutable sources = Seq.empty
    val mutable graph = nil 0
    method set (s,g) =
      assert (Seq.size sources = arity graph);
      sources <- s; graph <- g

    method sources = sources
    method graph = graph
    method sgraph = sources,graph
    method arity = arity graph

    method neighbours e = snd (E.get graph.edges e)
    method einfo e = fst (E.get graph.edges e)

    method sinfo = Seq.get sources
    method iinfo = I.get graph.ivertices
    method vinfo = function
      | Src i -> self#sinfo i
      | Inn x -> self#iinfo x

    method iter_edges f =
      iter_edges f graph
    method iter_sources f =
      Seq.iter f sources;
    method iter_ivertices f =
      iter_ivertices f graph
    method iter_vertices f =
      self#iter_sources (fun i -> f (src i));
      self#iter_ivertices (fun i -> f (inn i));         
    method iter_infos f =
      self#iter_vertices (fun _ -> f);
      self#iter_edges (fun _ x _ -> f x)

    method lift v =
      sources <- Seq.snoc sources v;
      graph <- lft graph

    method permute p =
      sources <- Perm.sapply p sources;
      graph <- prm p graph
        
    method add_edge (i: 'a) n =
      let e = E.fresh() in
      graph <- { graph with edges = E.add e (i,n) graph.edges };
      e
    method add_ivertex i =
      let x = I.fresh() in
      graph <- { graph with ivertices = I.add x i graph.ivertices };
      x
    method rem_edge e =
      graph <- { graph with edges = E.rem e graph.edges }
    method rem_ivertex x =
      graph <- { graph with
                 ivertices = I.rem x graph.ivertices;
                 edges = nfilter
                           (function
                            | Inn y when x == y -> false
                            | _ -> true)
                           graph.edges }
    method private rem_last_source =
      match Seq.case sources with
      | None -> failwith "no source to remove"
      | Some (q,_) ->
         sources <- q;
         graph <- { graph with
                    arity = graph.arity - 1;
                    edges = nomap
                              (function
                               | Src i when i = graph.arity -> None
                               | v -> Some v)
                              graph.edges }
    method rem_source i =
      let k = graph.arity in
      if i>k then failwith "rem_source: not a valid source"
      else if i=k then self#rem_last_source
      else (self#permute (Perm.of_cycle [i;k]); self#rem_last_source)
    method rem_vertex = function
        | Src i -> self#rem_source i
        | Inn x -> self#rem_ivertex x

    method promote x =
      let arity = graph.arity+1 in
      sources <- Seq.snoc sources (self#iinfo x);
      graph <- { arity;
                 ivertices = I.rem x graph.ivertices;
                 edges = nmap
                           (function
                            | Inn y when x == y -> src arity
                            | v -> v)
                           graph.edges }
    
    method private forget_last =
      match Seq.case sources with
      | None -> failwith "fgt: no source to forget"
      | Some (q,v) ->
         sources <- q;
         graph <- fgt v graph
    method forget i =
      let k = graph.arity in
      if i>k then failwith "forget: not a valid source"
      else if i=k then self#forget_last
      else (self#permute (Perm.of_cycle [i;k]); self#forget_last)

    method find f: [`V of vertex * 'a | `E of edge * 'a | `N] =
      let r = ref `N in
      try
        self#iter_vertices (fun v i -> if f i then (r := `V(v,i); raise Not_found));
        self#iter_edges (fun e i _ -> if f i then (r := `E(e,i); raise Not_found));
        `N
      with Not_found -> !r

  end

(* let export_gen cmd print file g = *)
(*   let o = Unix.open_process_out (cmd^" > "^file) in *)
(*   print o g; *)
(*   flush o; *)
(*   let e = Unix.close_process_out o in *)
(*   if e <> Unix.WEXITED 0 then *)
(*     failwith ("export failed: "^cmd) *)

(* let export_dot = export_gen "neato -Tpng" print_dot *)
