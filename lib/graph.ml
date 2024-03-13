open Misc
open Common

type 'a vertex = Src of int | Inn of 'a
let src i = Src i
let inn i = Inn i
let vmap f = function
  | Inn x -> Inn (f x)
  | Src i -> Src i

type 'a edge = { einfo: 'a; neighbours: 'a vertex seq }
let einfo e = e.einfo
let neighbours e = e.neighbours

let nomap f = Set.map (fun e -> { e with neighbours = Seq.omap f e.neighbours })
let nmap f = Set.map (fun e -> { e with neighbours = Seq.map f e.neighbours })
let nfilter f = nomap (fun v -> if f v then Some v else None)

module M = struct
type 'a t = {
    arity: int;
    ivertices: 'a set;
    edges: 'a edge set;
  }
let arity g = g.arity
let edges g = g.edges

let isize g = Set.size g.ivertices
let esize g = Set.size g.edges

let iter_ivertices f g = Set.iter f g.ivertices
let iter_edges f g = Set.iter (fun e -> f e.einfo e.neighbours) g.edges

let nil arity =
  { arity; ivertices = Set.empty; edges = Set.empty }

let par g h =
  assert (arity g = arity h);
  { g with
    ivertices = Set.union g.ivertices h.ivertices;
    edges = Set.union g.edges h.edges }

let lft g = { g with arity = g.arity + 1 }

let fgt z g =
  let k = arity g in
  assert (k > 0);
  let z' = Inn z in
  { arity = k-1;
    ivertices = Set.add z g.ivertices;
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
  { arity;
    ivertices = Set.empty;
    edges = Set.single { einfo; neighbours=Seq.init arity src } }

let map f g =
  (* TODO: need to memoize [f] to guarantee physical identities *)
  let _ = failwith "Warning: fix Graph.map before using it" in
  { arity = g.arity;
    ivertices = Set.map f.fi g.ivertices;
    edges = Set.map (fun x -> { einfo = f.fe x.einfo;
                                neighbours = Seq.map (vmap f.fi) x.neighbours }) g.edges }

let pp mode f g =
  let ppv f = function
  | Src i -> Format.fprintf f "%i" i
  | Inn x -> Format.fprintf f "i%i" (Set.index x g.ivertices)
  in
  Set.iteri (fun id x -> Format.fprintf f "i%i %t\n" id (x#pp mode)) g.ivertices;
  Set.iteri (fun id x ->
      Format.fprintf f "e%i %t\n" id (x.einfo#pp mode);
        Seq.iter (fun i v ->
          Format.fprintf f "e%i -- %a [label=%i]\n" id ppv v i
        ) x.neighbours
    ) g.edges

end
include M
include Extend(M)
type 'a graph = 'a t
type 'a sgraph = 'a st

let sinfo (s,_) = Seq.get s 
let vinfo (s,_) = function
  | Src i -> Seq.get s i
  | Inn x ->  x
let iter_sources f (s,_) = Seq.iter f s
let iter_vertices f (_,g as sg) =
  iter_sources (fun i _ -> f (src i)) sg;
  iter_ivertices (fun i -> f (inn i)) g


module INIT(M: EALGEBRA) = struct
  let eval g =
    let k = arity g in
    let v = isize g in
    let k' = k + v in
    let idx = function
      | Src i -> i
      | Inn x -> k + Set.index x g.ivertices (* or reversed, if Set.fold changes *)
    in
    let u =
      Set.fold (fun e ->
          let e,n = e.einfo, e.neighbours in
          M.par
            (M.inj k' (Inj.of_list (List.map idx (Seq.to_list n)))
               (M.edg (Seq.size n) e)))
        (M.nil k') g.edges
    in
    Set.fold M.fgt u g.ivertices
  let seval (s,g) = s,eval g
end

(* checking isomorphism
   naively for now: just try to match edges in all possible ways *)
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
  let extend r x y =
    let x,nx = x.einfo,x.neighbours in
    let y,ny = y.einfo,y.neighbours in
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

let siso cmp (_,g) (_,h) = iso cmp g h

class ['a] dynamic = 
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

    method sinfo = Seq.get sources
    method vinfo = vinfo (sources,graph)

    method iter_edges f =
      Set.iter f self#graph.edges
    method iter_sources f =
      Seq.iter f sources;
    method iter_ivertices f =
      iter_ivertices f graph
    method iter_vertices f =
      self#iter_sources (fun i _ -> f (src i));
      self#iter_ivertices (fun i -> f (inn i));         
    method iter_infos f =
      self#iter_sources (fun _ -> f);
      self#iter_ivertices f;         
      self#iter_edges (fun x -> f x.einfo)

    method lift v =
      sources <- Seq.snoc sources v;
      graph <- lft graph

    method permute p =
      sources <- Perm.sapply p sources;
      graph <- prm p graph
        
    method add_edge einfo neighbours =
      let e = {einfo;neighbours} in
      graph <- { graph with edges = Set.add e graph.edges };
      e
    method add_ivertex i =
      graph <- { graph with ivertices = Set.add i graph.ivertices }
    method rem_edge e =
      graph <- { graph with edges = Set.remq e graph.edges }
    method rem_ivertex x =
      graph <- { graph with
                 ivertices = Set.remq x graph.ivertices;
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
      sources <- Seq.snoc sources x;
      graph <- { arity;
                 ivertices = Set.remq x graph.ivertices;
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

    method find f: [`V of 'a vertex | `E of 'a edge | `N] =
      let r = ref `N in
      try
        self#iter_vertices (fun v -> if f (self#vinfo v) then (r := `V v; raise Not_found));
        self#iter_edges (fun e -> if f e.einfo then (r := `E e; raise Not_found));
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
