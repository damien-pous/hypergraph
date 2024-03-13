open Types
open Misc

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

module U0 = struct
type 'a t = {
    arity: int;
    ivertices: 'a set;
    edges: 'a edge set;
  }
type ('a,'b) m = ('a,'b) umapper

let arity g = g.arity
let isize g = Set.size g.ivertices
let esize g = Set.size g.edges
let width _ = failwith "todo"

let edges g = g.edges
let iter_ivertices f g = Set.iter f g.ivertices
let iter_edges'' f g = Set.iter (fun e -> f e e.einfo e.neighbours) g.edges
let iter_edges' f g = Set.iter (fun e -> f e) g.edges
let iter_edges f g = Set.iter (fun e -> f e.einfo e.neighbours) g.edges

let pp_dot mode f g =
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

let add_edge einfo neighbours g =
  let e = {einfo;neighbours} in
  e,{ g with edges = Set.add e g.edges }

let rem_edge e g =
  { g with edges = Set.remq e g.edges }

let add_ivertex v g =
  { g with ivertices = Set.add v g.ivertices }

let rem_ivertex v g =
  { g with
    ivertices = Set.remq v g.ivertices;
    edges = nfilter
              (function
               | Inn w when v == w -> false
               | _ -> true)
              g.edges }

let rem_last_source g =
  let k = g.arity in
  if k = 0 then failwith "no source to remove";
  { g with
    arity = k - 1;
    edges = nomap
              (function
               | Src i when i = k -> None
               | v -> Some v)
              g.edges }
let rem_source i g =
  let k = g.arity in
  if i>k then failwith "rem_source: not a valid source"
  else if i=k then rem_last_source g
  else rem_last_source (prm (Perm.of_cycle [i;k]) g)

let rem_vertex = function
  | Src i -> rem_source i
  | Inn x -> rem_ivertex x

let promote x g =
  let arity = g.arity+1 in
  { arity; ivertices = Set.remq x g.ivertices;
    edges = nmap
              (function
               | Inn y when x == y -> src arity
               | v -> v)
              g.edges }

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

end
module U1 = struct
  include U0
  include Functor.E(U0)
  module I(M: EALGEBRA) = struct
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
  end
end
include Functor.S(U1)
module U = struct include U0 include U1 include U end
type 'a graph = 'a t
type 'a ugraph = 'a u

let sinfo (s,_) = Seq.get s 
let vinfo (s,_) = function
  | Src i -> Seq.get s i
  | Inn x ->  x
let iter_edges f (_,g) = U.iter_edges f g
let iter_edges' f (_,g) = U.iter_edges' f g
let iter_edges'' f (_,g) = U.iter_edges'' f g
let iter_ivertices f (_,g) = U.iter_ivertices f g
let iter_sources f (s,_) = Seq.iter f s
let iter_vertices f g =
  iter_sources (fun i _ -> f (src i)) g;
  iter_ivertices (fun i -> f (inn i)) g
let iter_infos f g =
  iter_sources (fun _ -> f) g;
  iter_ivertices f g;         
  iter_edges (fun e _ -> f e) g

let add_edge e n (s,g) = let e,g = U.add_edge e n g in e,(s,g)
let rem_edge e (s,g) = (s,U.rem_edge e g)
let add_ivertex v (s,g) = (s,U.add_ivertex v g)
let rem_ivertex v (s,g) = (s,U.rem_ivertex v g)
let rem_last_source (s,g) =
  match Seq.case s with
  | None -> failwith "no source to remove"
  | Some (s,_) -> (s,U.rem_last_source g)
let rem_source i g =
  let k = arity g in
  if i>k then failwith "rem_source: not a valid source"
  else if i=k then rem_last_source g
  else rem_last_source (prm (Perm.of_cycle [i;k]) g)
let rem_vertex = function
  | Src i -> rem_source i
  | Inn x -> rem_ivertex x
let promote x (s,g) = (Seq.snoc s x, U.promote x g)

let iso cmp (_,g) (_,h) = U.iso cmp g h

let find f g =
  let r = ref `N in
  try
    iter_vertices (fun v -> if f (vinfo g v) then (r := `V v; raise Not_found)) g;
    iter_edges'' (fun e x _ -> if f x then (r := `E e; raise Not_found)) g;
    `N
  with Not_found -> !r

let pp_dot mode f (s,g) =
  Format.fprintf f "graph {\n";
  Seq.iter (fun i x -> Format.fprintf f "%i %t\n" i (x#pp mode)) s;
  U.pp_dot mode f g;
  Format.fprintf f "}\n"

(* let export_gen cmd print file g = *)
(*   let o = Unix.open_process_out (cmd^" > "^file) in *)
(*   print o g; *)
(*   flush o; *)
(*   let e = Unix.close_process_out o in *)
(*   if e <> Unix.WEXITED 0 then *)
(*     failwith ("export failed: "^cmd) *)

(* let export_dot = export_gen "neato -Tpng" print_dot *)
