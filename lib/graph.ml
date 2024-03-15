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

let arity g = g.arity
let isize g = Set.size g.ivertices
let esize g = Set.size g.edges
let width _ = failwith "todo"

let edges g = g.edges
let iter_ivertices f g = Set.iter f g.ivertices
let iter_edges'' f g = Set.iter (fun e -> f e e.einfo e.neighbours) g.edges
let iter_edges' f g = Set.iter (fun e -> f e) g.edges
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
    edges = Set.map (fun x -> { einfo = f.fe (Seq.size x.neighbours) x.einfo;
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

let sources (s,_) = s
let ivertices (_,g) = g.U0.ivertices
let edges (_,g) = g.U0.edges
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

let bbox (g: #positionned graph) =  
  let b = ref Gg.Box2.empty in
  iter_infos (fun i ->
      let d = i#radius *. 2. in 
      b := Gg.Box2.union !b (Gg.Box2.v_mid i#pos (Gg.Size2.v d d));
    ) g;
  !b

let draw_on (draw: canvas) (g: #positionned graph) =
  let npos = Seq.lmap (fun v -> (vinfo g v)#pos) in
  let draw_source i x =
    let p = x#pos in
    let d = 2. *. x#radius in
    let fill = x#color in
    draw#box ~fill (Gg.Box2.v_mid p (Gg.Size2.v d d));
    draw#text p (string_of_int i)
  in
  let draw_ivertex x =
    let c = x#circle in
    let fill = x#color in
    draw#circle ~fill c;
    draw#text x#pos x#label
  in
  let draw_edge x n =
    let c = x#circle in
    let fill = x#color in
    draw#path ~fill (Geometry.edge c (npos n));
    (* draw#circle c; *)
    draw#text x#pos x#label
  in
  (* Geometry.set_debug draw; *)
  iter_edges draw_edge g;
  iter_sources draw_source g;
  iter_ivertices draw_ivertex g;
  (* draw#box (bbox g); *)
  (* draw#get *)
  ()

let draw g =
  let c = new Picture.basic_canvas in
  draw_on c g;
  c#get

let find f g =
  let r = ref `N in
  try
    iter_vertices (fun v -> if f (vinfo g v) then (r := `V v; raise Not_found)) g;
    iter_edges'' (fun e x _ -> if f x then (r := `E e; raise Not_found)) g;
    `N
  with Not_found -> !r

let get_info (s,g) = function
  | S,i -> Seq.get s i
  | I,i -> Set.nth g.U0.ivertices i
  | E,i -> (Set.nth g.U0.edges i).einfo
