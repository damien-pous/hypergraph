open Graph
open Gg

let npos g = Seq.lmap (fun v -> (vinfo g v)#pos)

let sources_on_circle g =
  let k = arity g in
  let r = sqrt (float_of_int (size g)) *. Constants.sradius *. 10. in
  let spos i =
    if k=1 then V2.zero
    else
      V2.smul r
      (V2.polar 0.7 (
           2. *. Float.pi *. float_of_int (i-1) /. float_of_int k
           -. Float.pi_div_2 -. Float.pi /. float_of_int k))
  in
  iter_sources (fun i s -> if not s#placed then s#move (spos i)) g

let randomly g =
  let n = size g in
  let s = sqrt (float_of_int n) *. Constants.sradius *. 10. in
  iter_sources (fun _ x -> if not x#placed then x#move (Geometry.random2 s)) g;
  iter_ivertices (fun x -> if not x#placed then x#move (Geometry.random2 s)) g;
  iter_edges (fun e n -> 
      if not e#placed then
        let c = Geometry.center (npos g n) in
        let shift = e#pos in    (* position used as shift if not placed *)
        let radius = e#radius in
        let pos =
           match Seq.size n with
           | 0 -> Geometry.random2 s
           | 1 when shift=V2.zero ->
              let a = Float.random ~min:0. ~len:(2. *. Float.pi) () in
              V2.add c (V2.ltr (M2.rot2 a) (V2.smul (radius *. 2.) V2.ox))
           | _ -> V2.add c shift     
        in
        e#move pos
    ) g

let pp_dot f g =
  let ppp f x =
    let d = 2. *. x#radius /. Constants.inch in
    Format.fprintf f "width=%g,height=%g,fixedsize=true,label=\"\"" d d;
    if x#placed then
      Format.fprintf f ",pos=\"%g,%g\",pin=true" (P2.x x#pos) (P2.y x#pos)
  in
  let ppv f = function
  | Src i -> Format.fprintf f "s%i" i
  | Inn x -> Format.fprintf f "i%i" (MSet.index x (ivertices g))
  in
  Format.fprintf f "graph {\n";
  Seq.iter (fun i x -> Format.fprintf f "s%i[%a]\n" i ppp x) (sources g);
  MSet.iteri (fun id x -> Format.fprintf f "i%i[%a]\n" id ppp x) (ivertices g);
  MSet.iteri (fun id x ->
      Format.fprintf f "e%i[%a]\n" id ppp (einfo x);
      Format.fprintf f "e%i--%a\n" id (Seq.pp ppv) (neighbours x)
    ) (edges g);
  Format.fprintf f "}\n"

let graphviz ?(cmd="neato -s72") g =
  let i,o = Unix.open_process cmd in
  let f = Format.formatter_of_out_channel o in
  let l = Lexing.from_channel i in
  Format.fprintf f "%a%!" pp_dot g; close_out o;
  let _,l = Parser.dotlines Lexer.dotline l in
  if Unix.close_process (i,o) <> Unix.WEXITED 0 then failwith "warning: neato returned an error";
  List.iter (fun (id,p) -> (Graph.get_info g id)#move p) l

let center_edge sg e =
  let n = neighbours e in
  match Seq.size n with
  | 0 -> ()
  | 1 ->
     let p = (vinfo sg (Seq.get n 1))#pos in
     let x = einfo e in
     let px = x#pos in
     let v = V2.unit (V2.sub px p) in
     x#move (V2.add p (V2.smul (x#radius *. 2.) v))
  | _ -> 
     let c = Geometry.center (npos sg n) in
     (einfo e)#move c

let graph_center g =
  Geometry.center (Seq.lmap (fun s -> s#pos) (Graph.sources g))

let scale s g =
  let c = graph_center g in
  let m = M3.mul (M3.move2 c)
         (M3.mul (M3.scale2 (V2.v s s))
                 (M3.move2 (V2.neg c))) in
  Graph.iter_infos (fun e -> e#move (P2.tr m e#pos)) g

let improve_placement s g =
  let t = Hashtbl.create (Graph.size g) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
  in
  (* repulse *)
  Graph.iter_infos (fun x ->
      Graph.iter_infos (fun y ->
          if x!=y then
            let xy = V2.sub y#pos x#pos in
            let d = V2.norm xy in
            if d = 0.0 then
              add x x#radius (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let dmin =
                if x#kind = `E || y#kind = `E then
                  2. *. (x#radius +. y#radius)
                else
                  4. *. Constants.eradius 3
              in
              if d < dmin then add x (3.*.(1. -. dmin/.d)) xy
        ) g
    ) g;
  (* attract *)
  Graph.iter_edges (fun e ->
      Seq.iter (fun _ x ->
          let x = Graph.vinfo g x in
          let ex = V2.sub x#pos e#pos in
          let d = V2.norm ex in
          let dmin = 2. *. (e#radius +. x#radius) in
          if d > dmin then (
            let s = dmin/.d -. 1. in
            add x s ex;
            add e (-.s) ex
          )          
        )
    ) g;
  Hashtbl.iter (fun x u ->
      if x#get "fixed" <> Some "true" then
        x#move (V2.add x#pos (V2.smul s u))) t

let elastic g =
  randomly g;
  for _ = 1 to 100 do
    improve_placement 0.1 g
  done

let fix x = x#set "fixed" "true"
let unfix x = x#unset "fixed"

let fix_sources g = Graph.iter_sources (fun _ -> fix) g

let automatic g =
  sources_on_circle g;
  fix_sources g;
  try
    if not Sys.unix then raise Not_found;
    graphviz g
  with
    _ -> randomly g; elastic g
