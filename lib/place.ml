open Gg
open Graph

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
  iter_sources (fun i s -> (* if not s#placed then *) s#move (spos i)) g

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

let graphviz g =
  (* neato/fdp/sfdp *)
  let i,o = Unix.open_process "neato -s72" in
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

