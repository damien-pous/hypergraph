open Gg
open Graph

let npos sg = Seq.lmap (fun v -> (vinfo sg v)#pos)

let circle_random sg =
  let _,g = sg in
  let k = arity g in
  let n = size g in
  let s = sqrt (float_of_int n) /. 2. in
  let spos i =
    if k=1 then V2.zero
    else
      (V2.polar 0.7 (
           2. *. Float.pi *. float_of_int (i-1) /. float_of_int k
           -. Float.pi_div_2 -. Float.pi /. float_of_int k))
  in
  iter_sources (fun i s -> if not s#placed then s#move (spos i)) sg;
  iter_ivertices (fun x -> if not x#placed then x#move (Geometry.random2 s)) g;
  iter_edges (fun e n -> 
      if not e#placed then
        let c = Geometry.center (npos sg n) in
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

