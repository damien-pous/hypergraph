open Gg
open Graph

let circle_random g =
  let k = arity g in
  let n = size g in
  let spos i =
    if k=1 then V2.zero
    else
      (V2.polar 0.7 (
           2. *. Float.pi *. float_of_int (i-1) /. float_of_int k
           -. Float.pi_div_2 -. Float.pi /. float_of_int k))
  in
  let sources = Seq.init k (fun i -> Info.for_source (spos i) i) in
  let s = sqrt (float_of_int n) /. 2. in
  Graph.iter_ivertices
    (fun _ x -> Info.set_pos x
                  (Option.value ~default:(Geometry.random2 s) (Info.user_pos x)))
    g;
  Graph.iter_edges
    (fun _ x n -> 
      match Info.user_pos x with
      | None ->
         let c = Geometry.center (Seq.lmap (Graph.Sourced.vpos (sources,g)) n) in
         let shift = Info.user_shift x in
         let radius = Info.radius x in
         let pos =
           match Seq.size n with
           | 0 -> Geometry.random2 s
           | 1 when shift=V2.zero ->
              let a = Float.random ~min:0. ~len:(2. *. Float.pi) () in
              V2.add c (V2.ltr (M2.rot2 a) (V2.smul (radius *. 2.) V2.ox))
           | _ -> V2.add c shift     
         in
         Info.set_pos x pos
      | Some _ -> ())
    g;
  sources, g
