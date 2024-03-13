open Misc
open Graph
open Gg
open Vg

let graph (sg: #drawable sgraph) =
  let _,g = sg in
  let npos = Seq.lmap (fun v -> (vinfo sg v)#pos) in
  let drawing = ref I.void in
  let blend d = drawing := !drawing |> I.blend d in
  let area = `O { P.o with P.width = Constants.linewidth } in
  let curve ?(c=Color.black) p = blend (I.const c |> I.cut ~area p) in
  let surface ?(c=Color.gray 0.5) p = blend (I.const c |> I.cut p); curve p in
  let circle ?(color=Color.black) c r =
    blend (I.const color |> I.cut ~area (P.empty |> P.circle c r))
  in
  let text p text =
    if not Constants.render_labels_with_cairo then
      let p = V2.sub p
                (V2.v (float_of_int (String.length text) *. Constants.fontsize /. 3.)
                   (Constants.fontsize /. 3.)) in
      blend (I.move p (I.const Color.black |> I.cut_glyphs ~text Constants.font []))
  in
  let draw_source i x =
    let p = x#pos in
    let d = 2. *. x#radius in
    let c = x#color in
    surface ~c (P.empty |> P.rect (Box2.v_mid p (Size2.v d d)));
    text p (string_of_int i)
  in
  let draw_ivertex x =
    let p = x#pos in
    let r = x#radius in
    let c = x#color in
    surface ~c (P.empty |> P.circle p r);
    text p x#text
  in
  let draw_edge x n =
    let p = x#pos in
    let r = x#radius in
    let c = x#color in
    surface ~c (Geometry.edge (Geometry.circle p r) (npos n));
    circle p r;
    text p x#text
  in
  iter_edges draw_edge g;
  iter_sources draw_source sg;
  iter_ivertices draw_ivertex g;
  !drawing
