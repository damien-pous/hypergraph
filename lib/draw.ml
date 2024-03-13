open Types
open Graph
open Gg
open Vg

let area = `O { P.o with P.width = Constants.linewidth }

class pic: picture =
  object(self)
    val mutable image = I.void
    method clear = image <- I.void
    method get = image
    method blend i = image <- (image |> I.blend i)
    method path ?(color=Color.black) p = self#blend (I.const color |> I.cut ~area p)
    method surface ?(color=Color.gray 0.5) p = self#blend (I.const color |> I.cut p); self# path p
    method circle ?color c = self#path ?color (P.empty |> P.circle c.center c.radius)
    method disc ?color c = self#surface ?color (P.empty |> P.circle c.center c.radius)
    method point ?color p = self#surface ?color (P.empty |> P.circle p 0.006)
    method segment ?color x y = self#path ?color (P.empty |> P.sub x |> P.line y)
    method line ?color l =
      let d = V2.smul 10. l.dir in
      self#point ?color l.point;
      self#segment ?color (V2.sub l.point d) (V2.add l.point d)
    method text p text =
      if not Constants.render_labels_with_cairo then
        let p = V2.sub p
                  (V2.v (float_of_int (String.length text) *. Constants.fontsize /. 3.)
                     (Constants.fontsize /. 3.)) in
        self#blend (I.move p (I.const Color.black |> I.cut_glyphs ~text Constants.font []))
  end

let graph (g: #drawable graph) =
  let npos = Seq.lmap (fun v -> (vinfo g v)#pos) in
  let draw = new pic in
  let draw_source i x =
    let p = x#pos in
    let d = 2. *. x#radius in
    let color = x#color in
    draw#surface ~color (P.empty |> P.rect (Box2.v_mid p (Size2.v d d)));
    draw#text p (string_of_int i)
  in
  let draw_ivertex x =
    let c = x#circle in
    let color = x#color in
    draw#disc ~color c;
    draw#text x#pos x#label
  in
  let draw_edge x n =
    let c = x#circle in
    let color = x#color in
    draw#surface ~color (Geometry.edge c (npos n));
    (* draw#circle c; *)
    draw#text x#pos x#label
  in
  (* Geometry.set_debug draw; *)
  iter_edges draw_edge g;
  iter_sources draw_source g;
  iter_ivertices draw_ivertex g;
  draw#get
