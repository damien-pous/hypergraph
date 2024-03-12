open Common
open Gg
open Vg

class graph = 
  object(self)
    inherit [info] Graph.dyn as parent
    
    (* tothink: surcharge [permute] in this way? *)
    method private permute' p =
      parent#permute p;
      Seq.iter (fun i s -> Info.set_label s (string_of_int i)) self#sources
    
    method private npos = Seq.lmap (fun v -> Info.pos (self#vinfo v))
    
    method draw =
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
        let p = Info.pos x in
        let d = 2. *. Info.radius x in
        let c = Info.color x in
        surface ~c (P.empty |> P.rect (Box2.v_mid p (Size2.v d d)));
        text p (string_of_int i)
      in
      let draw_ivertex _ x =
        let p = Info.pos x in
        let r = Info.radius x in
        let c = Info.color x in
        surface ~c (P.empty |> P.circle p r);
        text p (Info.label x)
      in
      let draw_edge _ x n =
        let p = Info.pos x in
        let r = Info.radius x in
        let c = Info.color x in
        surface ~c (Geometry.edge (Geometry.circle p r) (self#npos n));
        circle p r;
        text p (Info.label x)
      in
      self#iter_edges draw_edge;
      self#iter_sources draw_source;
      self#iter_ivertices draw_ivertex;
      !drawing
  end
