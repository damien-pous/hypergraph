open Hypergraphs

open GMain

open Gg
open Vg
open Geometry

let width = 1000
let height = 1000
let width' = float_of_int width
let height' = float_of_int height

let _ = GtkMain.Main.init ()
let window = GWindow.window ~width ~height ~title:"TW" ()
let vbox = GPack.vbox ~packing:window#add ()
let da = GMisc.drawing_area ~packing:vbox#add ()
let size = Size2.v width' height'
let view = Box2.unit
let area = `O { P.o with P.width = 0.001 }
let black = I.const Color.black
let white = I.const Color.white
let gray = I.const (Color.gray 0.5)
let blue = I.const (Color.v_srgb 0.0 0.4 0.7)
(* let violet = I.const (Color.v_srgb 0.7 0.2 0.7) *)

let x = ref (P2.v 0.3 0.5)
let y = ref (P2.v 0.7 0.5)
let z = ref (P2.v 0.5 0.65)
let c = ref (P2.v 0.65 0.6)
let r = ref 0.03
let move = ref c
let mode = ref 2

let repaint () =    
  let cr = Cairo_gtk.create da#misc#window in
  let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
  let drawing = ref white in
  let blend d = drawing := !drawing |> I.blend d in
  let node ?(color=black) ?(radius=0.005) c =
    blend (color |> I.cut (P.empty |> P.circle c radius))
  in
  let circle ?(color=black) c =
    blend (color |> I.cut ~area (P.empty |> P.circle c.center c.radius))
  in
  let line ?(color=black) x y =
    blend (color |> I.cut ~area (start x |> P.line y))
  in
  (* let vect ?(color=black) x d = *)
  (*   blend (color |> I.cut ~area (start x |> P.line ~rel:true d)) *)
  (* in *)
  let curve p = blend (blue |> I.cut ~area p) in
  let surface p = blend (gray |> I.cut p); curve p in

  let c = Geometry.circle !c !r in
  let x = !x in
  let y = !y in
  let z = !z in
  
  (match !mode with
   | 0 ->
      line x c.center;
      line y c.center;
      curve (Geometry.curve c x y);
      node x;
      node y;
      ()

   | 1 ->
      surface (Geometry.edge1 c x);
      node x;
      ()

   | 2 ->
      surface (Geometry.edge2 c x y);
      node x;
      node y;
      ()

   | 3 ->
      surface (Geometry.edge3 c x y z);
      node x;
      node y;
      node z;
      ()
   
   | _ -> failwith "invalid mode"
  );
  circle c;
  

  let _ = Vgr.render vgr (`Image (size, view, !drawing)) in
  let _ = Vgr.render vgr (`End) in
  ()

let update_params _ =
  let (px,py) = da#misc#pointer in
  let p = P2.v (float_of_int px /. width') (1. -. float_of_int py /. height') in
  !move := p;
  repaint();
  true

let _ = GtkBase.Widget.add_events da#as_widget [`BUTTON_MOTION; `BUTTON_PRESS(* ; `BUTTON_RELEASE *)]
let _ = da#event#connect#motion_notify ~callback:update_params
let _ = da#event#connect#button_press ~callback:update_params
let _ = da#event#connect#expose ~callback:(fun _ -> repaint (); false)
let _ = window#connect#destroy ~callback:Main.quit
let _ = window#event#connect #key_press ~callback:(fun e ->
            (match GdkEvent.Key.string e with
             | "q" -> Main.quit()
             | "c" -> move := c
             | "x" -> move := x
             | "y" -> move := y
             | "z" -> move := z
             | "o" -> mode := 0
             | "1" -> mode := 1
             | "2" -> mode := 2
             | "3" -> mode := 3
             | "+" -> r := !r *. 1.1
             | "-" -> r := !r /. 1.1
             | _ -> ()
            ); repaint(); true)
let _ =
  window#show ();
  Main.main ()
