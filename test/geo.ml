open Hypergraphs

open GMain

open Gg
open Vg

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

let x = ref (P2.v 0.3 0.5)
let y = ref (P2.v 0.7 0.5)
let z = ref (P2.v 0.5 0.65)
let c = ref (P2.v 0.65 0.6)
let r = ref 0.03
let s = ref false
let move = ref c
let mode = ref 0

let repaint () =    
  let cr = Cairo_gtk.create da#misc#window in
  let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
  let draw = new Draw.pic in
  let _ = draw#blend (I.const Color.white) in
  let _ = Geometry.set_debug draw in
  let c = Geometry.circle !c !r in
  let x = !x in
  let y = !y in
  let z = !z in

  (match !mode with
   | 0 ->
      draw#segment x c.center;
      draw#segment y c.center;
      draw#path (Geometry.curve ~lx:!s (* ~ly:!s *) c x y);
      draw#point x;
      draw#point y;
      ()

   | 1 ->
      draw#surface (Geometry.edge1 c x);
      draw#point x;
      ()

   | 2 ->
      draw#surface (Geometry.edge2 c x y);
      draw#point x;
      draw#point y;
      ()

   | 3 ->
      draw#path (Geometry.edge3 c x y z);
      draw#point x;
      draw#point y;
      draw#point z;
      ()
   
   | _ -> failwith "invalid mode"
  );
  draw#circle c;
  

  let _ = Vgr.render vgr (`Image (size, view, draw#get)) in
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
             | "s" -> s := not !s
             | "+" -> r := !r *. 1.1
             | "-" -> r := !r /. 1.1
             | _ -> ()
            ); repaint(); true)
let _ =
  window#show ();
  Main.main ()
