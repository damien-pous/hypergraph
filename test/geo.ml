open Hypergraphs

open GMain

open Gg
open Vg

let width = 1000
let height = 1000

let _ = GtkMain.Main.init ()
let window = GWindow.window ~width ~height ~title:"TW" ()
let backing = ref (GDraw.pixmap ~width ~height ~window ())
let vbox = GPack.vbox ~packing:window#add ()
let da = GMisc.drawing_area ~packing:vbox#add ()
let view = Box2.v V2.zero (V2.v 400. 400.)

let x = ref (P2.v 120. 200.)
let y = ref (P2.v 280. 200.)
let z = ref (P2.v 200. 260.)
let c = ref (P2.v 260. 240.)
let r = ref (Constants.eradius 2)
let s = ref false
let move = ref c
let mode = ref 0

let da_size() =
  let w,h = !backing#size in
  float_of_int w, float_of_int h
let da_size'() =
  let w,h = da_size() in Size2.v w h

let p2_of_pointer' (x,y) =
  let w,h = da_size() in
  (* p is (x,y) in [0;1]x[0;1] *)
  let p = P2.v
            (x /. w)
            (1. -. y /. h)
  in
  let v = view in
  V2.add (Box2.o v) (V2.mul p (Box2.size v))

let p2_of_pointer (x,y) = p2_of_pointer' (float_of_int x, float_of_int y)

let pointer() = p2_of_pointer da#misc#pointer

let repaint () =    
  let cr = Cairo_gtk.create !backing#pixmap in 
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
  let _ = Vgr.render vgr (`Image (da_size'(), view, draw#get)) in
  let _ = Vgr.render vgr (`End) in
  da#misc#draw None

let update_params _ =
  !move := pointer();
  repaint();
  true

let _ = GtkBase.Widget.add_events da#as_widget [`BUTTON_MOTION; `BUTTON_PRESS(* ; `BUTTON_RELEASE *)]
let _ = da#event#connect#motion_notify ~callback:update_params
let _ = da#event#connect#button_press ~callback:update_params
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
  da#event#connect#expose ~callback:(fun ev ->
      let area = GdkEvent.Expose.area ev in
      let x = Gdk.Rectangle.x area in
      let y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area in
      let height = Gdk.Rectangle.width area in
      let drawing =
        da#misc#realize ();
        new GDraw.drawable da#misc#window
      in
      drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
      false)
let _ =
  da#event#connect#configure ~callback:(fun ev ->
      let width = GdkEvent.Configure.width ev in
      let height = GdkEvent.Configure.height ev in
      let pixmap = GDraw.pixmap ~width ~height ~window () in
      backing := pixmap;
      repaint();
      false)
let _ =
  window#show ();
  Main.main ()
