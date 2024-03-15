open Hypergraphs
open GMain
open Gg

(* initial width/height *)
let width = 600
let height = 600

let draw = new Picture.basic_canvas
let _ = Geometry.set_debug draw

let _ = GtkMain.Main.init ()
let window = GWindow.window ~width ~height ~title:"HG - geometry debug" ()
let vbox = GPack.vbox ~packing:window#add ()
let da = GMisc.drawing_area ~packing:vbox#add ()
let arena = GArena.create ~width ~height ~window da draw ()
let _ = arena#ensure (Box2.v V2.zero (Size2.v 300. 300.))

let fill = Color.gray ~a:0.5 0.6
let x = ref (P2.v  70. 100.)
let y = ref (P2.v 230. 100.)
let z = ref (P2.v 150. 160.)
let c = ref (P2.v 190. 140.)
let r = ref (Constants.eradius 2)
let s = ref false
let move = ref c
let mode = ref 0

let repaint () =
  draw#clear;
  let c = Geometry.circle !c !r in
  let x = !x in
  let y = !y in
  let z = !z in
  (match !mode with
   | 0 ->
      draw#segment x c.center;
      draw#segment y c.center;
      draw#path ~fill (Geometry.curve ~lx:!s (* ~ly:!s *) c x y);
      draw#point x;
      draw#point y;
      ()

   | 1 ->
      draw#path ~fill (Geometry.edge1 c x);
      draw#point x;
      ()

   | 2 ->
      draw#path ~fill (Geometry.edge2 c x y);
      draw#point x;
      draw#point y;
      ()

   | 3 ->
      draw#path ~fill (Geometry.edge3 c x y z);
      draw#point x;
      draw#point y;
      draw#point z;
      ()
   
   | _ -> failwith "invalid mode"
  );
  draw#circle c;
  arena#refresh

let update () =
  !move := arena#pointer;
  repaint();
  true

let motion ev =
  GdkEvent.Motion.state ev = 256 &&
  update ()

let button ev =
  GdkEvent.Button.state ev = 0 &&
  (* GdkEvent.Button.button ev = 0 && *)
  update ()

let key_press e = 
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
  ); repaint(); true

let _ = GtkBase.Widget.add_events da#as_widget [`BUTTON_MOTION; `BUTTON_PRESS]
let _ = da#event#connect#motion_notify ~callback:motion
let _ = da#event#connect#button_press ~callback:button
let _ = arena#enable_moves
let _ = window#event#connect #key_press ~callback:key_press
let _ = window#connect#destroy ~callback:Main.quit
let _ =
  window#show ();
  repaint();
  Main.main()
