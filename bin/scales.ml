open Gg
open Vg

(* standalone program to understand scales, fontsizes, export alternatives *)

let inch = 72.27

(* in points *)
let fontsize = 11.
let font = Vg.Font.{name="Latin Modern Roman"; slant=`Italic; weight=`W100; size=fontsize }

let linewidth = 0.5
let pradius = 2.0

let area = `O { P.o with P.width = linewidth }
class pic =
  object(self)
    val mutable image = I.void
    val mutable texts = []
    method clear = image <- I.void; texts <- []
    method current = image
    method blend i = image <- (image |> I.blend i)
    method path ?(color=Color.black) p = self#blend (I.const color |> I.cut ~area p)
    method surface ?(color=Color.gray 0.5) p = self#blend (I.const color |> I.cut p); self# path p
    method circle ?color c r = self#path ?color (P.empty |> P.circle c r)
    method disc ?color c r = self#surface ?color (P.empty |> P.circle c r)
    method point ?color p = self#surface ?color (P.empty |> P.circle p pradius)
    method segment ?color x y = self#path ?color (P.empty |> P.sub x |> P.line y)
    method text p text =
      let p = V2.sub p
                (V2.v (float_of_int (String.length text) *. fontsize /. 3.)
                   (fontsize /. 3.)) in
      self#blend (I.move p (I.const Color.black |> I.cut_glyphs ~text font []))
    method text' p text = texts <- (p,text) :: texts
    method render cr size view =
      let vgr = Vgr.create (Vgr_cairo.target cr) `Other in
      let _ = Vgr.render vgr (`Image (size, view, image)) in
      let _ = Vgr.render vgr `End in
      (* we need to set the transformation matrix... *)
      Cairo.set_source_rgb cr 0.0 0.0 0.0;
      Cairo.select_font_face cr "Latin Modern Roman" ~slant:Cairo.Italic;
      Cairo.set_font_size cr fontsize;
      List.iter (fun (p,text) ->
          let te = Cairo.text_extents cr text in
          let x,y = V2.x p, -.V2.y p in
          Cairo.move_to cr
            (x -. te.width /. 2. -. te.x_bearing)
            (y -. te.height /. 2. -. te.y_bearing);
          Cairo.show_text cr text) texts;
      ()
  end

let draw = new pic
let _ = draw#circle V2.zero inch
let _ = draw#circle V2.zero fontsize
let _ = draw#point (V2.v (inch/.2.) (inch/.2.))
let _ = draw#text V2.zero "a"
let _ = draw#text' V2.zero "c"
let _ = draw#point (V2.v inch inch)
let _ = draw#text (V2.v inch inch) "b"
let _ = draw#point (V2.v inch (inch *. 0.66))
let _ = draw#text (V2.v inch (inch *. 0.66)) "E"

let w,h = 2. *. inch, 2. *. inch (* ~ 5x5 mm *)
let size = Size2.v w h
let view = Box2.v V2.zero size  
let i = Cairo.PDF.create "test.pdf" ~w ~h (* w,h in points *)
let cr = Cairo.create i
let _ = draw#render cr size view
let _ = Cairo.Surface.finish i
let _ = print_endline "exported to test.pdf"
