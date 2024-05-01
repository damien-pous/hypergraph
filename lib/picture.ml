open Types
open Gg
open Vg

let area = `O { P.o with P.width = Constants.linewidth }

let pentagon c r =
  let r = V2.smul r V2.oy in
  let p i = V2.add c (V2.ltr (M2.rot2 (i *. Float.pi /. 2.5)) r) in
  P.empty |>
    P.sub (p 0.) |>
    P.line (p 1.) |>
    P.line (p 2.) |>
    P.line (p 3.) |>
    P.line (p 4.) |>
    P.close

class basic_canvas: canvas =
  object(self)
    val mutable image = I.void
    method clear = image <- I.void
    method get = image
    method private blend i = image <- (image |> I.blend i)
    method path ?(color=Color.black) ?fill p =
      Option.iter (fun fill -> self#blend (I.const fill |> I.cut p)) fill;
      self#blend (I.const color |> I.cut ~area p)
    method circle ?color ?fill c =
      self#path ?color ?fill (P.empty |> P.circle c.center c.radius)
    method pentagon ?color ?fill c =
      self#path ?color ?fill (pentagon c.center c.radius)
    method box ?color ?fill b =
      self#path ?color ?fill (P.empty |> P.rect b)
    method point ?color p =
      let fill = color in
      self#path ?color ?fill (P.empty |> P.circle p Constants.pradius)
    method segment ?color x y =
      self#path ?color (P.empty |> P.sub x |> P.line y)
    method line ?color l =
      let d = V2.smul 1000. l.dir in
      self#point ?color l.point;
      self#segment ?color (V2.sub l.point d) (V2.add l.point d)
    method text p text =
      let p = V2.sub p
                (V2.v (float_of_int (String.length text) *. Constants.fontsize/.3.)
                   (Constants.fontsize/.3.)) in
      self#blend (I.move p (I.const Color.black |> I.cut_glyphs ~text Constants.font []))
  end

class void_canvas: canvas =
  object
    method clear = ()
    method get = I.void
    method path ?color ?fill _ = ignore (color,fill) 
    method circle ?color ?fill _ = ignore (color,fill) 
    method pentagon ?color ?fill _ = ignore (color,fill) 
    method box ?color ?fill _ = ignore (color,fill) 
    method point ?color _ = ignore color 
    method segment ?color _ _ = ignore color 
    method line ?color _ = ignore color 
    method text _ _ = ()
  end

class virtual virtual_arena =
  object(self)
        
    method virtual private dpointer: float*float
    method virtual private dsize: float*float
    method virtual private refresh: unit

    val mutable view = Box2.empty
    method view = view

    method private point_of_dpoint (x,y) =
      let w,h = self#dsize in
      (* p is (x,y) in [0;1]x[0;1] *)
      let p = P2.v
                (x /. w)
                (1. -. y /. h)
      in
      V2.add (Box2.o view) (V2.mul p (Box2.size view))

    method private vector_of_dvector v =
      V2.sub (self#point_of_dpoint v) (self#point_of_dpoint (0.,0.))
    
    method private dpoint_of_point p =
      let w,h = self#dsize in
      let p = V2.div (V2.sub p (Box2.o view)) (Box2.size view) in
      let x',y' = V2.x p, V2.y p in
      (w *. x', h *. (1.-.y'))
    
    method pointer = self#point_of_dpoint self#dpointer

    method ensure b =
      let bw,bh = Box2.w b, Box2.h b in
      let w,h = self#dsize in
      if bw*.h <= bh*.w
      then view <- Box2.v_mid (Box2.mid b) (V2.smul 1.1 (V2.v (bh*.w/.h) bh))
      else view <- Box2.v_mid (Box2.mid b) (V2.smul 1.1 (V2.v bw (bw*.h/.w)));
      self#refresh

    method move v =
      view <- Box2.move (self#vector_of_dvector v) view;
      self#refresh

    method resize (w,h) =
      view <- Box2.v_mid (Box2.mid view)
                (V2.sub (self#point_of_dpoint (w,0.)) (self#point_of_dpoint (0.,h)));      
      (* note: self#refresh to be called concrete subclasses *)
    
    method zoom s =
      let c = self#pointer in
      let v = V2.sub c (Box2.o view) in
      view <- Box2.v (V2.sub c (V2.smul s v)) (V2.smul s (Box2.size view));
      self#refresh
    
  end
