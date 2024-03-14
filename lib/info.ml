open Gg
open Types
open Misc

type kv = string*string
type kvl = kv list

let float_of_string x =
  try float_of_string x
  with _ -> failwith "not a float: %s" x
    
let p2_of_string s =
  let i = String.index s ',' in
  P2.v (float_of_string (String.sub s 0 i))
    (float_of_string (String.sub s (i+1) (String.length s-i-1)))

let string_of_p2 p =
  Format.sprintf "%f,%f" (P2.x p) (P2.y p)

let kv k v =
  (match k with
  | "radius" -> ignore(float_of_string v)
  | "pos" | "shift" -> ignore(p2_of_string v)
  | "color" -> ignore (Constants.color v)
  | _ -> ());
  k,v

let get_color l = Option.map Constants.color (List.assoc_opt "color" l)
let get_label l = Option.value ~default:"" (List.assoc_opt "label" l)

let same_label x y = x#label = y#label
let same_label_kvl x y = get_label x = get_label y

let pp_kvl f l =
  if l<>[] then
    Format.fprintf f "<%a>"
      (pp_print_list ";" (fun f (k,v)-> Format.fprintf f "%s=%s" k v))
      l

let forbidden s =
  s="" || String.contains s 'f' || String.contains s 'l' || String.contains s 's'

let pp_label f = function
  | s when forbidden s -> Format.fprintf f "-%s" s
  | s -> Format.pp_print_string f s


class virtual holder_ = 
  object
    method virtual private kvl: kvl
    method virtual private has: string -> bool
    method virtual private rem: string -> unit
    method virtual private add: string -> string -> unit
    method virtual private get: string -> string option
  end

class holder (l: kvl) =
  object(self)
    val mutable kvl = l
    method private kvl = kvl
    method private has k = List.mem_assoc k kvl 
    method private rem k = kvl <- List.remove_assoc k kvl 
    method private add k v = self#rem k; kvl <- (k,v)::kvl 
    method private get k = List.assoc_opt k kvl
  end

class virtual printer =
  object(self)
    inherit holder_
    val mutable label = ""
    method label = label
    method virtual private pp_label: pp_mode -> Format.formatter -> unit
    method virtual private pp_other: pp_mode -> Format.formatter -> unit
    method private update_kvl = ()
    method pp mode f = self#update_kvl; self#pp_label mode f; self#pp_other mode f
    method pp_empty (_: pp_mode): bool = assert false (* only for sources *)
  end

class virtual iprinter_ =
  object(self)
    inherit printer
    method private pp_label _ _ = ()
    method private pp_other mode f = if mode=Full then pp_kvl f self#kvl
    initializer label <- get_label self#kvl
  end
class iprinter l = object inherit holder l inherit iprinter_ end

class virtual sprinter_ i =
  object(self)
    inherit iprinter_
    method! pp_empty mode = mode=Sparse || (self#update_kvl; self#kvl=[])
    initializer if not (self#has "label") then label <- string_of_int i
  end
class sprinter i l =
  object
    inherit holder l
    inherit sprinter_ i
  end

class virtual eprinter_ =
  object(self)
    inherit printer
    method private pp_label _ f = pp_label f label
    method private pp_other mode f = if mode=Full then pp_kvl f self#kvl
    initializer label <- get_label self#kvl; self#rem "label"
  end
class eprinter _ l = object inherit holder l inherit eprinter_ end

let print_mapper = {fs=new sprinter;fi=new iprinter;fe=new eprinter}


class virtual draw =
  object(self)
    inherit holder_
    val mutable pos = V2.zero
    val mutable placed = false
    val mutable radius = 0.0
    val mutable scaled = false
    val mutable color = Constants.gray
    method pos = pos    
    method radius = radius
    method circle = {center = pos; radius}
    method color = color
    method placed = placed    
    method move p = pos <- p; placed <- true
    method scale s = radius <- radius *. s; scaled <- true
    method private update_kvl =
      if placed then self#add "pos" (string_of_p2 pos);
      if scaled then self#add "radius" (string_of_float radius);
    initializer
      (match self#get "pos" with Some p -> pos <- p2_of_string p; placed <- true | None -> ());
      (match self#get "radius" with Some r -> radius <- float_of_string r | None -> ());
      (match self#get "color" with Some c -> color <- Constants.color c | None -> ());      
  end

class idrawer l =
  object(self)
    inherit holder l
    inherit iprinter_
    inherit! draw
    initializer
      if not (self#has "radius") then radius <- Constants.iradius
  end

class sdrawer i l =
  object(self)
    inherit holder l
    inherit sprinter_ i
    inherit! draw
    initializer
      if not (self#has "radius") then radius <- Constants.sradius
  end

class edrawer k l =
  object(self)
    inherit holder l
    inherit eprinter_
    inherit! draw
    initializer
      color <- Constants.color' ?color:(get_color l) label;
      if not (self#has "radius") then radius <- Constants.eradius k;
      if not placed then 
        (match self#get "shift" with Some v -> pos <- p2_of_string v | None -> ());
  end

let draw_mapper = {fs=new sdrawer;fi=new idrawer;fe=new edrawer}


let drawable_ivertex p = new idrawer ["pos", string_of_p2 p]
let drawable_source i p = new sdrawer i ["pos", string_of_p2 p]
let drawable_edge k l = new edrawer k ["label", l]

