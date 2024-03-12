open Gg
open Misc

type kv = string*string
type kvl = kv list

type t = {
    kind: [`S | `I | `E];
    mutable pos: p2;
    mutable scale: float;
    mutable label: string;
    mutable color: Color.t;
    user_pos: p2 option;
    user_shift: v2;
    other: kvl;
  }
let pos x = x.pos
let label x = x.label
let scale x = x.scale
let color x = x.color
let user_pos x = x.user_pos
let user_shift x = x.user_shift
let radius x =
  x.scale *. (match x.kind with
              | `S -> Constants.sradius
              | `E -> Constants.eradius
              | `I -> Constants.iradius)

let set_pos x p = x.pos <- p
let set_scale x r = x.scale <- r
let set_label x l = x.label <- l
let set_color x c = x.color <- c

let float_of_string x =
  try float_of_string x
  with _ -> failwith "not a float: %s" x
    
let p2_of_string s =
  let i = String.index s ',' in
  P2.v (float_of_string (String.sub s 0 i))
    (float_of_string (String.sub s (i+1) (String.length s-i-1)))

let get_pos l = Option.map p2_of_string (List.assoc_opt "pos" l)
let get_color l = Option.map Constants.color (List.assoc_opt "color" l)
let get_label l = Option.value ~default:"" (List.assoc_opt "label" l)
let get_shift l = Option.value ~default:V2.zero (Option.map p2_of_string (List.assoc_opt "shift" l))
let get_scale l = Option.value ~default:1.0 (Option.map float_of_string (List.assoc_opt "scale" l))
let get_other = List.filter (fun (k,_) -> not (List.mem k ["pos";"label";"shift";"scale"]))

let kv_check = function
  | ("pos"|"shift"),x -> ignore(p2_of_string x)
  | ("scale"),x -> ignore(float_of_string x)
  | "color",x -> ignore(Constants.color x)
  | _ -> ()
let rec check = function
  | [] -> []
  | (x,_ as xy) :: q -> kv_check xy; xy :: List.remove_assoc x (check q)
let of_kvl ?label l =
  let kind,l = match label with
    | Some s -> `E,check (("label",s) :: l)
    | None -> `I,l
  in
  let label = get_label l in
  let scale = get_scale l in
  let user_pos = get_pos l in
  let pos = Option.value ~default:V2.zero user_pos in
  let color = Constants.color' ?color:(get_color l) label in
  let other = get_other l in
  let user_shift = get_shift l in
  { kind; pos; scale; label; color; user_pos; user_shift; other } 

let for_ivertex pos = 
  { kind=`I; pos; scale=1.0; label=""; color=Constants.gray;
    user_pos=None; user_shift=V2.zero; other=[] }

let for_source pos i = 
  { kind=`S; pos; scale=1.0; label=string_of_int i; color=Constants.gray;
    user_pos=None; user_shift=V2.zero; other=[] }

let for_edge ?color label n = 
  let pos x = x.pos in
  let pos = Geometry.center (Seq.lmap pos n) in
  let color = Constants.color' ?color label in
  { kind=`E; pos; scale=1.0; label; color;
    user_pos=None; user_shift=V2.zero; other=[] }

let same_label x y = label x = label y

let inside p i = Geometry.dist p (pos i) <= radius i

let kvl_of_info x =
  let l = x.other in
  let l =
    if x.user_shift = V2.zero then l
    else ("shift", Format.sprintf "%f,%f" (V2.x x.user_shift) (V2.y x.user_shift))::l
  in
  let l =
    if x.scale = 1.0 then l
    else ("scale", Format.sprintf "%f" x.scale)::l
  in
  let l =
    if x.pos = V2.zero then l
    else ("pos", Format.sprintf "%f,%f" (V2.x x.pos) (V2.y x.pos))::l
  in
  match x.label with
  | "" -> l
  | label -> ("label", label)::l

let pp_kvl f l =
  if l<>[] then
    Format.fprintf f "<%a>"
      (pp_print_list ";" (fun f (k,v)-> Format.fprintf f "%s=%s" k v))
      l

let pp_label f = function
  | "" -> Format.pp_print_char f '-'
  | s when List.mem s.[0] ['f';'l';'s'] -> Format.fprintf f "\\%s" s
  | s -> Format.pp_print_string f s

let pp ?(full=false) f x =
  match x.kind with
  | `I | `S -> if full then pp_kvl f (kvl_of_info x)
  | `E -> let l = List.remove_assoc "label" (kvl_of_info x) in
          if full
          then Format.fprintf f "%a%a" pp_label x.label pp_kvl l
          else pp_label f x.label
