open Misc

(* stored in reverse order *)
type 'a t = 'a list

let get l i = List.nth l (List.length l - i)
let init n f = List.init n (fun i -> f (n-i))
let to_list = List.rev
let lmap = List.rev_map
let mem = List.mem
let empty = []
let snoc q x = x::q
let case = function [] -> None | x::q -> Some(q,x)
let is_empty l = l=[]

let map = List.map
let iter f l =
  let n = List.length l in
  List.iteri (fun i x -> f (n-i) x) l
let fold f a l = List.fold_right f a l

let rec omap f = function
  | [] -> []
  | x::q -> match f x with None -> omap f q | Some y -> y::omap f q

let rec ofold2 f a h k =
  match h,k with
  | [],[] -> Some a
  | [],_ | _,[] -> failwith "ofold2: not the same size"
  | x::h,y::k -> match f a x y with
                 | Some a -> ofold2 f a h k
                 | None -> None 

let rec id = function
  | 0 -> []
  | k -> k::id (k-1)

let size = List.length

let pp ppe f l = Format.fprintf f "{%a}" (pp_print_list "," ppe) (to_list l)
