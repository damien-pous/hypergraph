open Misc

type t = int list

let rec check = function
  | [] -> true
  | x::q -> x>0 && not (List.mem x q) && check q

let of_list l =
  if check l then l else failwith "not an injection"

let is_id k =
  let rec is_id i = function
    | [] -> i=k+1
    | j::q -> i=j && is_id (i+1) q
  in is_id 1

let dom = List.length
let cod = List.fold_left max 0

let extend l k =
  let n = dom l in
  if k < cod l then failwith "Inj.extend: invalid arity";
  let rec complete k acc =
    if k = 0 then acc
    else if List.mem k l then complete (k-1) acc
    else complete (k-1) (k::acc)
  in 
  let r = k-n in
  Perm.of_list (l @ complete k []), r

let pp f i =
  match i with
  | [] -> Format.fprintf f "{}"
  | [x] when x>9 -> Format.fprintf f "{0%i}" x
  | _ ->
     let sep = if cod i > 9 then "," else "" in
     Format.fprintf f "{%a}" (pp_print_list sep Format.pp_print_int) i
