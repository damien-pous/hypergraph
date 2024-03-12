type t = int list

let size = List.length

let max = List.fold_left max 0

let to_inj = Inj.of_list

let pp f i = Inj.pp f (to_inj i)

let rec check = function
  | [] -> true
  | x::q -> x>0 && (List.for_all ((<) x) q) && check q

let of_list l =
  if check l then l else failwith "not an increasing injection"

let rec mem x = function
  | [] -> false
  | y::q when y<x -> mem x q
  | y::_ -> x=y

let id k = List.init k (fun i -> i+1)

let empty = []

let rec merge h k = match h,k with
  | [],k -> k
  | h,[] -> h
  | x::h,((y::_) as yk) when x<y -> x::merge h yk
  | x::h,y::k when x=y -> x::merge h k
  | xh,y::k -> y::merge xh k

let rec index t i =
  match t with
  | j::q when j<i -> 1+index q i
  | j::_ when j=i -> 1
  | _ -> failwith "ISeq.reindex: not a subsequence"
let reindex s t = List.map (index t) s

let rec crop = function
  | [] | [_] -> []
  | x::q -> x::crop q

let map p s =
  (* Format.printf "<map %a %a@." Perm.pp p pp s; *)
  let s = List.map (Perm.apply p) s in
  (* Format.printf " map %a@." pp s; *)
  let p i = List.fold_left (fun n j -> if j<i then n+1 else n) 1 s in
  let p = Perm.of_list (List.map p s) in
  let s = Perm.lapply (Perm.inv p) s in
  (* Format.printf "<map %a %a@." pp s Perm.pp p; *)
  s,p
