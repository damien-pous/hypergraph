open Misc

type 'a t = N | S of 'a | U of 'a t * 'a t

let empty = N
let single a = S a
let union x y =
  match x,y with
  | N,y -> y
  | x,N -> x
  | _ -> U (x,y)
let add a = union (single a)

let rec filter f = function
  | S i as x when f i -> x
  | U(x,y) -> union (filter f x) (filter f y)
  | _ -> N
let remq x = filter ((!=) x)

let case x =
  let rec case acc = function
  | N -> None
  | S x -> Some(x,acc)
  | U(x,y) -> case (union y acc) x
  in case N x
let is_empty l = l = N

let exists f =
  let rec exists acc = function
    | N -> false
    | S x -> f x acc
    | U(x,y) -> exists (union x acc) y || exists (union y acc) x
  in exists N

let rec partition f = function
  | S i as x -> if f i then N,x else x,N
  | U(x,y) ->
     let xf,xt = partition f x in
     let yf,yt = partition f y in
     union xf yf, union xt yt
  | _ -> N,N

let rec size = function
  | N -> 0
  | S _ -> 1
  | U(x,y) -> size x + size y

let rec map f = function
  | N -> N
  | S i -> S (f i)
  | U(x,y) -> U (map f x, map f y)

let rec iter f = function
  | N -> ()
  | S i -> f i
  | U(x,y) -> iter f x; iter f y

let rec fold f a = function
  | N -> a
  | S i -> f i a
  | U(x,y) -> fold f (fold f a y) x

let iteri f x =
  let rec iteri id = function
    | N -> id
    | S i -> f id i; id+1
    | U(x,y) -> iteri (iteri id x) y
  in ignore (iteri 1 x)
exception Found of int
let index j x =
  try
    iteri (fun id i -> if i==j then raise (Found id)) x;
    failwith "not found"
  with Found id -> id

let to_list l = fold List.cons [] l

let pp ppe f l = Format.fprintf f "{%a}" (pp_print_list "," ppe) (to_list l)

let lmap f l = to_list (map f l)
