open Misc

type t = int array

let id = [||]
let size = Array.length
let get p i = p.(i-1)           (* only on the support *)
let apply p i =                 (* everywhere *)
  assert (1<=i);
  if i<=size p then get p i else i
let sapply p s =
  Seq.init (Seq.size s) (fun i -> Seq.get s (get p i))
let lapply p s =
  List.init (List.length s) (fun i -> List.nth s (get p (i+1) - 1))

let build n f = Array.init n (fun i -> f (i+1))
let check n p = 
  for i = 1 to n do
    if get p i < 0 || get p i > n then failwith "not a permutation (out of bounds)";
    if not (Array.mem i p) then failwith "not a permutation"
  done

let of_fun n f =
  assert (0<=n);
  let p = build n f in
  check n p; p

let of_list l =
  let p = Array.of_list l in
  check (size p) p; p

let comp p q =
  let n = size p in
  let m = size q in
  build (max n m) (fun i -> apply q (apply p i))

let inv p =
  let rec find i j = if get p i = j then i else find (i+1) j in
  build (size p) (find 1)
  
let of_cycle = function
  | [] -> id
  | [x] -> if x<=0 then failwith "invalid cycle" else id
  | (x::_) as c ->
     let n = List.fold_left max 0 c in
     let rec check = function
       | [] -> ()
       | i::c -> if i<=0 || List.mem i c then failwith "invalid cycle" else check c
     in 
     let rec find c i = match c with
       | [j] when i=j -> x
       | [] | [_] -> i
       | j::k::_ when i=j -> k
       | _::c -> find c i
     in
    check c; build n (find c)

let cycles p =
  let cycle i =
    let rec xcycle j =
      if j=i then []
      else j::xcycle (get p j)
    in i::xcycle (get p i)
  in
  let cycles = ref [] in
  let seen = ref [] in
  for i = 1 to size p do
    if not (List.mem i !seen) then
      let c = cycle i in
      if List.length c > 1 then cycles := c :: !cycles;
      seen := List.rev_append c !seen
  done;
  !cycles

let eq p q =
  let rec eq = function
    | 0 -> true
    | i -> apply p i = apply q i && eq (i-1)
  in eq (max (size p) (size q))

let pp f p =
  if size p > 1 then
    let sep = if size p > 9 then "," else "" in
    if false then
      Format.fprintf f "[%a]" (pp_print_list sep Format.pp_print_int) (Array.to_list p)
    else
      List.iter
        (Format.fprintf f "(%a)" (pp_print_list sep Format.pp_print_int)) (cycles p)
