(*
  current element is first element of [right] field
  invariant: if right is empty then so is left
 *)
type 'a t = { left: 'a list; right: 'a list }

let of_list right = { left=[]; right }
let to_list x = List.rev_append x.left x.right
let pos x = List.length x.left + 1
let size x = List.length x.left + List.length x.right
let count f x = List.length (List.filter f (to_list x))

let same x y = to_list x = to_list y

let not_empty x msg =
  if x.right = [] then Misc.failwith "Stack.%s: empty stack" msg
  
let current x =
  not_empty x "current";
  List.hd x.right

let move_left x =
  match x.left with
  | [] -> x
  | v::left -> { left; right=v::x.right}
let move_right x =
  match x.right with
  | [] 
  | [_] -> x
  | v::right -> { left=v::x.left; right}

let pop x =
  not_empty x "pop";  
  match x.right with
  | _::((_::_) as right) -> { x with right }
  | _ -> match x.left with
         | [] -> of_list []
         | v::left -> {left; right=[v]}
  
let replace x v =
  not_empty x "replace";  
  { x with right=v::List.tl x.right }

let push_here x v =
  { x with right=v::x.right }

let push_right x v =
  not_empty x "push_right";
  { x with right=List.hd x.right::v::List.tl x.right }
