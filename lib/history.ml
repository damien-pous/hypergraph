type 'a t = ('a list * 'a * 'a list) ref

let past h = let (l,_,_) = !h in l
let present h = let (_,x,_) = !h in x
let future h = let (_,_,l) = !h in l

let create x = ref ([],x,[])
let save h x = h := (present h::past h, x, [])
let clear h = h := ([],present h,[]) 
let undo h =  
  match past h with
  | [] -> None
  | x::past -> h := (past, x, present h::future h); Some x
let redo h =  
  match future h with
  | [] -> None
  | x::future -> h := (present h::past h, x, future); Some x
