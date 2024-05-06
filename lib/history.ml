type 'a t = { mutable past: 'a list; mutable future: 'a list }

let create() = { past = []; future = [] }
let save h x = h.past <- x::h.past; h.future <- []
let reset h = h.past <- []; h.future <- []
let undo h =
  match h.past with
  | [] -> None
  | x::past -> h.past <- past; h.future <- x::h.future; Some x
let redo h =
  match h.future with
  | [] -> None
  | x::future -> h.future <- future; h.past <- x::h.past; Some x
