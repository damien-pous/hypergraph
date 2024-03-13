let failwith fmt =
  Format.kasprintf failwith fmt

let pp_print_sep sep f () =
  Format.pp_print_string f sep

let pp_print_list sep =
  Format.pp_print_list ~pp_sep:(pp_print_sep sep)

let rec big b z = function
  | [] -> z
  | [x] -> x
  | x::q -> b x (big b z q)

let sqr x = x *. x
