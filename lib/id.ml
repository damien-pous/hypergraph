module Make(M: sig val prefix: string end) = struct
  type t = unit ref
  let fresh (): t = ref ()
  let to_int (i: t): int = Obj.magic i
  let pp f i = Format.fprintf f "%s%i" M.prefix (to_int i)
  
  type 'a map = (t*'a) list
  let empty = []
  let add i x q = (i,x)::q
  let single i x = add i x empty
  let rem = List.remove_assq
  let get l i = List.assq i l
  let union = List.rev_append
  let iter f = List.iter (fun (i,x) -> f i x)
  let size = List.length
  let map f = List.rev_map (fun (i,x) -> i, f x)
  let imap f = List.rev_map (fun (i,x) -> i, f i x)
  let rec omap f = function
    | [] -> []
    | (i,x)::q -> match f x with None -> omap f q | Some y -> (i,y)::omap f q
  let rec find f = function
    | [] -> None
    | (i,x)::_ when f i x -> Some (i,x)
    | _::q -> find f q
  let keys m = List.fold_right (fun (k,_) -> Set.add k) m Set.empty
  let values m = List.fold_right (fun (_,v) -> Set.add v) m Set.empty
end
