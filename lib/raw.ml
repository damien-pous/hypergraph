open Types
open Misc

type 'a s = 
  | Nil
  | Par of 'a s * 'a s
  | Fgt of 'a * 'a s
  | Lft of 'a s
  | Prm of perm * 'a s
  | Edg of 'a
  (* derived *)
  | Inj of inj * 'a s
  | Ser of 'a s list
  | Str of 'a * 'a s list
  | Dot of 'a * 'a s * 'a s
  | Cnv of 'a s
type 'a u = 'a s

module U = struct
type 'a t = 'a s

let nil' = Nil
let nil _ = nil'
let rec par u v =
  match u,v with
  | Nil,w | w,Nil -> w
  | Par(u,v),w -> par u (Par(v,w))
  | _ -> Par(u,v)
let bigpar _ = big par Nil
let fgt x u = Fgt(x,u)
let lft u = Lft u
let cnv u =
  match u with
  | Cnv u -> u
  | u -> Cnv u
let prm p u =
  if Perm.eq p Perm.id then u else
  let k = Perm.size p in
  if k>=2 && Perm.eq p (Perm.of_cycle [k-1;k]) then cnv u else
  match u with
  | Prm(q,u) -> Prm (Perm.comp q p, u)
  | u -> Prm(p,u)
let edg' x = Edg x
let edg _ = edg'
let inj' i u = Inj(i,u)
let inj k i u = if Inj.is_id k i then u else  inj' i u
(* let inj _ = inj' *)
let ser l = Ser l
let str i l = Str(i,l)
let rec dot x u w =
  match u with
  | Dot(y,u,v) -> dot y u (Dot(x,v,w))
  | _ -> Dot(x,u,w)

let arity,source =
  let zero = 0,true in
  let strict k = k,false in
  let weak k = k,true in
  let max (i,b as ib) (j,c as jc) =
    match b,c with
    | true,true -> weak (max i j)
    | false,true when j<=i -> ib
    | true,false when i<=j -> jc
    | false,false when i=j -> ib
    | _ -> failwith "arity mismatch"
  in
  let pred = function
    | 0,true as a -> a
    | 0,false -> failwith "arity mismatch"
    | i,b -> i-1,b
  in
  let succ = function
    | i,b -> i+1,b
  in
  let eq k = function
    | i,false when i=k -> ()
    | i,true when i<=k -> ()
    | _ -> failwith "arity mismatch"
  in
  let rec arity = function
    | Nil | Edg _ -> zero
    | Par(u,v) -> max (arity u) (arity v)
    | Fgt(_,u) -> pred (arity u)
    | Lft u -> succ (arity u)
    | Prm(p,u) -> max (weak (Perm.size p)) (arity u)
    | Inj(i,u) -> eq (Inj.dom i) (arity u); (weak (Inj.cod i))
    | Ser l ->
       let k = List.length l in
       List.iter (fun u -> eq k (arity u)) l; strict (k+1)
    | Str(_,l) ->
       let k = List.length l in
       List.iter (fun u -> eq 2 (arity u)) l; strict k
    | Dot(_,u,v) -> eq 2 (arity u); eq 2 (arity v); strict 2
    | Cnv u -> max (weak 2) (arity u)
  in
  (fun u -> fst (arity u)), (fun s u -> ignore (max (strict (Seq.size s)) (arity u)); (s,u))

let rec isize = function
  | Nil | Edg _ -> 0
  | Par(u,v)  | Dot(_,u,v) -> isize u + isize v
  | Fgt(_,u) -> 1 + isize u
  | Ser l -> big (+) 0 (List.map isize l)
  | Str(_,l) -> 1 + big (+) 0 (List.map isize l)
  | Lft u | Prm(_,u) | Inj(_,u) | Cnv u -> isize u

let rec esize = function
  | Nil -> 0
  | Edg _ -> 1
  | Par(u,v) | Dot(_,u,v) -> esize u + esize v
  | Ser l | Str(_,l) -> big (+) 0 (List.map esize l)
  | Fgt(_,u) | Lft u | Prm(_,u) | Inj(_,u) | Cnv u -> esize u
    
let size g = arity g + isize g + esize g

let width _ = assert false      (* not useful raw terms? *)

module I(X: EALGEBRA) = struct
  let rec xeval k = function
    | Nil        -> X.nil k
    | Par(u,v)   -> X.par (xeval k u) (xeval k v)
    | Fgt(x,u)   -> X.fgt x (xeval (k+1) u)
    | Lft u      -> X.lft (xeval (k-1) u)
    | Prm(p,u)   -> X.prm p (xeval k u)
    | Edg l      -> X.edg k l
    | Inj(i,u)   -> X.inj k i (xeval (Inj.dom i) u)
    | Ser l      -> X.ser (List.map (xeval (k-1)) l)
    | Str(x,l)   -> X.str x (List.map (xeval 2) l)
    | Dot(x,u,v) -> X.dot x (xeval 2 u) (xeval 2 v)
    | Cnv u      -> X.cnv (xeval k u)
  let eval u = xeval (arity u) u
end

let xmap f =
  let rec xmap k = function
    | Nil        -> Nil
    | Par(u,v)   -> Par(xmap k u, xmap k v)
    | Fgt(x,u)   -> Fgt(f.fi x, xmap (k+1) u)
    | Lft u      -> Lft(xmap (k-1) u)
    | Prm(p,u)   -> Prm(p, xmap k u)
    | Edg x      -> Edg(f.fe k x)
    | Inj(i,u)   -> Inj(i,xmap (Inj.dom i) u)
    | Ser l      -> Ser(List.map (xmap (k-1)) l)
    | Str(x,l)   -> Str(f.fi x, List.map (xmap 2) l)
    | Dot(x,u,v) -> Dot(f.fi x, xmap 2 u, xmap 2 v)
    | Cnv u      -> Cnv(xmap 2 u)
  in xmap
let map f u = xmap f (arity u) u

type l = BOT | PAR | DOT | PRF | CNV 
let head = function
  | Par(_,_)   -> PAR
  | Fgt(_,_)
  | Lft _   
  | Prm(_,_)
  | Inj(_,_)   -> PRF
  | Dot(_,_,_) -> DOT
  | Cnv _      -> CNV
  | Ser _   
  | Str(_,_)
  | Nil     
  | Edg _      -> BOT

let pp mode =
  let ppx f x = x#pp mode f in
  let rec pp o f u =
    let i = head u in
    let paren fmt = if o <= i then fmt else "("^^fmt^^")" in
    let pp = pp i in
    match u with
    | Nil        -> Format.fprintf f "0"
    | Par(u,v)   -> Format.fprintf f (paren "%a | %a") pp u pp v
    | Edg l      -> ppx f l
    | Fgt(x,u)   -> Format.fprintf f (paren "f%a%a") ppx x pp u
    | Lft u      -> Format.fprintf f (paren "l%a") pp u
    | Prm(p,u)   -> Format.fprintf f (paren "%a%a") Perm.pp p pp u
    | Inj(i,u)   -> Format.fprintf f (paren "%a%a") Inj.pp i pp u
    | Ser l      -> Format.fprintf f "s(%a)" (pp_print_list "," pp) l
    | Str(x,l)   -> Format.fprintf f "*%a(%a)" ppx x (pp_print_list "," pp) l
    | Dot(x,u,v) -> Format.fprintf f (paren "%a.%a%a") pp u ppx x pp v
    | Cnv u      -> Format.fprintf f (paren "%a'") pp u
in pp BOT

end
let nil' = U.nil'
let edg' = U.edg'
let inj' = U.inj'

type 'a t = 'a seq * 'a u

let arity (s,_) = Seq.size s
let isize (_,u) = U.isize u
let esize (_,u) = U.esize u
let width _ = assert false      (* not useful on raw terms? *)

let source = U.source
let flexible f u = let k = U.arity u in source (Seq.init k f) u

let map f (s,u) = (Seq.imap f.fs s, U.xmap f (Seq.size s) u)

let pp mode f (s,u) =
  if mode=Sparse || Seq.forall (fun s -> s#pp_empty mode) s then
    let k = Seq.size s in
    if U.arity u = k then U.pp mode f u
    else Format.fprintf f "#%i %a" k (U.pp mode) u
  else
    let ppx f x = x#pp mode f in
    Format.fprintf f "#%a %a" (pp_print_list "," ppx) (Seq.to_list s) (U.pp mode) u

module SI(X:SEALGEBRA) = struct
  module UI = U.I(X.U)
  let eval (s,u) = X.source s (UI.xeval (Seq.size s) u)
end
