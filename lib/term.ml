open Types
open Misc

type 'a s = 
  | Nil of int
  | Par of 'a s * 'a s
  | Fgt of 'a * 'a s
  | Lft of 'a s
  | Prm of perm * 'a s
  | Edg of int * 'a
  (* derived *)
  | Inj of int * inj * 'a s
  | Ser of 'a s list
  | Str of 'a * 'a s list
  | Dot of 'a * 'a s * 'a s
  | Cnv of 'a s
type 'a u = 'a s

module U = struct
type 'a t = 'a s

let rec arity = function
  | Nil k | Edg (k,_) | Inj (k,_,_) -> k
  | Par(u,_) | Prm(_,u) | Cnv u -> arity u
  | Fgt(_,u) -> arity u -1
  | Lft u -> arity u + 1
  | Ser l -> List.length l + 1
  | Str(_,l) -> List.length l
  | Dot(_,_,_) -> 2

let nil k = Nil k
let edg k x = Edg(k,x)
let rec par u v =
  match u,v with
  | Nil _,w | w,Nil _ -> w
  | Par(u,v),w -> par u (Par(v,w))
  | _ -> Par(u,v)
let bigpar k = big par (nil k)
let fgt x u = Fgt(x,u)
let lft u = Lft u
let prmr u = let k = arity u in Perm.of_cycle [k-1;k]
let is_prmr p u = Perm.eq p (prmr u)
let rec xprm b p = function
  | Prm(q,u) -> xprm b (Perm.comp q p) u
  | u -> if Perm.eq p Perm.id then u
         else if b && is_prmr p u then Cnv u
         else Prm(p,u)
let prm p = xprm true p
let cnv u = prm (prmr u) u
let inj k i u =
  if Inj.is_id k i then u
  else Inj(k,i,u)
(* the above three operations are not safe on flexible terms
   (because converse/inj arity is flexible), instead,
   we use the following ones during parsing *)
let flexible_prm p = xprm false p
let flexible_cnv u =
  match u with
  | Cnv u -> u
  | u -> Cnv u
let flexible_inj k i u = Inj(k,i,u)
let ser l = Ser l
let str i l = Str(i,l)
let rec dot x u w =
  match u with
  | Dot(y,u,v) -> dot y u (Dot(x,v,w))
  | _ -> Dot(x,u,w)

let rec isize = function
  | Nil _ | Edg _ -> 0
  | Par(u,v)  | Dot(_,u,v) -> isize u + isize v
  | Fgt(_,u) -> 1 + isize u
  | Ser l -> big (+) 0 (List.map isize l)
  | Str(_,l) -> 1 + big (+) 0 (List.map isize l)
  | Lft u | Prm(_,u) | Inj(_,_,u) | Cnv u -> isize u

let rec esize = function
  | Nil _ -> 0
  | Edg _ -> 1
  | Par(u,v) | Dot(_,u,v) -> esize u + esize v
  | Ser l | Str(_,l) -> big (+) 0 (List.map esize l)
  | Fgt(_,u) | Lft u | Prm(_,u) | Inj(_,_,u) | Cnv u -> esize u
    
let size g = arity g + isize g + esize g

let width _ = assert false      (* not useful raw terms? *)

module I(X: EALGEBRA) = struct
  let rec eval = function
    | Nil k      -> X.nil k
    | Par(u,v)   -> X.par (eval u) (eval v)
    | Fgt(x,u)   -> X.fgt x (eval u)
    | Lft u      -> X.lft (eval u)
    | Prm(p,u)   -> X.prm p (eval u)
    | Edg (k,l)  -> X.edg k l
    | Inj(k,i,u) -> X.inj k i (eval u)
    | Ser l      -> X.ser (List.map (eval) l)
    | Str(x,l)   -> X.str x (List.map eval l)
    | Dot(x,u,v) -> X.dot x (eval u) (eval v)
    | Cnv u      -> X.cnv (eval u)
end

let map f = 
  let rec map = function
    | Nil k      -> Nil k
    | Par(u,v)   -> Par(map u, map v)
    | Fgt(x,u)   -> Fgt(f.fi x, map u)
    | Lft u      -> Lft(map u)
    | Prm(p,u)   -> Prm(p, map u)
    | Edg(k,x)   -> Edg(k,f.fe k x)
    | Inj(k,i,u) -> Inj(k,i,map u)
    | Ser l      -> Ser(List.map map l)
    | Str(x,l)   -> Str(f.fi x, List.map map l)
    | Dot(x,u,v) -> Dot(f.fi x, map u, map v)
    | Cnv u      -> Cnv(map u)
  in map

type l = BOT | PAR | DOT | PRF 
let head = function
  | Par(_,_)   -> PAR
  | Fgt(_,_)
  | Lft _   
  | Prm(_,_)
  | Cnv _
  | Inj(_,_,_) -> PRF
  | Dot(_,_,_) -> DOT
  | Ser _   
  | Str(_,_)
  | Nil _
  | Edg _      -> BOT

let pp mode =
  let ppx f x = x#pp mode f in
  let rec pp o f u =
    let i = head u in
    let paren fmt = if o <= i then fmt else "("^^fmt^^")" in
    let pp = pp i in
    match u with
    | Nil _      -> Format.fprintf f "0"
    | Par(u,v)   -> Format.fprintf f (paren "%a | %a") pp u pp v
    | Edg (_,l)  -> ppx f l
    | Fgt(x,u)   -> Format.fprintf f (paren "f%a%a") ppx x pp u
    | Lft u      -> Format.fprintf f (paren "l%a") pp u
    | Prm(p,u)   -> Format.fprintf f (paren "%a%a") Perm.pp p pp u
    | Cnv u      -> Format.fprintf f (paren "r%a") pp u
    | Inj(_,i,u) -> Format.fprintf f (paren "%a%a") Inj.pp i pp u
    | Ser l      -> Format.fprintf f "s(%a)" (pp_print_list "," pp) l
    | Str(x,l)   -> Format.fprintf f "*%a(%a)" ppx x (pp_print_list "," pp) l
    | Dot(x,u,v) -> Format.fprintf f (paren "%a.%a%a") pp u ppx x pp v
in pp BOT

end

type 'a t = 'a seq * 'a u

let arity (s,_) = Seq.size s
let isize (_,u) = U.isize u
let esize (_,u) = U.esize u
let width _ = assert false      (* not useful on raw terms? *)

let source s u =
  assert(Seq.size s = U.arity u);
  (s,u)

let map f (s,u) = (Seq.imap f.fs s, U.map f u)

let pp mode f (s,u) =
  if mode=Sparse || Seq.forall (fun s -> s#pp_empty mode) s then
    let k = Seq.size s in
    (* if U.arity u = k then U.pp mode f u else *)
    Format.fprintf f "#%i %a" k (U.pp mode) u
  else
    let ppx f x = x#pp mode f in
    Format.fprintf f "#%a %a" (pp_print_list "," ppx) (Seq.to_list s) (U.pp mode) u

module SI(X:SEALGEBRA) = struct
  module UI = U.I(X.U)
  let eval (s,u) = X.source s (UI.eval u)
end

module Flexible = struct
  type 'a t = 'a u
  let rec up k = function
    | Nil _      -> Nil k
    | Par(u,v)   -> Par(up k u, up k v)
    | Fgt(x,u)   -> Fgt(x, up (k+1) u)
    | Lft u      -> Lft(up (k-1) u)
    | Prm(p,u)   -> Prm(p, up k u)
    | Edg(_,x)   -> Edg(k,x)
    | Inj(_,i,u) -> Inj(k,i,u)
    | Cnv u      -> Cnv(up k u)
    | (Ser _ | Str(_,_) | Dot(_,_,_)) as u ->
       if k = U.arity u then u else failwith "arity mismatch"
  let up' k u =
    let n = U.arity u in
    if k=n then u
    else if n<k then up k u
    else failwith "arity mismatch"
  let nil() = U.nil 0
  let edg x = U.edg 0 x
  let par u v =
    let n = U.arity u in
    let m = U.arity v in
    if n=m then U.par u v
    else if n<m then U.par (up m u) v
    else U.par u (up n v)
  let fgt x u =
    match U.arity u with
    | 0 -> U.fgt x (up 1 u)
    | _ -> U.fgt x u
  let lft = U.lft
  let prm p u =
    let m,n = Perm.size p, U.arity u in
    if m<=n then U.flexible_prm p u
    else U.flexible_prm p (up m u)
  let inj i u = U.flexible_inj (Inj.cod i) i (up' (Inj.dom i) u)
  let ser l =
    let k = List.length l in
    U.ser (List.map (up' k) l)
  let str x l = 
    U.str x (List.map (up' 2) l)
  let dot x u v = U.dot x (up' 2 u) (up' 2 v)
  let cnv u =
    let n = U.arity u in
    if n<2 then U.flexible_cnv (up 2 u)
    else U.flexible_cnv u
end

let fixed s u =
  let m,n = Seq.size s, U.arity u in
  if n=m then s,u
  else if n<m then s,Flexible.up m u
  else failwith "arity mismatch"

let flexible f u =
  let k = U.arity u in
  Seq.init k f, u
