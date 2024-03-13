open Common
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
type 'a t = 'a s
type 'a st = 'a seq * 'a t

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

let fix f k = source (Seq.init k f)
let flex f u = let k = arity u in fix f k u

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
let ssize (_,g) = size g

module INIT(X: EALGEBRA) = struct
  let rec eval k = function
    | Nil        -> X.nil k
    | Par(u,v)   -> X.par (eval k u) (eval k v)
    | Fgt(x,u)   -> X.fgt x (eval (k+1) u)
    | Lft u      -> X.lft (eval (k-1) u)
    | Prm(p,u)   -> X.prm p (eval k u)
    | Edg l      -> X.edg k l
    | Inj(i,u)   -> X.inj k i (eval (Inj.dom i) u)
    | Ser l      -> X.ser (List.map (eval (k-1)) l)
    | Str(x,l)   -> X.str x (List.map (eval 2) l)
    | Dot(x,u,v) -> X.dot x (eval 2 u) (eval 2 v)
    | Cnv u      -> X.cnv (eval k u)
  let seval (s,u) = (s,eval (Seq.size s) u)
  let eval u = eval (arity u) u
end

let map f =
  let rec map = function
    | Nil        -> Nil
    | Par(u,v)   -> Par(map u, map v)
    | Fgt(x,u)   -> Fgt(f.fi x, map u)
    | Lft u      -> Lft(map u)
    | Prm(p,u)   -> Prm(p, map u)
    | Edg x      -> Edg(f.fe x)
    | Inj(i,u)   -> Inj(i,map u)
    | Ser l      -> Ser(List.map map l)
    | Str(x,l)   -> Str(f.fi x, List.map map l)
    | Dot(x,u,v) -> Dot(f.fi x, map u, map v)
    | Cnv u      -> Cnv(map u)
  in map

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

let spp mode f x = spp pp ~arity mode f x
let smap f (s,u) = (Seq.imap f.fs s, map f.fo u)
