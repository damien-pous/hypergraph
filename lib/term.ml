open Misc
open Common

type 'a s =
  | Nil of int
  | Par of 'a s * 'a s
  | Fgt of 'a * 'a s
  | Lft of 'a s
  | Prm of perm * 'a s
  | Edg of int * 'a

module M = struct
type 'a t = 'a s

let rec arity = function
  | Nil k | Edg(k,_) -> k
  | Par(u,_) -> arity u
  | Fgt(_,u) -> arity u - 1
  | Lft u -> arity u + 1
  | Prm(_,u) -> arity u

let rec isize = function
  | Nil _ | Edg(_,_) -> 0
  | Par(u,v) -> isize u + isize v
  | Fgt(_,u) -> isize u + 1
  | Lft u -> isize u
  | Prm(_,u) -> isize u

let rec esize = function
  | Nil _ -> 0
  | Edg(_,_) -> 1
  | Par(u,v) -> esize u + esize v
  | Fgt(_,u) | Lft u | Prm(_,u) -> esize u

let rec width = function
  | Nil k | Edg(k,_) -> k-1
  | Par(u,v) -> max (width u) (width v)
  | Fgt(_,u) -> width u
  | Lft u -> width u + 1
  | Prm(_,u) -> width u

let nil k = Nil k
let par u v = Par(u,v)
let fgt x u = Fgt(x,u)
let lft u = Lft u
let prm p u = Prm(p,u)
let edg k x = Edg(k,x)

let pp _ _ _ = assert false (* derived below *)

let map f =
  let rec map = function
    | Nil k    -> nil k
    | Par(u,v) -> par (map u) (map v)
    | Fgt(x,u) -> fgt (f.fi x) (map u)
    | Lft u    -> lft (map u)
    | Prm(p,u) -> prm p (map u)
    | Edg(k,l) -> edg k (f.fe l)
  in map

end
include M
include Extend(M)

module INIT(X: ALGEBRA) = struct
  let rec eval = function
    | Nil k    -> X.nil k
    | Par(u,v) -> X.par (eval u) (eval v)
    | Fgt(x,u) -> X.fgt x (eval u)
    | Lft u    -> X.lft (eval u)
    | Prm(p,u) -> X.prm p (eval u)
    | Edg(k,l) -> X.edg k l
  let seval (s,u) = (s,eval u)
end

let raw u = let module I = INIT(Raw) in I.eval u
let sraw u = let module I = INIT(Raw) in I.seval u 
let pp mode f u = Raw.pp mode f (raw u)
let spp mode f u = Raw.spp mode f (sraw u)
