open Types

type 'a s = int * (iseq * 'a fpt) set
and 'a fpt =
  | Fgt of 'a * 'a s
  | Edg of int * perm * 'a

let ismap f = Set.map (fun (i,x) -> i,f x)

module M = struct

type 'a t = 'a s
type ('a,'b) m = ('a,'b) umapper

let arity (k,_) = k
(* let fp_arity = function *)
(*   | Edg(k,_,_) -> k *)
(*   | Fgt(_,u) -> arity u - 1 *)

let rec isize (_,s) =
  Set.fold (fun (_,x) n -> n + fp_isize x) 0 s
and fp_isize = function
  | Edg(_,_,_) -> 0
  | Fgt(_,u) -> isize u + 1

let rec esize (_,s) =
  Set.fold (fun (_,x) n -> n + fp_esize x) 0 s
and fp_esize = function
  | Edg(_,_,_) -> 1
  | Fgt(_,u) -> esize u

let rec width (k,s) =
  Set.fold (fun (_,x) n -> max n (fp_width x)) (k-1) s
and fp_width = function
  | Edg(k,_,_) -> k-1
  | Fgt(_,u) -> width u

let nil k = (k, Set.empty)

let par (h,u) (k,v) = assert (h=k); (h,Set.union u v)

let edg k x = (k,Set.single(ISeq.id k,Edg(k,Perm.id,x)))

let rec prm p (k,s) =
  (k, Set.map
        (fun (i,x) ->
          let j,q = ISeq.map p i in
          j, fp_prm q x) s)
and fp_prm p = function
  | Edg(k,q,x) -> Edg(k,Perm.comp q p,x)
  | Fgt(x,u) -> Fgt(x,prm p u)
  
let lft (k,s) = (k+1,s)

let fgt x (k,s) =
  let u,v = Set.partition (fun (inj,_) -> ISeq.mem k inj) s in
  let l = Set.fold (fun (inj,_) -> ISeq.merge inj) ISeq.empty v in
  let v = Set.map (fun (inj,fp) -> (ISeq.reindex inj l,fp)) v in
  let i = ISeq.crop l in
  (k-1,Set.union (Set.single(i,Fgt(x,(ISeq.size i+1,v)))) u)

let map f =
  let rec map (k,s) = (k,ismap fp_map s)
  and fp_map = function
    | Fgt(x,u) -> Fgt(f.fi x, map u)
    | Edg(k,p,x) -> Edg(k,p,f.fe x)
  in map

module I(X: EALGEBRA) = struct
  let rec eval (k,s) =
    X.bigpar k (Set.lmap (fun (i,x) -> X.inj k (ISeq.to_inj i) (fp_eval x)) s)
  and fp_eval = function
    | Fgt(x,u) -> X.fgt x (eval u)
    | Edg(k,p,x) -> X.prm p (X.edg k x)
end

end include Functor.S(M)
