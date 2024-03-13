open Types
open Misc

module E(M: ALGEBRA) =
  struct  
    include M
    let bigpar n = big par (nil n)
    let cnv u =
      let k = arity u in
      if k<2 then failwith "invalid converse arity";
      prm (Perm.of_cycle[k-1;k]) u
    let inj k i u =
      let p,n = Inj.extend i k in
      let rec l = function
        | 0 -> u
        | n -> lft (l(n-1))
      in
      prm p (l n)
    let ser l =
      let k = List.length l+1 in
      bigpar k (List.mapi (fun i u -> prm (Perm.of_cycle[i+1;k]) (lft u)) l)
    let dot i u v = fgt i (ser [v;u])  
    let str i l =
      let k = List.length l+1 in
      fgt i (bigpar k (List.mapi (fun i u -> inj k (Inj.of_list[i+1;k]) u) l))  
    let size u = arity u + isize u + esize u
  end

module S(M: IALGEBRA) =
  struct
    type 'a ru = 'a Raw.u
    type 'a rt = 'a Raw.t
    module U = struct
      include E(M)
      module I = M.I
      module IRU = I(Raw.U)
      type 'a r = 'a Raw.u
      let raw u = IRU.eval u
      let pp mode f u = Raw.U.pp mode f (raw u)
    end
    type 'a u = 'a U.t
    type 'a t = 'a seq * 'a u
    type ('a,'b) m = ('a,'b) mapper
    let arity (s,_) = Seq.size s
    let esize (_,u) = U.esize u
    let isize (_,u) = U.isize u
    let width (_,u) = U.width u   (* check *)
    let size x = arity x + esize x + isize x
    let source s u = assert (Seq.size s = U.arity u); (s,u)
    let map f (s,u) = (Seq.imap f.fs s, U.map f.fu u)
    module SI(N: SEALGEBRA) = struct
      module UI = M.I(N.U)
      let eval (s,u) = N.source s (UI.eval u)
    end
    module SIR = SI(Raw)
    let raw u = SIR.eval u
    let pp mode f u = Raw.pp mode f (raw u)
  end

(* module X(M: ALGEBRA with type ('a,'b) m = ('a,'b) umapper) *)
(*          (I: functor(X: EALGEBRA) -> sig val eval: 'a M.t -> 'a X.t end) = *)
(*   struct *)
(*     module IRU = I(Raw.U) *)
(*     let uraw = IRU.eval  *)
(*     module M' = struct *)
(*       include M *)
(*       let pp mode f u = Raw.U.pp mode f (uraw u) *)
(*     end *)
(*     module U' = struct *)
(*       include E(M') *)
(*       module INIT = I *)
(*     end *)
(*     include S(U') *)
(*     module SIR = SINIT(Raw) *)
(*     let raw = SIR.eval *)
(*     let pp mode f u = Raw.pp mode f (raw u) *)
(*   end *)

