let pterm u = let module I=Term.SI(PTerm) in I.eval u
let nterm u = let module I=Term.SI(NTerm) in I.eval u

let plain u = PTerm.term (pterm u)
let normalise u = NTerm.term (nterm u)

let graph_of_term u = let module I=Term.SI(Graph) in I.eval u
let term_of_graph g = let module I=Graph.SI(Term) in I.eval g

module U = struct
let pterm u = let module I=Term.U.I(PTerm.U) in I.eval u
let nterm u = let module I=Term.U.I(NTerm.U) in I.eval u

let plain u = PTerm.U.term (pterm u)
let normalise u = NTerm.U.term (nterm u)

let graph_of_term u = let module I=Term.U.I(Graph.U) in I.eval u
let term_of_graph g = let module I=Graph.U.I(Term.U) in I.eval g
end
