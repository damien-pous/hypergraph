let term_of_raw u = let module I=Raw.SI(Term) in I.eval u
let nterm_of_raw u = let module I=Raw.SI(NTerm) in I.eval u
let graph_of_raw u = let module I=Raw.SI(Graph) in I.eval u
let graph_of_term u = let module I=Term.SI(Graph) in I.eval u
let graph_of_nterm u = let module I=NTerm.SI(Graph) in I.eval u
let normalise u = let module I=Term.SI(NTerm) in I.eval u

let raw_of_graph g = let module I=Graph.SI(Raw) in I.eval g
let term_of_graph g = let module I=Graph.SI(Term) in I.eval g

module U = struct
let term_of_raw u = let module I=Raw.U.I(Term.U) in I.eval u
let nterm_of_raw u = let module I=Raw.U.I(NTerm.U) in I.eval u
let graph_of_raw u = let module I=Raw.U.I(Graph.U) in I.eval u
let graph_of_term u = let module I=Term.U.I(Graph.U) in I.eval u
let graph_of_nterm u = let module I=NTerm.U.I(Graph.U) in I.eval u
let normalise u = let module I=Term.U.I(NTerm.U) in I.eval u

let raw_of_graph g = let module I=Graph.U.I(Raw.U) in I.eval g
let term_of_graph g = let module I=Graph.U.I(Term.U) in I.eval g
end
