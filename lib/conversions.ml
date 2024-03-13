
let term_of_raw u = let module I=Raw.INIT(Term) in I.eval u
let nterm_of_raw u = let module I=Raw.INIT(NTerm) in I.eval u
let graph_of_raw u = let module I=Raw.INIT(Graph) in I.eval u
let graph_of_term u = let module I=Term.INIT(Graph) in I.eval u
let graph_of_nterm u = let module I=NTerm.INIT(Graph) in I.eval u
let normalise u = let module I=Term.INIT(NTerm) in I.eval u

let raw_of_graph g = let module I=Graph.INIT(Raw) in I.eval g
let term_of_graph g = let module I=Graph.INIT(Term) in I.eval g

module S = struct
let term_of_raw u = let module I=Raw.INIT(Term) in I.seval u
let nterm_of_raw u = let module I=Raw.INIT(NTerm) in I.seval u
let graph_of_raw u = let module I=Raw.INIT(Graph) in I.seval u
let graph_of_term u = let module I=Term.INIT(Graph) in I.seval u
let graph_of_nterm u = let module I=NTerm.INIT(Graph) in I.seval u
let normalise u = let module I=Term.INIT(NTerm) in I.seval u

let raw_of_graph g = let module I=Graph.INIT(Raw) in I.seval g
let term_of_graph g = let module I=Graph.INIT(Term) in I.seval g
end
