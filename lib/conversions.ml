type 'a raw = 'a Raw.t
type 'a term = 'a Term.t
type 'a nterm = 'a NTerm.t
type 'a graph = 'a Graph.t

let term_of_raw u = let module I=Raw.INIT(Term) in I.eval u
let nterm_of_raw u = let module I=Raw.INIT(NTerm) in I.eval u
let graph_of_raw u = let module I=Raw.INIT(Graph) in I.eval u
let graph_of_term u = let module I=Term.INIT(Graph) in I.eval u
let graph_of_nterm u = let module I=NTerm.INIT(Graph) in I.eval u
let normalise u = let module I=Term.INIT(NTerm) in I.eval u

let raw_of_graph g = let module I=Graph.INIT(Raw) in Raw.fix (Graph.arity g) (I.eval g)
let term_of_graph g = let module I=Graph.INIT(Term) in I.eval g
