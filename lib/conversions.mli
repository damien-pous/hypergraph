val term_of_raw: 'a Raw.t -> 'a Term.t
val nterm_of_raw: 'a Raw.t -> 'a NTerm.t
val graph_of_raw: 'a Raw.t -> 'a Graph.t
val graph_of_term: 'a Term.t -> 'a Graph.t
val graph_of_nterm: 'a NTerm.t -> 'a Graph.t
val normalise: 'a Term.t -> 'a NTerm.t

val raw_of_graph: 'a Graph.t -> 'a Raw.t
val term_of_graph: 'a Graph.t -> 'a Term.t

module S: sig
val term_of_raw: 'a Raw.st -> 'a Term.st
val nterm_of_raw: 'a Raw.st -> 'a NTerm.st
val graph_of_raw: 'a Raw.st -> 'a Graph.st
val graph_of_term: 'a Term.st -> 'a Graph.st
val graph_of_nterm: 'a NTerm.st -> 'a Graph.st
val normalise: 'a Term.st -> 'a NTerm.st

val raw_of_graph: 'a Graph.st -> 'a Raw.st
val term_of_graph: 'a Graph.st -> 'a Term.st
end
