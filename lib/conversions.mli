val term_of_raw: 'a Raw.t -> 'a Term.t
val nterm_of_raw: 'a Raw.t -> 'a NTerm.t
val graph_of_raw: 'a Raw.t -> 'a Graph.t
val graph_of_term: 'a Term.t -> 'a Graph.t
val graph_of_nterm: 'a NTerm.t -> 'a Graph.t
val normalise: 'a Term.t -> 'a NTerm.t

val raw_of_graph: 'a Graph.t -> 'a Raw.t
val term_of_graph: 'a Graph.t -> 'a Term.t

module U: sig
val term_of_raw: 'a Raw.u -> 'a Term.u
val nterm_of_raw: 'a Raw.u -> 'a NTerm.u
val graph_of_raw: 'a Raw.u -> 'a Graph.u
val graph_of_term: 'a Term.u -> 'a Graph.u
val graph_of_nterm: 'a NTerm.u -> 'a Graph.u
val normalise: 'a Term.u -> 'a NTerm.u

val raw_of_graph: 'a Graph.u -> 'a Raw.u
val term_of_graph: 'a Graph.u -> 'a Term.u
end
