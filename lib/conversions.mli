val pterm: 'a Term.t -> 'a PTerm.t
val nterm: 'a Term.t -> 'a NTerm.t

val plain: 'a Term.t -> 'a Term.t
val normalise: 'a Term.t -> 'a Term.t

val graph_of_term: 'a Term.t -> 'a Graph.t
val term_of_graph: 'a Graph.t -> 'a Term.t

module U: sig
val pterm: 'a Term.u -> 'a PTerm.u
val nterm: 'a Term.u -> 'a NTerm.u

val plain: 'a Term.u -> 'a Term.u
val normalise: 'a Term.u -> 'a Term.u

val graph_of_term: 'a Term.u -> 'a Graph.u
val term_of_graph: 'a Graph.u -> 'a Term.u
end
