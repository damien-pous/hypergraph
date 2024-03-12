type 'a raw = 'a Raw.t
type 'a term = 'a Term.t
type 'a nterm = 'a NTerm.t
type 'a graph = 'a Graph.t

val term_of_raw: 'a raw -> 'a term
val nterm_of_raw: 'a raw -> 'a nterm
val graph_of_raw: 'a raw -> 'a graph
val graph_of_term: 'a term -> 'a graph
val graph_of_nterm: 'a nterm -> 'a graph
val normalise: 'a term -> 'a nterm

val raw_of_graph: 'a graph -> 'a raw
val term_of_graph: 'a graph -> 'a term
