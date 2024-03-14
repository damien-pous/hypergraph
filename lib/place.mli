open Types
open Graph

(** position the sources on a circle *)
val sources_on_circle: #drawable graph -> unit

(** position elements randomly *)
val randomly: #drawable graph -> unit

(** position the graph using graphviz *)
val graphviz: #drawable graph -> unit

(** center the edge according to its neighbours *)
val center_edge:( #drawable as 'a) graph -> 'a edge -> unit
