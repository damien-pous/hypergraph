open Types
open Graph

(** position the sources on a circle *)
val sources_on_circle: #positionned graph -> unit

(** position elements randomly *)
val randomly: #positionned graph -> unit

(** position the graph using graphviz *)
val graphviz: #positionned graph -> unit

(** center the edge according to its neighbours *)
val center_edge:( #positionned as 'a) graph -> 'a edge -> unit
