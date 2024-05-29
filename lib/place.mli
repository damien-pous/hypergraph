open Types
open Graph

(** position the sources on a circle *)
val sources_on_circle: #positionned graph -> unit

(** position elements randomly *)
val randomly: #positionned graph -> unit

(** position the graph using graphviz (only on Unix systems) *)
val graphviz: ?cmd: string -> #positionned graph -> unit

(** center the edge according to its neighbours *)
val center_edge:(#positionned as 'a) graph -> 'a edge -> unit

(** scale the graph placement by a given factor *)
val scale: float -> #positionned graph -> unit


(** center of a graph (= barycenter of its sources) *)
val graph_center: #positionned graph -> point
