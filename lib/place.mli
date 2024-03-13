open Types
open Graph

(** position the graph with the sources on a circle,
    and inner elements at random (unless specified in the given decorations) *)
val circle_random: #drawable graph -> unit

(** center the edge according to its neighbours *)
val center_edge:( #drawable as 'a) graph -> 'a edge -> unit
