open Common
open Graph

(** position the graph with the sources on a circle,
    and inner elements at random (unless specified in the given decorations) *)
val circle_random: info graph -> info sgraph
