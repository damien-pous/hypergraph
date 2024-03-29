open Types

(** graphs with inner elements decorated with 'a values *)

type 'a vertex = Src of int | Inn of 'a
type 'a edge
include ISEALGEBRA' with type 'a ru = 'a Raw.u and type 'a rt = 'a Raw.t
type 'a ugraph = 'a u           (* source-decoration-free graphs *)
type 'a graph = 'a t

val ivertices: 'a graph -> 'a set
val sources: 'a graph -> 'a seq
val edges: 'a graph -> 'a edge set

val neighbours: 'a edge -> 'a vertex seq
val einfo: 'a edge -> 'a
val sinfo: 'a graph -> int -> 'a
val vinfo: 'a graph -> 'a vertex -> 'a

val iter_edges: ('a -> 'a vertex seq -> unit) -> 'a graph -> unit
val iter_edges': ('a edge -> unit) -> 'a graph -> unit
val iter_edges'': ('a edge -> 'a -> 'a vertex seq -> unit) -> 'a graph -> unit
val iter_ivertices: ('a -> unit) -> 'a graph -> unit
val iter_vertices: ('a vertex -> unit) -> 'a graph -> unit
val iter_sources: (int -> 'a -> unit) -> 'a graph -> unit
val iter_infos: ('a -> unit) -> 'a graph -> unit

val promote: 'a -> 'a graph -> 'a graph

val rem_edge: 'a edge -> 'a graph -> 'a graph
val rem_ivertex: 'a -> 'a graph -> 'a graph
val rem_source: int -> 'a graph -> 'a graph
val rem_vertex: 'a vertex -> 'a graph -> 'a graph

val add_edge: 'a -> 'a vertex seq -> 'a graph -> 'a edge * 'a graph
val add_ivertex: 'a -> 'a graph -> 'a graph

(* isomorphim check, using the given function to compare edge infos *)
val iso: ('a -> 'a -> bool) -> 'a graph -> 'a graph -> bool

val draw_on: canvas -> #positionned graph -> unit
val draw: #positionned graph -> image

val bbox: #positionned graph -> box

val find: ('a -> bool) -> 'a graph -> [`V of 'a vertex | `E of 'a edge | `N]

val get_info: 'a graph -> kind*int -> 'a




module U: sig
  (* variants of the above functions on source-decoration-free graphs *)
  include IEALGEBRA' with type 'a t = 'a ugraph and type 'a r = 'a Raw.u
  val iter_edges: ('a -> 'a vertex seq -> unit) -> 'a ugraph -> unit
  val iter_edges': ('a edge -> unit) -> 'a ugraph -> unit
  val iter_edges'': ('a edge -> 'a -> 'a vertex seq -> unit) -> 'a ugraph -> unit
  val iter_ivertices: ('a -> unit) -> 'a ugraph -> unit
  
  val rem_edge: 'a edge -> 'a ugraph -> 'a ugraph
  val rem_ivertex: 'a -> 'a ugraph -> 'a ugraph
  val rem_source: int -> 'a ugraph -> 'a ugraph
  val rem_vertex: 'a vertex -> 'a ugraph -> 'a ugraph
  
  val add_edge: 'a -> 'a vertex seq -> 'a ugraph -> 'a edge * 'a ugraph
  val add_ivertex: 'a -> 'a ugraph -> 'a ugraph    
  
  val iso: ('a -> 'a -> bool) -> 'a ugraph -> 'a ugraph -> bool
end
