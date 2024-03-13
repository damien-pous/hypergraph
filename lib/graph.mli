open Common

(** graphs with inner elements decorated with 'a values *)
include INITIAL_ALGEBRA
type 'a graph = 'a t
type 'a sgraph = 'a st
type 'a vertex = Src of int | Inn of 'a
type 'a edge

val neighbours: 'a edge -> 'a vertex seq
val einfo: 'a edge -> 'a
val sinfo: 'a st -> int -> 'a
val vinfo: 'a st -> 'a vertex -> 'a

val iter_edges: ('a -> 'a vertex seq -> unit) -> 'a t -> unit
val iter_ivertices: ('a -> unit) -> 'a t -> unit
val iter_vertices: ('a vertex -> unit) -> 'a st -> unit
val iter_sources: (int -> 'a -> unit) -> 'a st -> unit

(* isomorphim check, using the given function to compare edge infos *)
val iso : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val siso : ('a -> 'a -> bool) -> 'a st -> 'a st -> bool

(** dynamic graphs, which can be modified *)
class ['a] dynamic: 
  object
    method set: 'a sgraph -> unit

    method sources: 'a seq
    method sgraph: 'a sgraph
    method graph: 'a graph
    method arity: int

    method sinfo: int -> 'a
    method vinfo: 'a vertex -> 'a

    method iter_sources: (int -> 'a -> unit) -> unit
    method iter_ivertices: ('a -> unit) -> unit
    method iter_vertices: ('a vertex -> unit) -> unit
    method iter_edges: ('a edge -> unit) -> unit
    method iter_infos: ('a -> unit) -> unit

    method add_edge: 'a -> 'a vertex seq -> 'a edge
    method add_ivertex: 'a -> unit
    method rem_edge: 'a edge -> unit
    method rem_ivertex: 'a -> unit
    method rem_source: int -> unit
    method rem_vertex: 'a vertex -> unit
    method promote: 'a -> unit
    method permute: perm -> unit
    method forget: int -> unit
    method lift: 'a -> unit

    method find: ('a -> bool) -> [`V of 'a vertex | `E of 'a edge | `N]
  end
